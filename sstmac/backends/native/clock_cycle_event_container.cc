/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#define __STDC_FORMAT_MACROS
#include <inttypes.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/handler_event_queue_entry.h>
#if !SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/backends/native/clock_cycle_event_container.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>
#include <limits>
#include <cinttypes>

#define event_debug(...) \
  debug_printf(sprockit::dbg::parallel, "manager %d:%d %s", \
    rt_->me(), thread_id_, sprockit::printf(__VA_ARGS__).c_str())

RegisterDebugSlot(EventManager_time_vote);

RegisterKeywords(
  { "num_profile_loops", "the number of loops to execute of the parallel core for profiling parallel overheads" },
  { "epoch_print_interval", "the print interval for stats on parallel execution" }
);

#define epoch_debug(...) \
  debug_printf(sprockit::dbg::EventManager_time_vote, \
    "LP %d: %s", rt_->me(), sprockit::printf(__VA_ARGS__).c_str()); fflush(stdout)

#if SSTMAC_DEBUG_THREAD_EVENTS
DeclareDebugSlot(thread_events)
RegisterDebugSlot(thread_events)
#endif

static int epoch_print_interval = 10000;
static uint64_t event_cycles = 0;
static uint64_t barrier_cycles = 0;

namespace sstmac {
namespace native {

ClockCycleEventMap::ClockCycleEventMap(
  SST::Params& params, ParallelRuntime* rt) :
  EventManager(params, rt)
{
  num_profile_loops_ = params.find<int>("num_profile_loops", 0);
  epoch_print_interval = params.find<int>("epoch_print_interval", epoch_print_interval);
}

int
ClockCycleEventMap::handleIncoming(char* buf)
{
#if SSTMAC_USE_MULTITHREAD
  serializer ser;
  ser.start_unpacking(buf, 1<<31); //just pass in a huge number
  uint32_t sz; //these are guaranteed to be first
  uint32_t thread;
  ser & sz;
  ser & thread;
  if (sz == 0){
    sprockit::abort("got zero size for incoming buffer");
  }
  EventManager* mgr = threadManager(thread);
  mgr->schedulePendingSerialization(buf);
  return sz;
#else
  return serializeSchedule(buf);
#endif
}

Timestamp
ClockCycleEventMap::receiveIncomingEvents(Timestamp vote)
{
  if (nproc_ == 1) return vote;

  Timestamp min_time = no_events_left_time;
  if (!stopped_){
    event_debug("voting for minimum time %10.6e on epoch %d", vote.sec(), epoch());
    min_time = rt_->sendRecvMessages(vote);

    event_debug("got back minimum time %10.6e", min_time.sec());

    int num_recvs = rt_->numRecvsDone();
    for (int i=0; i < num_recvs; ++i){
      auto& buf = rt_->recvBuffer(i);
      size_t bytesRemaining = buf.totalBytes();
      char* serBuf = buf.buffer();
      while (bytesRemaining > 0){
        int size = handleIncoming(serBuf);
        bytesRemaining -= size;
        serBuf += size;
      }
    }
  }
  rt_->resetSendRecv();
  return min_time;
}


void
ClockCycleEventMap::run()
{
  interconn_->setup();

  Timestamp lower_bound;
  /** If we want to just execute the synchronization without any actual events
   *  we can force a certain number of loops to execue for timing purposes
   *  This allows measuring only the overhead of parallel execution
   */
  int num_loops_left = num_profile_loops_;
  if (num_loops_left && rt_->me() == 0){
    printf("Running %d profile loops\n", num_loops_left);
    fflush(stdout);
  }
  if (lookahead_.ticks() == 0){
    sprockit::abort("Zero-latency link - no lookahaed, cannot run in parallel");
  }
  if (rt_->me() == 0){
    printf("Running parallel simulation with lookahead %10.6fus\n", lookahead_.usec());
  }
  uint64_t epoch = 0;

  while (lower_bound != no_events_left_time || num_loops_left > 0){
    Timestamp horizon = lower_bound + lookahead_;
    event_debug("running from %10.6e->%10.6e for lookahead %10.6e",
                lower_bound.sec(), horizon.sec(), lookahead_.sec());
    auto t_start = rdtsc();
    Timestamp min_time = runEvents(horizon);
    auto t_run = rdtsc();
    lower_bound = receiveIncomingEvents(min_time);
    auto t_stop = rdtsc();
    uint64_t event = t_run - t_start;
    uint64_t barrier = t_stop - t_run;
    event_cycles += event;
    barrier_cycles += barrier;
    if (epoch % epoch_print_interval == 0 && rt_->me() == 0){
      printf("Epoch %13" PRIu64 " ran %13" PRIu64 ", %13" PRIu64 " cumulative %13" PRIu64
             ", %13" PRIu64 " until horizon %13" PRIu64 "\n",
             epoch, event, barrier, event_cycles, barrier_cycles, horizon.time.ticks());
    }
    if (num_loops_left > 0){
      --num_loops_left;
    }
    ++epoch;
  }
  computeFinalTime(now_);
  if (rt_->me() == 0) printf("Ran %" PRIu64 " epochs on MPI parallel\n", epoch);
}

void
ClockCycleEventMap::computeFinalTime(Timestamp vote)
{
  if (nproc_ == 1){
    final_time_ = vote;
  } else {
    uint64_t final_epochs = vote.epochs;
    rt_->allreduceMax(final_epochs);
    uint64_t final_ticks = vote.time.ticks();
    if (final_epochs != vote.epochs){
      //don't vote
      final_ticks = 0;
    }
    rt_->allreduceMax(final_ticks);
    final_time_ = Timestamp(final_epochs, final_ticks);
  }
}

}
}

#endif // !SSTMAC_INTEGRATED_SST_CORE
