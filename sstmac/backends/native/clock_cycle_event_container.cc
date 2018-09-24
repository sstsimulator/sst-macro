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
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>
#include <limits>
#include <cinttypes>

#define event_debug(...) \
  debug_printf(sprockit::dbg::parallel, "LP %d: %s", rt_->me(), sprockit::printf(__VA_ARGS__).c_str())

RegisterDebugSlot(event_manager_time_vote);

RegisterKeywords(
  { "num_profile_loops", "the number of loops to execute of the parallel core for profiling parallel overheads" },
  { "epoch_print_interval", "the print interval for stats on parallel execution" }
);

#define epoch_debug(...) \
  debug_printf(sprockit::dbg::event_manager_time_vote, \
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

clock_cycle_event_map::clock_cycle_event_map(
  sprockit::sim_parameters* params, parallel_runtime* rt) :
  event_manager(params, rt),
  epoch_(0)
{
  num_profile_loops_ = params->get_optional_int_param("num_profile_loops", 0);
  epoch_print_interval = params->get_optional_int_param("epoch_print_interval", epoch_print_interval); 
}

int
clock_cycle_event_map::handle_incoming(char* buf)
{
#if SSTMAC_USE_MULTITHREAD
  serializer ser;
  ser.start_unpacking(buf, 1<<31); //just pass in a huge number
  uint32_t sz; //these are guaranteed to be first
  uint32_t dst;
  ser & sz;
  ser & dst;
  if (sz == 0){
    sprockit::abort("got zero size for incoming buffer");
  }
  event_manager* mgr = interconn_->component(dst)->event_mgr();
  mgr->schedule_pending_serialization(buf);
  return sz;
#else
  return serialize_schedule(buf);
#endif
}

timestamp
clock_cycle_event_map::receive_incoming_events(timestamp vote)
{
  if (nproc_ == 1) return vote;

  timestamp min_time = no_events_left_time;
  if (!stopped_){
    event_debug("voting for minimum time %lu on epoch %d", vote.ticks(), epoch_);
    min_time = rt_->send_recv_messages(vote);

    event_debug("got back minimum time %lu", min_time.ticks());

    int num_recvs = rt_->num_recvs_done();
    for (int i=0; i < num_recvs; ++i){
      auto& buf = rt_->recv_buffer(i);
      size_t bytesRemaining = buf.totalBytes();
      char* serBuf = buf.buffer();
      while (bytesRemaining > 0){
        int size = handle_incoming(serBuf);
        bytesRemaining -= size;
        serBuf += size;
      }
    }
  }
  rt_->reset_send_recv();
  ++epoch_;
  return min_time;
}


void
clock_cycle_event_map::run()
{
  interconn_->setup();

  timestamp lower_bound;
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
    timestamp horizon = lower_bound + lookahead_;
    auto t_start = rdtsc();
    timestamp min_time = run_events(horizon);
    auto t_run = rdtsc();
    lower_bound = receive_incoming_events(min_time);
    auto t_stop = rdtsc();
    uint64_t event = t_run - t_start;
    uint64_t barrier = t_stop - t_run;
    event_cycles += event;
    barrier_cycles += barrier;
    if (epoch % epoch_print_interval == 0 && rt_->me() == 0){
      printf("Epoch %13" PRIu64 " ran %13" PRIu64 ", %13" PRIu64 " cumulative %13" PRIu64
             ", %13" PRIu64 " until horizon %13" PRIu64 "\n",
             epoch, event, barrier, event_cycles, barrier_cycles, horizon.ticks());
    }
    if (num_loops_left > 0){
      --num_loops_left;
    }
    ++epoch;
  }
  compute_final_time(now_);
  if (rt_->me() == 0) printf("Ran %" PRIu64 " epochs on MPI parallel\n", epoch);
}

void
clock_cycle_event_map::compute_final_time(timestamp vote)
{
  if (nproc_ == 1){
    final_time_ = vote;
  } else {
    uint64_t final_ticks = vote.ticks();
    final_time_ = timestamp(rt_->allreduce_max(final_ticks), timestamp::exact);
  }
}

}
}

#endif // !SSTMAC_INTEGRATED_SST_CORE
