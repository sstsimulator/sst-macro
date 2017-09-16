/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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
#if !SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/backends/native/clock_cycle_event_container.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sprockit/util.h>
#include <limits>
#include <cinttypes>

#define event_debug(...) \
  debug_printf(sprockit::dbg::parallel, "LP %d: %s", rt_->me(), sprockit::printf(__VA_ARGS__).c_str())

RegisterDebugSlot(event_manager_time_vote);

#define epoch_debug(...) \
  debug_printf(sprockit::dbg::event_manager_time_vote, \
    "LP %d: %s", rt_->me(), sprockit::printf(__VA_ARGS__).c_str()); fflush(stdout)

#if SSTMAC_DEBUG_THREAD_EVENTS
DeclareDebugSlot(thread_events)
RegisterDebugSlot(thread_events)
#endif

namespace sstmac {
namespace native {

clock_cycle_event_map::clock_cycle_event_map(
  sprockit::sim_parameters* params, parallel_runtime* rt) :
  event_manager(params, rt),
  epoch_(0)
{
}

void
clock_cycle_event_map::schedule_incoming(ipc_event_t* iev)
{
  auto comp = interconn_->component(iev->dst);
  event_handler* dst_handler = iev->credit ? comp->credit_handler(iev->port) : comp->payload_handler(iev->port);
  event_debug("epoch %d: scheduling incoming event at %12.8e to device %d:%s, payload? %d:   %s",
    epoch_, iev->t.sec(), iev->dst, dst_handler->to_string().c_str(),
    iev->ev->is_payload(), sprockit::to_string(iev->ev).c_str());
  auto qev = new handler_event_queue_entry(iev->ev, dst_handler, iev->src);
  qev->set_seqnum(iev->seqnum);
  qev->set_time(iev->t);
  schedule(qev);
}

timestamp
clock_cycle_event_map::receive_incoming_events(timestamp vote)
{
  if (nproc_ == 1) return vote;

#if SSTMAC_SANITY_CHECK
  if (thread_id() != 0){
    sprockit::abort("clock_cycle_event_map::schedule_incoming: only thread 0 should handle incoming MPI messages");
  }
#endif
  timestamp min_time = rt_->send_recv_messages(vote);

  int nthr = nthread();
  int num_recvs = rt_->num_recvs_done();
  for (int i=0; i < num_recvs; ++i){
    auto& buf = rt_->recv_buffer(i);
    serializer ser;
    size_t bytesRemaining = buf.bytesUsed();
    size_t bytesUnpacked = 0;
    char* serBuf = buf.buffer();
    ser.start_unpacking(serBuf, bytesRemaining);
    while (bytesRemaining > 0){
      event_debug("unpacking event starting at offset %lu of %lu",
                  bytesUnpacked, buf.bytesUsed());
      ipc_event_t iev;
      parallel_runtime::run_serialize(ser, &iev);
      if (nthr == 1){
        event_debug("unpacked event %s", sprockit::to_string(iev.ev).c_str());
        schedule_incoming(&iev);
      } else {
        spkt_abort_printf("multithread not compatible with MPI currently");
      }
      size_t size = ser.unpacker().size();
      align64(size);
      bytesRemaining -= size;
      serBuf += size;
      ser.start_unpacking(serBuf, bytesRemaining);
      bytesUnpacked += size;
    }
  }
  rt_->reset_send_recv();
  ++epoch_;
  return min_time;
}


void
clock_cycle_event_map::run()
{
  timestamp lower_bound;
  while (lower_bound != no_events_left_time){
    timestamp horizon = lower_bound + lookahead_;
    timestamp min_time = run_events(horizon);
    lower_bound = receive_incoming_events(min_time);
  }

  timestamp max_time;
  uint64_t final_ticks = now_.ticks();
  final_time_ = timestamp(rt_->allreduce_max(final_ticks), timestamp::exact);
}

}
}

#endif // !SSTMAC_INTEGRATED_SST_CORE
