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
#include <sstmac/backends/native/clock_cycle_parallel/clock_cycle_event_container.h>
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
  event_map(params, rt),
  epoch_(0)
{
  int64_t max_ticks = std::numeric_limits<int64_t>::max() - 100;
  no_events_left_time_ = timestamp(max_ticks, timestamp::exact);
  min_ipc_time_ = no_events_left_time_;
  thread_incoming_.resize(nthread());
}

void
clock_cycle_event_map::schedule_incoming(ipc_event_t* iev)
{
  event_handler* dst_handler = nullptr;
  switch (iev->dst.type()){
    case device_id::node:
      dst_handler = interconn_->node_at(iev->dst.id())->get_nic()->payload_handler(hw::nic::LogP);
      break;
    case device_id::logp_overlay:
      dst_handler = interconn_->logp_switch_at(iev->dst.id())->payload_handler(0);
      break;
    case device_id::router:
      if (iev->ev->is_payload()){
        dst_handler = interconn_->switch_at(iev->dst.id())->payload_handler(0); //port 0 for now - hack - all the same
      } else {
        dst_handler = interconn_->switch_at(iev->dst.id())->credit_handler(0);
      }
      break;
    default:
      spkt_abort_printf("Invalid device type %d in parallel run", iev->dst.type());
      break;
  }
  if (dst_handler->ipc_handler()){
    spkt_abort_printf("On rank %d, event going from %d:%d to %d:%d got scheduled to IPC handler",
                      me(), iev->src.id(), iev->src.type(), iev->dst.id(), iev->dst.type());
  }
  event_debug("epoch %d: scheduling incoming event of type %d at %12.8e to device %d:%s, payload? %d:   %s",
    epoch_, iev->dst.type(), iev->t.sec(), iev->dst.id(), dst_handler->to_string().c_str(),
    iev->ev->is_payload(), sprockit::to_string(iev->ev).c_str());
  schedule(iev->t, iev->seqnum, new handler_event_queue_entry(iev->ev, dst_handler, iev->src));
}

timestamp
clock_cycle_event_map::receive_incoming_events(timestamp vote)
{
#if SSTMAC_SANITY_CHECK
  if (thread_id() != 0){
    sprockit::abort("clock_cycle_event_map::schedule_incoming: only thread 0 should handle incoming MPI messages");
  }
#endif
  //vote for the minimum time of all my events
  //and all the events I sent out to others
  timestamp my_vote = vote;
  if (vote > min_ipc_time_){
    vote = min_ipc_time_;
  }
  timestamp min_time = rt_->send_recv_messages(vote);

  current_vote_result_ = min_time;

  int nthr = nthread();
  int num_recvs = rt_->num_recvs_done();
  for (int i=0; i < num_recvs; ++i){
    auto& buf = rt_->recv_buffer(i);
    serializer ser;
    ser.start_unpacking(buf.buffer(), buf.bytesUsed());
    while (ser.size() < buf.bytesUsed()){
      event_debug("unpacking event starting at offset %lu of %lu",
                  ser.size(), buf.bytesUsed());
      ipc_event_t iev;
      parallel_runtime::run_serialize(ser, &iev);
      if (nthr == 1){
        event_debug("unpacked event %s", sprockit::to_string(iev.ev).c_str());
        schedule_incoming(&iev);
      } else {
        spkt_abort_printf("multithread not compatible with MPI currently");
      }
    }
  }
  rt_->reset_send_recv();
  //reset this guy
  min_ipc_time_ = no_events_left_time_;
  last_vote_result_ = min_time;
  return min_time;
}

bool
clock_cycle_event_map::vote_to_terminate()
{
  event_debug("epoch %d: voting to terminate on thread %d", 
    epoch_, thread_id());

  timestamp min_time = receive_incoming_events(no_events_left_time_);

  ++epoch_;
  if (min_time == no_events_left_time_){
    return true; //done
  } else {
    next_time_horizon_ = min_time + lookahead_;
    epoch_debug("epoch %d: next time horizon now %12.8e for lookahead %12.8e",
        epoch_, next_time_horizon_.sec(), lookahead_.sec());
    return false;
  }
}

void
clock_cycle_event_map::do_next_event()
{
  timestamp ev_time = next_event_time();
  while (ev_time >= next_time_horizon_){
    event_debug("epoch %d: voting NOT to terminate at time %12.8e on thread %d",
                epoch_, ev_time.sec(), thread_id_);
    ev_time = next_event_time();
    timestamp min_time = receive_incoming_events(ev_time);
    next_time_horizon_ = min_time + lookahead_;
    epoch_debug("epoch %d: next time horizon is %12.8e for lookahead %12.8e: next event at %12.8e %sready to proceed on thread %d",
        epoch_, next_time_horizon_.sec(), lookahead_.sec(), ev_time.sec(),
        ((ev_time > next_time_horizon_) ? "not " : ""),
        thread_id());


    ++epoch_;
  }

  event_queue_entry* ev = pop_next_event();

#if DEBUG_DETERMINISM
  std::ofstream*& f = outs[ev->event_location()];
  if (f == nullptr){
    char fname[64];
    sprintf(fname, "events.%d.out", int(ev->event_location().location));
    f = new std::ofstream(fname);
  }

  *f << sprockit::printf("%ld: %d: %d<-%d", 
    ev->time().ticks_int64(), ev->seqnum(),
    int(ev->event_location().location), int(ev->src_location().location));
  *f << " " << ev->to_string() << std::endl;
#endif

  set_now(ev->time());
#if SSTMAC_DEBUG_THREAD_EVENTS
  if (sprockit::debug::slot_active(sprockit::dbg::thread_events)){
    event_file_ << sprockit::printf("T=%10.5fms: %s\n",
                    ev->time().msec(), ev->to_string().c_str());
  }
#endif
  ev->execute();
  delete ev;
}

void
clock_cycle_event_map::run()
{
  event_map::run();
  uint64_t now_ticks = now().ticks_int64();
  uint64_t final = rt_->allreduce_max(now_ticks);
  set_now(timestamp(final, timestamp::exact));
}

#if SSTMAC_DEBUG_THREAD_EVENTS
void
clock_cycle_event_map::open_debug_file()
{
  if (sprockit::debug::slot_active(sprockit::dbg::thread_events)){
    std::string fname = sprockit::printf("events.thread%d", thread_id());
    event_file_.open(fname.c_str());
  }
}

void
clock_cycle_event_map::close_debug_file()
{
  if (sprockit::debug::slot_active(sprockit::dbg::thread_events)){
    event_file_.close();
  }
}
#endif

void
clock_cycle_event_map::set_interconnect(hw::interconnect* interconn)
{
  event_map::set_interconnect(interconn);
  interconn_ = interconn;
  int nworkers = rt_->nproc() * rt_->nthread();
  if (nworkers == 1){
    //dont need the interconnect
    lookahead_ = timestamp(1e5);
  }
  else {
    lookahead_ = interconn_->lookahead();
  }
  next_time_horizon_ = lookahead_;
}

void
clock_cycle_event_map::ipc_schedule(ipc_event_t* iev)
{
  event_debug("epoch %d: scheduling outgoing event with cls id %" PRIu32 " at t=%12.8e to location %d of type %d\n  %s",
    epoch_, iev->ev->cls_id(), iev->t.sec(), iev->dst.id(), iev->dst.type(),
    sprockit::to_string(iev->ev).c_str());

  if (iev->t < min_ipc_time_){
    min_ipc_time_ = iev->t;
  }
  rt_->send_event(thread_id_, iev);
}

}
}

#endif // !SSTMAC_INTEGRATED_SST_CORE
