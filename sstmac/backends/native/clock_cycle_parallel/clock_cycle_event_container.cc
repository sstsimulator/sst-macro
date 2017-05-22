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
  debug_printf(sprockit::dbg::parallel, "Rank %d: %s", rt_->me(), sprockit::printf(__VA_ARGS__).c_str())

RegisterDebugSlot(event_manager_time_vote);

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
  thread_incoming_.resize(nthread());
}

void
clock_cycle_event_map::schedule_incoming(const std::vector<void*>& buffers)
{
  sprockit::serializer ser;
  int num_bufs = buffers.size();
  int buf_size = rt_->ser_buf_size();
  for (int i=0; i < num_bufs; ++i){
    device_id dst;
    device_id src;
    uint32_t seqnum;
    timestamp time;
    event* ev;
    char* customBuffer = nullptr;
    void* buffer = buffers[i];
    ser.start_unpacking((char*)buffer, 100e9); //we don't worry about overruns anymore
    ser & dst;
    ser & src;
    ser & seqnum;
    ser & time;
    ser & ev;
    event_handler* dst_handler = nullptr;



    event_debug("epoch %d: scheduling incoming event of type %d at %12.8e to device %d, payload? %d:\n   %s",
      epoch_, dst.type(), time.sec(), dst.id(), ev->is_payload(), sprockit::to_string(ev).c_str());
    switch (dst.type()){
      case device_id::node:
        dst_handler = interconn_->node_at(dst.id())->get_nic()->payload_handler(hw::nic::LogP);
        break;
      case device_id::logp_overlay:
        dst_handler = interconn_->logp_switch_at(dst.id())->payload_handler(0);
        break;
      case device_id::router:
        if (ev->is_payload()){
          dst_handler = interconn_->switch_at(dst.id())->payload_handler(0); //port 0 for now - hack - all the same
        } else {
          dst_handler = interconn_->switch_at(dst.id())->credit_handler(0);
        }
        break;
      default:
        spkt_abort_printf("Invalid device type %d in parallel run", dst.type());
        break;
    }
    if (dst_handler->ipc_handler()){
      spkt_abort_printf("On rank %d, event going from %d:%d to %d:%d got scheduled to IPC handler",
                        me(), src.id(), src.type(), dst.id(), dst.type());
    }
    schedule(time, seqnum, new handler_event_queue_entry(ev, dst_handler, src));
  }
}

void
clock_cycle_event_map::receive_incoming_events()
{
#if SSTMAC_SANITY_CHECK
  if (thread_id() != 0){
    spkt_throw(sprockit::illformed_error,  
        "clock_cycle_event_map::schedule_incoming: only thread 0 should handle incoming MPI messages");
  }
#endif
  rt_->send_recv_messages(all_incoming_);

  if (nthread() == 1){
    schedule_incoming(all_incoming_);
  }
  else {
    int num_incoming = all_incoming_.size();
    for (int i=0; i < num_incoming; ++i){
      void* buffer = all_incoming_[i];
      int sid = *(reinterpret_cast<int*>(buffer));
      int thr = interconn_->thread_for_switch(switch_id(sid));
      thread_incoming_[thr].push_back(buffer);
    }
  }
  all_incoming_.clear();
}


int64_t
clock_cycle_event_map::do_vote(int64_t my_time, vote_type_t ty)
{
  switch (ty){
    case vote_type_t::max:
      return rt_->allreduce_max(my_time);
      break;
    case vote_type_t::min:
      return rt_->allreduce_min(my_time);
      break;
  }
}

timestamp
clock_cycle_event_map::vote_next_round(timestamp time, vote_type_t ty)
{
  int64_t vote_result = do_vote(time.ticks_int64(), ty);
  timestamp final_time(vote_result, timestamp::exact);
  event_debug("epoch %d: got time %12.8e on thread %d",
    epoch_, final_time.sec(), thread_id_);
  return final_time;
}

bool
clock_cycle_event_map::vote_to_terminate()
{
  event_debug("epoch %d: voting to terminate on thread %d", 
    epoch_, thread_id());

  receive_incoming_events();
  timestamp my_vote = (empty() || stopped_) ? no_events_left_time_ : next_event_time();
  timestamp min_time = vote_next_round(my_vote, vote_type_t::min);
  ++epoch_;
  if (min_time == no_events_left_time_){
    return true; //done
  } else {
    next_time_horizon_ = min_time + lookahead_;
    return false;
  }
}

timestamp
clock_cycle_event_map::next_event_time() const
{
  return queue_.empty() ? no_events_left_time_ : (*queue_.begin())->time();
}

#if DEBUG_DETERMINISM
std::map<device_id,std::ofstream*> outs;
#endif

void
clock_cycle_event_map::do_next_event()
{
  timestamp ev_time = next_event_time();
  while (ev_time >= next_time_horizon_){
    receive_incoming_events();

    ev_time = next_event_time();

    event_debug("epoch %d: voting NOT to terminate at time %12.8e on thread %d",
                epoch_, ev_time.sec(), thread_id_);

    timestamp min_time = vote_next_round(ev_time, vote_type_t::min);
    next_time_horizon_ = min_time + lookahead_;

    event_debug("epoch %d: next time horizon is %12.8e for lookahead %12.8e: next event at %12.8e %sready to proceed on thread %d",
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
  timestamp global_max = vote_next_round(now(), vote_type_t::max);
  set_now(global_max);
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
clock_cycle_event_map::ipc_schedule(timestamp t,
  device_id dst,
  device_id src,
  uint32_t seqnum,
  event* ev)
{
  event_debug("epoch %d: scheduling outgoing event with cls id %" PRIu32 " at t=%12.8e to location %d of type %d\n  %s",
    epoch_, ev->cls_id(), t.sec(), dst.id(), dst.type(),
    sprockit::to_string(ev).c_str());

  rt_->send_event(thread_id_, t,
    dst,
    src,
    seqnum,
    ev);
}

}
}

#endif // !SSTMAC_INTEGRATED_SST_CORE