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

#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/event_handler.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/sst_event.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/common/ipc_event.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/sst_core/connectable_wrapper.h>
#endif

#define test_schedule(x) \
  if (dynamic_cast<link_wrapper*>(x)) abort()

namespace sstmac {

const timestamp event_scheduler::no_events_left_time(std::numeric_limits<int64_t>::max() - 100, timestamp::exact);

void
event_component::cancel_all_messages()
{
#if SSTMAC_INTEGRATED_SST_CORE
  sprockit::abort("event_scheduler::cancel_all_messages: cannot cancel messages currently in integrated core");
#else
  event_mgr()->cancel_all_messages(component_id());
#endif
}

#define sending(x) //printf("Sending %u:%s at %s:%d\n", x->cls_id(), sprockit::to_string(ev).c_str(), __FILE__, __LINE__)

#if SSTMAC_INTEGRATED_SST_CORE
SST::TimeConverter* event_scheduler::time_converter_ = nullptr;

SST::SimTime_t
event_scheduler::extra_delay(timestamp t) const
{
  SST::SimTime_t current = comp_->getCurrentSimTime(time_converter_);
  SST::SimTime_t timestamp_time = t.ticks_int64();
  return timestamp_time - current;
}

timestamp
event_scheduler::now() const
{
  SST::SimTime_t nowTicks = comp_->getCurrentSimTime(time_converter_);
  return timestamp(nowTicks, timestamp::exact);
}

void
event_scheduler::schedule_now(event_handler* handler, event* ev)
{
  //this better be a zero latency link
  schedule(SST::SimTime_t(0), handler, ev);
}

void
event_scheduler::schedule_now(event_queue_entry* ev)
{
  self_link_->send(ev);
}

void
event_scheduler::send_self_event_queue(timestamp arrival, event_queue_entry* ev)
{
  SST::SimTime_t delay = extra_delay(arrival);
  self_link_->send(delay, time_converter_, ev);
}

void
event_scheduler::send_delayed_self_event_queue(timestamp delay, event_queue_entry* ev)
{
  SST::SimTime_t sst_delay = delay.ticks_int64();
  self_link_->send(sst_delay, time_converter_, ev);
}

void
event_scheduler::send_now_self_event_queue(event_queue_entry* ev)
{
  self_link_->send(ev);
}

void
event_scheduler::schedule(SST::SimTime_t delay, event_handler* handler, event* ev)
{
  event_queue_entry* evq = new handler_event_queue_entry(ev, handler, event_location());
  if (handler->link()){
    spkt_abort_printf("should never schedule directly to link! use send_to_link");
  }
  self_link_->send(delay, time_converter_, evq);
}

void
event_scheduler::send_to_link(event_handler* handler, event *ev)
{
  SST::Link* link = handler->link();
  if (link){
    //we ignore the latency here
    sending(ev);
    link->send(0, time_converter_, ev);
  } else {
    //oh - there is no link, you lied to me
    self_link_->send(ev);
  }
}

void
event_scheduler::init_self_link(SST::Component* comp)
{
  if (!time_converter_){
    time_converter_ = comp->getTimeConverter(timestamp::tick_interval_string());
  }
  self_link_ = comp->configureSelfLink("self", time_converter_,
    new_link_handler(this, &event_scheduler::handle_self_event));
  comp_ = comp;
}

void
event_scheduler::handle_self_event(SST::Event* ev)
{
#if SSTMAC_SANITY_CHECK
  sstmac::event_queue_entry* entry = dynamic_cast<sstmac::event_queue_entry*>(ev);
  if (!entry){
    spkt_throw_printf(sprockit::value_error,
      "event on self link did not cast to an event entry");
  }
#else
  sstmac::event_queue_entry* entry = static_cast<sstmac::event_queue_entry*>(ev);
#endif
  entry->execute();
  delete entry;
}

void
event_scheduler::send_to_link(timestamp enter, timestamp lat,
                              event_handler* handler, event *ev)
{
  SST::Link* link = handler->link();
  if (link){
    //we ignore the latency here
    sending(ev);
    link->send(extra_delay(enter), time_converter_, ev);
  } else {
    //oh - there is no link, you lied to me
    schedule(enter + lat, handler, ev);
  }
}

void
event_scheduler::send_delayed_to_link(timestamp extra_delay, timestamp lat,
                              event_handler* handler, event *ev)
{
  SST::Link* link = handler->link();
  if (link){
    //we ignore the latency here
    sending(ev);
    link->send(SST::SimTime_t(extra_delay.ticks_int64()), time_converter_, ev);
  } else {
    //oh - there is no link, you lied to me
    schedule_delay(extra_delay + lat, handler, ev);
  }
}

void
event_scheduler::send_delayed_to_link(timestamp extra_delay,
                              event_handler* handler, event *ev)
{
  SST::Link* link = handler->link();
  if (link){
    //we ignore the latency here
    sending(ev);
    link->send(SST::SimTime_t(extra_delay.ticks_int64()), time_converter_, ev);
  } else {
    //oh - there is no link, you lied to me
    schedule_delay(extra_delay, handler, ev);
  }
}

void
event_scheduler::schedule(timestamp t,
                          event_handler* handler,
                          event* ev)
{
  schedule(extra_delay(t), handler, ev);
}

void
event_scheduler::schedule(timestamp t, event_queue_entry *ev)
{
  send_self_event_queue(t, ev);
}

void
event_scheduler::schedule_delay(
  timestamp delay,
  event_handler* handler,
  event* ev)
{
  schedule(SST::SimTime_t(delay.ticks_int64()), handler, ev);
}

void
event_scheduler::schedule_delay(timestamp delay, event_queue_entry *ev)
{
  self_link_->send(SST::SimTime_t(delay.ticks_int64()), time_converter_, ev);
}

event_component::event_component(sprockit::sim_parameters* params,
               uint64_t cid,
               event_manager* mgr) :
 SSTIntegratedComponent(params, cid),
 event_scheduler(id)
{
  event_scheduler::init_self_link(this);
}


event_subcomponent::event_subcomponent(event_scheduler* parent) :
 event_scheduler(parent->event_location())
{
  event_scheduler::init_self_link(parent->comp(), parent->self_link());
}
#else
timestamp
event_scheduler::run_events(timestamp event_horizon)
{
  active_ = true;
  while (!event_queue_.empty()){
    auto iter = event_queue_.begin();
    event_queue_entry* ev = *iter;
    if (ev->time() >= event_horizon){
      active_ = false;
      return ev->time();
    } else {
      now_ = ev->time();
      ev->execute();
      delete ev;
      event_queue_.erase(iter);
    }
  }
  active_ = false;
  return timestamp();
}

void
event_scheduler::send_self_event_queue(timestamp arrival, event_queue_entry *ev)
{
  ev->set_seqnum(seqnum_++);
  schedule(arrival, ev);
}

void
event_scheduler::send_delayed_self_event_queue(timestamp delay, event_queue_entry* ev)
{
  ev->set_seqnum(seqnum_++);
  schedule(delay + now_, ev);
}

void
event_scheduler::send_now_self_event_queue(event_queue_entry* ev)
{
  ev->set_seqnum(seqnum_++);
  schedule(now_, ev);
}

void
event_scheduler::schedule(timestamp t, event_queue_entry* ev)
{
  if (ev->seqnum() == -1){
    spkt_abort_printf("seqnum unitialized for event");
  }
  if (t < now_){
    spkt_abort_printf("scheduling event in the past on %p: %lu < %lu",
                      this, t.ticks(), now_.ticks());
  }
  ev->set_time(t);
  event_queue_.insert(ev);
  if (!pending_ && !active_ && t < last_registration_){
    //std::cout << "Pending component " << this << std::endl;
    eventman_->register_component(this);
    pending_ = true;
  } else if (!pending_ && !active_){
    //std::cout << "Skipping pending of " << this << std::endl;
  }
}

void
event_scheduler::register_stat(stat_collector *coll, stat_descr_t* descr)
{
  eventman_->register_stat(coll, descr);
}
#endif

void
event_component::init(unsigned int phase)
{
#if SSTMAC_INTEGRATED_SST_CORE
  SSTIntegratedComponent::init(phase);
#endif
}

void
event_component::setup()
{
#if SSTMAC_INTEGRATED_SST_CORE
  SSTIntegratedComponent::setup();
#endif
}

void
event_subcomponent::init(unsigned int phase)
{
#if SSTMAC_INTEGRATED_SST_CORE
  event_scheduler* parent = safe_cast(event_component, comp());
#endif
}

void
event_subcomponent::setup()
{
  //do nothing
}

void
local_link::send(timestamp arrival, event *ev)
{
  multi_send(arrival, ev, scheduler_);
}

void
local_link::multi_send(timestamp arrival, event *ev, event_scheduler *src)
{
  src->set_min_ipc_time(arrival);
  event_queue_entry* qev = new handler_event_queue_entry(ev, handler_, src->component_id());
  qev->set_seqnum(src->next_seqnum());
  dst_->schedule(arrival, qev);
}

void
ipc_link::multi_send(timestamp arrival, event *ev, event_scheduler *src)
{
  src->set_min_ipc_time(arrival);
  ipc_event_t iev;
  iev.src = src->component_id();
  iev.dst = dst_;
  iev.seqnum = src->next_seqnum();
  iev.ev = ev;
  iev.t = arrival;
  iev.rank = rank_;
  iev.credit = is_credit_;
  iev.port = port_;
  mgr_->ipc_schedule(&iev);
}

void
multithread_link::send(timestamp arrival, event *ev)
{
  multi_send(arrival, ev, scheduler_);
}

void
multithread_link::multi_send(timestamp arrival, event* ev, event_scheduler* src)
{
  event_queue_entry* qev = new handler_event_queue_entry(ev, handler_, scheduler_->component_id());
  qev->set_time(arrival);
  qev->set_seqnum(scheduler_->next_seqnum());
  mgr_->multithread_schedule(src->thread(), qev, dst_);
}


} // end of namespace sstmac
