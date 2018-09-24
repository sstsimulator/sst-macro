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

#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/event_handler.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/sst_event.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/common/ipc_event.h>
#include <sstmac/common/handler_event_queue_entry.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <unistd.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/sst_core/connectable_wrapper.h>
#endif

#define test_schedule(x) \
  if (dynamic_cast<link_wrapper*>(x)) abort()

namespace sstmac {

void
event_component::cancel_all_messages()
{
#if SSTMAC_INTEGRATED_SST_CORE
  sprockit::abort("event_scheduler::cancel_all_messages: cannot cancel messages currently in integrated core");
#else
  event_mgr()->cancel_all_messages(component_id());
#endif
}

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
  event_queue_entry* evq = new handler_event_queue_entry(ev, handler, component_id());
  self_link_->send(delay, time_converter_, evq);
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
event_link::send_extra_delay(timestamp delay, event *ev)
{
  event* to_send = handler_ ? new handler_event_queue_entry(ev, handler_, 0) : ev;
  link_->send(delay.ticks(), event_scheduler::time_converter(), to_send);
}

event_component::event_component(sprockit::sim_parameters* params,
               uint32_t cid,
               event_manager* mgr) :
 SSTIntegratedComponent(params, cid),
 event_scheduler(cid)
{
  event_scheduler::init_self_link(this);
}

#else
timestamp event_link::min_remote_latency_;
timestamp event_link::min_thread_latency_;

event_scheduler::event_scheduler(event_manager* mgr, uint32_t comp_id) :
  eventman_(mgr),
  seqnum_(0),
  id_(comp_id),
  thread_id_(mgr->thread()),
  nthread_(mgr->nthread()),
  now_(mgr->now_ptr())
{
}

void
event_scheduler::send_self_event_queue(timestamp arrival, event_queue_entry *ev)
{
  ev->set_time(arrival);
  ev->set_seqnum(seqnum_++);
  eventman_->schedule(ev);
}

void
event_scheduler::send_delayed_self_event_queue(timestamp delay, event_queue_entry* ev)
{
  ev->set_seqnum(seqnum_++);
  send_self_event_queue(delay+eventman_->now(), ev);
}

void
event_scheduler::send_now_self_event_queue(event_queue_entry* ev)
{
  send_self_event_queue(eventman_->now(), ev);
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
}

void
event_subcomponent::setup()
{
  //do nothing
}

#if SSTMAC_INTEGRATED_SST_CORE
#else
void
local_link::multi_send_extra_delay(timestamp delay, event *ev, event_scheduler *src)
{
  timestamp arrival = src->now() + delay + latency_;
  event_queue_entry* qev = new handler_event_queue_entry(ev, handler_, src->component_id());
  qev->set_seqnum(src->next_seqnum());
  qev->set_time(arrival);
  dst_->event_mgr()->schedule(qev);
}

void
ipc_link::multi_send_extra_delay(timestamp delay, event *ev, event_scheduler *src)
{
  timestamp arrival = src->now() + delay + latency_;
  src->event_mgr()->set_min_ipc_time(arrival);
  ipc_event_t iev;
  iev.src = src->component_id();
  iev.dst = dst_;
  iev.seqnum = src->next_seqnum();
  iev.ev = ev;
  iev.t = arrival;
  iev.rank = rank_;
  iev.credit = is_credit_;
  iev.port = port_;
  src->event_mgr()->ipc_schedule(&iev);
  //this guy is gone
  delete ev;
}

void
multithread_link::send_extra_delay(timestamp delay, event *ev)
{
  multi_send_extra_delay(delay, ev, scheduler_);
}

void
multithread_link::multi_send_extra_delay(timestamp delay, event* ev, event_scheduler* src)
{
  event_queue_entry* qev = new handler_event_queue_entry(ev, handler_, src->component_id());
  timestamp arrival = src->now() + delay + latency_;
  src->event_mgr()->set_min_ipc_time(arrival);
  qev->set_time(arrival);
  qev->set_seqnum(src->next_seqnum());
  if (dst_->thread() == src->thread()){
    dst_->event_mgr()->schedule(qev);
  } else {
    dst_->event_mgr()->multithread_schedule(src->event_mgr()->pending_slot(), src->thread(), qev);
  }
}
#endif

} // end of namespace sstmac
