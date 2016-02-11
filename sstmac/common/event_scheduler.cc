/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/event_handler.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/sst_event.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/common/messages/library_message.h>
#include <sstmac/common/messages/callback_message.h>
#include <sstmac/hardware/node/node.h>
#include <sprockit/sim_parameters.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/sst_core/connectable_wrapper.h>
#endif


#define test_schedule(x) \
  if (dynamic_cast<integrated_connectable_wrapper*>(x)) abort()

namespace sstmac {

#if SSTMAC_INTEGRATED_SST_CORE
event_scheduler* event_scheduler::global = 0;
#else
event_scheduler::event_scheduler() :
#ifdef INTEGRATED_SST_CORE_CHECK
 correctly_scheduled_(false)
#else
 seqnum_(0)
#endif
{
}
#endif

void
event_scheduler::handle(const sst_message::ptr &msg)
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "event scheduler %s should never handle messages",
    to_string().c_str());
}

void
event_scheduler::cancel_all_messages()
{
#if SSTMAC_INTEGRATED_SST_CORE
  spkt_throw(sprockit::unimplemented_error,
    "event_scheduler::cancel_all_messages: cannot cancel messages currently in integrated core");
#else
  eventman_->cancel_all_messages(event_location());
#endif
}

#if SSTMAC_INTEGRATED_SST_CORE

// TODO fill in event_scheduler using SelfLink and stuff @integrated_core @critical

timestamp
event_scheduler::now() const
{
  SST::Time_t nowTicks = getCurrentSimTime(time_converter_);
  return timestamp(nowTicks, timestamp::exact);
}

void
event_scheduler::init(unsigned int phase)
{
  SSTIntegratedComponent::init(phase);
}

void
event_scheduler::schedule_now(event_handler *handler, const sst_message::ptr &msg)
{
  //this better be a zero latency link
  schedule(SST::Time_t(0), handler, msg);
}

void
event_scheduler::schedule_now(event *ev)
{
  self_link_->send(new SSTEventEvent(ev));
}

void
event_scheduler::send_self_message(timestamp arrival, const sst_message::ptr &msg)
{
  SST::Time_t delay = extra_delay(arrival);
  self_link_->send(delay, time_converter_, new SSTHandlerEvent(this, msg));
}

void
event_scheduler::send_delayed_self_message(timestamp delay, const sst_message::ptr &msg)
{
  SST::Time_t sst_delay = delay.ticks_int64();
  self_link_->send(sst_delay, time_converter_, new SSTHandlerEvent(this, msg));
}

void
event_scheduler::send_now_self_message(const sst_message::ptr &msg)
{
  self_link_->send(new SSTHandlerEvent(this, msg));
}

void
event_scheduler::send_self_event(timestamp arrival, event *ev)
{
  SST::Time_t delay = extra_delay(arrival);
  self_link_->send(delay, time_converter_, new SSTEventEvent(ev));
}

void
event_scheduler::send_delayed_self_event(timestamp delay, event* ev)
{
  SST::Time_t sst_delay = delay.ticks_int64();
  self_link_->send(sst_delay, time_converter_, new SSTEventEvent(ev));
}

void
event_scheduler::send_now_self_event(event* ev)
{
  self_link_->send(new SSTEventEvent(ev));
}

void
event_scheduler::schedule(SST::Time_t delay, event_handler* handler, const sst_message::ptr& msg)
{
  fflush(stdout);
  switch(handler->type()){
    case event_handler::self_handler:
    {
      self_link_->send(delay, time_converter_, new SSTHandlerEvent(handler, msg));
      break;
    }
    case event_handler::link_handler:
    {
      integrated_connectable_wrapper* wrapper = static_cast<integrated_connectable_wrapper*>(handler);
      wrapper->link()->send(delay, time_converter_, new SSTMessageEvent(msg));
      break;
    }
  }
}

void
event_scheduler::schedule(timestamp t,
                          event_handler* handler,
                          const sst_message::ptr& msg)
{
  schedule(extra_delay(t), handler, msg);
}

void
event_scheduler::schedule_delay(
  timestamp delay,
  event_handler* handler,
  const sst_message::ptr& msg)
{
  schedule(SST::Time_t(delay.ticks_int64()), handler, msg);
}


void
event_subscheduler::handle_event(SST::Event* ev)
{ 
  SSTMessageEvent* mev = static_cast<SSTMessageEvent*>(ev);
  handle(mev->message());
}
#else
void
event_scheduler::set_event_manager(event_manager* mgr)
{
  eventman_ = mgr;
  init_thread_id(mgr->thread_id());
}

void
event_scheduler::schedule_now(event_handler *handler, const sst_message::ptr &msg)
{
  LINK_SCHEDULE_CHECK
  event* ev = new handler_event(msg, handler, event_location());
  schedule(now(), ev);
}

void
event_scheduler::send_self_message(timestamp arrival, const sst_message::ptr &msg)
{
  SCHEDULE(arrival, this, msg);
}

void
event_scheduler::send_delayed_self_message(timestamp delay, const sst_message::ptr &msg)
{
  SCHEDULE_DELAY(delay, this, msg);
}

void
event_scheduler::send_now_self_message(const sst_message::ptr &msg)
{
  SCHEDULE_NOW(this, msg);
}

void
event_scheduler::send_self_event(timestamp arrival, event *ev)
{
  SCHEDULE(arrival, ev);
}

void
event_scheduler::send_delayed_self_event(timestamp delay, event* ev)
{
  SCHEDULE_DELAY(delay, ev);
}

void
event_scheduler::send_now_self_event(event* ev)
{
  SCHEDULE_NOW(ev);
}

void
event_scheduler::schedule_delay(
  timestamp delay,
  event_handler* handler,
  const sst_message::ptr& msg)
{
  LINK_SCHEDULE_CHECK
  schedule(now() + delay, handler, msg);
}

void
event_scheduler::schedule_now(event* ev)
{
  LINK_SCHEDULE_CHECK
  schedule(now(), ev);
}

void
event_scheduler::schedule_delay(timestamp delay, event* ev)
{
  LINK_SCHEDULE_CHECK
  schedule(now() + delay, ev);
}

void
event_scheduler::schedule(timestamp time,
                          event_handler* handler)
{
  LINK_SCHEDULE_CHECK
  schedule(time, new null_msg_event(handler, event_location()));
}

void
event_scheduler::schedule(timestamp t, event* ev)
{
  LINK_SCHEDULE_CHECK
#if SSTMAC_SANITY_CHECK
  double delta_t = (t - now()).sec();
  if (delta_t < -1e-9) {
    spkt_throw_printf(sprockit::illformed_error,
                     "time has gone backwards %8.4e seconds", delta_t);
  }
#endif
  eventman_->schedule(t, seqnum_++, ev);
}

void
event_scheduler::register_stat(stat_collector *coll)
{
  eventman_->register_stat(coll);
}

void
event_scheduler::schedule(timestamp t,
                          event_handler* handler,
                          const sst_message::ptr& msg)
{
  LINK_SCHEDULE_CHECK
#if SSTMAC_SANITY_CHECK
  if (eventman_ == 0) {
    spkt_throw_printf(sprockit::null_error,
                     "event_scheduler::schedule: null event manager");
  }
  double delta_t = (t - now()).sec();
  if (delta_t < -1e-9) {
    spkt_throw_printf(sprockit::illformed_error,
      "time has gone backwards %8.4e seconds",
      delta_t);
  }
  if (!handler) {
    spkt_throw_printf(sprockit::null_error,
       "event_scheduler::schedule: null handler passed in");
  }
#endif
  if (handler->ipc_handler()){
    eventman_->ipc_schedule(t, handler->event_location(), event_location(), seqnum_++, msg);
  }
#if SSTMAC_USE_MULTITHREAD
  else {
    event* ev = new handler_event(msg, handler, event_location());
    int dstthread = handler->thread_id();
    int srcthread = this->thread_id();
    debug_printf(sprockit::dbg::event_manager,
        "On %s, scheduling event at t=%12.8e srcthread=%d dstthread=%d",
        to_string().c_str(), t.sec(), srcthread, dstthread);
    if (dstthread != event_handler::null_threadid
       && dstthread != srcthread){
      ev->set_time(t);
      eventman_->multithread_schedule(
        this->thread_id(),
        handler->thread_id(),
        seqnum_++,
        ev);
    } else {
      eventman_->schedule(t, seqnum_++, ev);
    }
  }
#else
  else {
    event* ev = new handler_event(msg, handler, event_location());
    eventman_->schedule(t, seqnum_++, ev);
  }
#endif
}
#endif

void
event_subscheduler::handle(const sst_message::ptr &msg)
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "event scheduler %s should never handle messages",
    to_string().c_str());
}

void
event_subscheduler::schedule(timestamp t,
                          event_handler* handler,
                          const sst_message::ptr& msg)
{
  parent_->schedule(t, handler, msg);
}

void
event_subscheduler::schedule_delay(timestamp t, event *ev)
{
  parent_->schedule_delay(t, ev);
}

void
event_subscheduler::schedule(timestamp t, event *ev)
{
  parent_->schedule(t, ev);
}

void
event_subscheduler::schedule_now(event_handler *handler, const sst_message::ptr &msg)
{
  parent_->schedule_now(handler, msg);
}

void
event_subscheduler::send_self_message(timestamp arrival, const sst_message::ptr &msg)
{
  parent_->schedule(arrival, this, msg);
}

void
event_subscheduler::send_delayed_self_message(timestamp delay, const sst_message::ptr &msg)
{
  parent_->schedule_delay(delay, this, msg);
}

void
event_subscheduler::send_now_self_message(const sst_message::ptr &msg)
{
  parent_->schedule_now(this, msg);
}

void
event_subscheduler::send_self_event(timestamp arrival, event *ev)
{
  parent_->send_self_event(arrival, ev);
}

void
event_subscheduler::send_delayed_self_event(timestamp delay, event* ev)
{
  parent_->send_delayed_self_event(delay, ev);
}

void
event_subscheduler::send_now_self_event(event* ev)
{
  parent_->send_now_self_event(ev);
}

void
event_subscheduler::schedule_delay(
  timestamp delay,
  event_handler* handler,
  const sst_message::ptr& msg)
{
  parent_->schedule_delay(delay, handler, msg);
}

void
event_subscheduler::schedule_now(event* ev)
{
  parent_->send_now_self_event(ev);
}


} // end of namespace sstmac

