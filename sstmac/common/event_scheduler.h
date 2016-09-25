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

#ifndef SSTMAC_COMMON_event_scheduler_H_INCLUDED
#define SSTMAC_COMMON_event_scheduler_H_INCLUDED

#include <sstmac/common/timestamp.h>
#include <sstmac/common/stats/location_trace.h>
#include <sstmac/common/event_handler.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/event_manager_fwd.h>
#include <sprockit/sim_parameters_fwd.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/params.h>
#include <sst/core/link.h>
#include <sstmac/sst_core/integrated_component.h>
#define ImplementSSTComponent(str, parent, comp, docstring)

namespace sstmac {
typedef SST::Event::HandlerBase link_handler;
}
#else
#define ImplementSSTComponent(str, parent, comp, docstring) \
  SpktRegister(str, parent, comp, docstring)

namespace sstmac {
typedef event_handler link_handler;

template <class T, class Fxn>
event_handler*
new_link_handler(T* t, Fxn f){
  return new_handler(t,f);
}

}
#include <sstmac/common/event_manager.h>
#endif

namespace sstmac {

/**
 * The interface for something that can schedule messages
 */
class event_scheduler :
  public locatable
#if SSTMAC_INTEGRATED_SST_CORE
  , public SSTIntegratedComponent
#endif
{

 public:
  virtual
  ~event_scheduler() {
  }

  void
  cancel_all_messages();

  /**
   * @brief ipc_schedule Should only be called on stub handlers for which handler->ipc_handler() returns true
   * @param t         The time the event will run at
   * @param handler   The handler to receive the event. This should always be a stub for a real handler on a remote process.
   * @param ev        The event to deliver
   */
  void
  ipc_schedule(timestamp t,
    event_handler* handler,
    event* ev);

  virtual void
  deadlock_check() {}

  virtual void
  deadlock_check(event* ev) {}

  virtual void setup(); //needed for SST core compatibility

  virtual void init(unsigned int phase); //needed for SST core compatibility

  /**
   * Add an event to the event queue, where msg will get delivered to handler at time t.
   * @param t Time at which the event should happen
   * @param handler The handler for the event
   * @param msg The message to deliver to the handler
   */
  void
  schedule(timestamp t,
           event_handler* handler,
           event* ev);

  void
  schedule(timestamp t, event_queue_entry* ev);

  void
  schedule_now(event_queue_entry* ev);
  
  void
  schedule_now(event_handler* handler, event* ev);

  void
  schedule_delay(timestamp delay,
                 event_handler* handler,
                 event* ev);
  void
  schedule_delay(timestamp delay, event_queue_entry* ev);

  void
  send_self_event(timestamp arrival, event* ev);

  void
  send_delayed_self_event(timestamp delay, event* ev);

  void
  send_now_self_event(event* ev);

  void
  send_self_event_queue(timestamp arrival, event_queue_entry* ev);

  void
  send_delayed_self_event_queue(timestamp delay, event_queue_entry* ev);

  void
  send_now_self_event_queue(event_queue_entry* ev);

  /**
   * @brief send_to_link  The message should arrive now
   * @param lnk
   * @param ev
   */
  void
  send_to_link(event_handler* lnk, event* ev);

  /**
   * @brief send_to_link  The arrival time will be enter + lat
   * @param enter        The time the enters the link
   * @param lat
   * @param lnk
   * @param ev
   */
  void
  send_to_link(timestamp enter, timestamp lat,
               event_handler* lnk, event* ev);

  void
  send_delayed_to_link(timestamp extra_delay, timestamp lat,
               event_handler* lnk, event* ev);


  void
  send_delayed_to_link(timestamp extra_delay,
               event_handler* lnk, event* ev);

  void
  register_stat(stat_collector* coll);

  event_manager*
  event_mgr() const {
    return eventman_;
  }

  event_handler*
  self_handler() const {
    return self_handler_;
  }

 private:
  event_manager* eventman_;
  event_handler* self_handler_;

 protected:
  event_scheduler(sprockit::sim_parameters* params,
                  uint64_t cid,
                  event_loc_id id,
                  event_manager* mgr,
                  event_handler* self) :
#if SSTMAC_INTEGRATED_SST_CORE
    SSTIntegratedComponent(params, cid),
    locatable(id, locatable::null_threadid),
#else
    locatable(id, mgr->thread_id()),
    seqnum_(0),
    eventman_(mgr),
#endif
    self_handler_(self)
  {
  }

#if SSTMAC_INTEGRATED_SST_CORE
 private:
  void
  schedule(SST::SimTime_t delay, event_handler* handler, event* ev);
#else
 public:
  /**
   * get the current time
   * @return a timestamp
   */
  timestamp
  now() const {
    return eventman_->now();
  }

  int
  nthread() const {
    return eventman_->nthread();
  }


 private:
  void sanity_check(timestamp t);

  void
  multithread_schedule(int src_thread, int dst_thread,
    timestamp t, event_queue_entry* ev);

 private:
  uint32_t seqnum_;
#endif

};

class event_subscheduler :
  public locatable
{
 public:
  /**
   * get the current time
   * @return a timestamp
   */
  timestamp now() const {
    return parent_->now();
  }

  /**
   * Add an event to the event queue, where msg will get delivered to handler at time t.
   * @param t Time at which the event should happen
   * @param handler The handler for the event
   * @param msg The message to deliver to the handler
   */
  void
  schedule(timestamp t,
           event_handler* handler,
           event* ev);

  void
  schedule(timestamp t, event_queue_entry* ev);

  void
  schedule_now(event_queue_entry* ev);

  void
  schedule_now(event_handler* handler, event* ev);

  void
  schedule_delay(timestamp delay,
                 event_handler* handler,
                 event* ev);
  void
  schedule_delay(timestamp delay, event_queue_entry* ev);

  void
  send_self_event(timestamp arrival, event* ev);

  void
  send_delayed_self_event(timestamp delay, event* ev);

  void
  send_now_self_event(event* ev);

  void
  send_self_event_queue(timestamp arrival, event_queue_entry* ev);

  void
  send_delayed_self_event_queue(timestamp delay, event_queue_entry* ev);

  void
  send_now_self_event_queue(event_queue_entry* ev);

  /**
   * @brief send_to_link  The arrival time will be enter + lat
   * @param enter        The time the enters the link
   * @param lat
   * @param lnk
   * @param ev
   */
  void
  send_to_link(timestamp enter, timestamp lat,
               event_handler* lnk, event* ev);


  void
  send_delayed_to_link(timestamp extra_delay,
               event_handler* link,
               event* ev);

  /**
   * @brief send_to_link  The message should arrive now
   * @param lnk
   * @param ev
   */
  void
  send_to_link(event_handler* lnk, event* ev);

  void
  send_delayed_to_link(timestamp extra_delay, timestamp lat,
               event_handler* link, event* ev);

  virtual void
  deadlock_check() {}

  virtual void
  deadlock_check(event* ev) {}

  event_scheduler*
  parent() const {
    return parent_;
  }

 protected:
  event_subscheduler(event_scheduler* parent,
                     event_handler* self) :
    parent_(parent),
    self_handler_(self),
#if SSTMAC_INTEGRATED_SST_CORE
    locatable(parent->event_location(),
              locatable::null_threadid)
#else
    locatable(parent->event_location(),
              parent->thread_id())
#endif
  {
  }

 private:
  event_scheduler* parent_;
  event_handler* self_handler_;
 };


} // end of namespace sstmac
#endif


