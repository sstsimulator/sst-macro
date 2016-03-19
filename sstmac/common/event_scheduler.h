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

#include <sstmac/common/event_manager.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/common/stats/location_trace.h>
#include <sstmac/common/event_handler.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/event_manager_fwd.h>
#include <sstmac/common/sstmac_config.h>
#include <sprockit/sim_parameters_fwd.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/sst_core/integrated_component.h>
#include <sst/core/link.h>
#  define DeclareIntegratedComponent(comp) \
  SST::Component* \
  create_##comp(SST::ComponentId_t id, SST::Params& params); \
  extern const SST::ElementInfoComponent comp##_element_info;
#  define ImplementIntegratedComponent(comp) \
  SST::Component* \
  create_##comp(SST::ComponentId_t id, SST::Params& params) { \
    return new comp(id, params); \
  } \
  const SST::ElementInfoComponent comp##_element_info = { \
      #comp, \
      "undocumented (for now) SST/macro integrated component.", \
      NULL, \
      create_##comp, NULL, NULL, COMPONENT_CATEGORY_SYSTEM \
  };
#else
#  define DeclareIntegratedComponent(comp)
#  define ImplementIntegratedComponent(comp)
#endif

#ifdef INTEGRATED_SST_CORE_CHECK
#define LINK_SCHEDULE_CHECK \
  if (!correctly_scheduled_) spkt_throw(sprockit::illformed_error, "directly scheduled event");
#define SCHEDULE(...) \
  correctly_scheduled_ = true; schedule(__VA_ARGS__); correctly_scheduled_ = false
#define SCHEDULE_DELAY(...) \
  correctly_scheduled_ = true; schedule_delay(__VA_ARGS__); correctly_scheduled_ = false;
#define SCHEDULE_NOW(...) \
  correctly_scheduled_ = true; schedule_now(__VA_ARGS__); correctly_scheduled_ = false;
#define START_VALID_SCHEDULE(x) x->set_correctly_scheduled(true);
#define STOP_VALID_SCHEDULE(x) x->set_correctly_scheduled(false);
#else
#define LINK_SCHEDULE_CHECK
#define SCHEDULE(...) schedule(__VA_ARGS__)
#define SCHEDULE_DELAY(...) schedule_delay(__VA_ARGS__)
#define SCHEDULE_NOW(...) schedule_now(__VA_ARGS__)
#define START_VALID_SCHEDULE(x)
#define STOP_VALID_SCHEDULE(x)
#endif

namespace sstmac {

/**
 * The interface for something that can schedule messages
 */
class event_scheduler :
#if SSTMAC_INTEGRATED_SST_CORE
  public SSTIntegratedComponent,
#endif
  public event_handler
{

 public:
  virtual
  ~event_scheduler() {
  }

  virtual std::string
  to_string() const {
    return "event scheduler";
  }

  virtual void
  handle(event* ev);

  void
  cancel_all_messages();

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

  void
  register_stat(stat_collector* coll);

#ifdef INTEGRATED_SST_CORE_CHECK
 public:
  void
  set_correctly_scheduled(bool flag) {
    correctly_scheduled_ = flag;
  }
 protected:
  bool correctly_scheduled_;
#endif


#if SSTMAC_INTEGRATED_SST_CORE
 public:
  event_scheduler(
    SST::ComponentId_t id,
    SST::Params& params) : SSTIntegratedComponent(id, params)
  {
    global = this;
  }

  virtual void
  set_event_manager(event_manager* mgr){}

  timestamp
  now() const;

  virtual void
  init(unsigned int phase);

  static event_scheduler* global;

 private:
  void
  schedule(SST::Time_t delay, event_handler* handler, sst_message* msg);

#else
 public:
  /**
   * Set the eventmanager for this scheduler.  Unfortunately,
   * this always has to be called after an event_scheduler is constructed.
   * @param m the simulation eventmanager
   */
  virtual void
  set_event_manager(event_manager* m);

  event_manager*
  event_mgr() const {
    return eventman_;
  }

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

 protected:
  event_scheduler();

  uint32_t seqnum_;

 private:
  event_manager* eventman_;
#endif

};

class event_subscheduler :
  public event_handler
{
 public:
  event_subscheduler() : parent_(0){}
  /**
   * get the current time
   * @return a timestamp
   */
  timestamp now() const {
    return parent_->now();
  }

#if SSTMAC_INTEGRATED_SST_CORE
  void
  handle_event(SST::Event* ev);
#endif

  virtual std::string
  to_string() const {
    return "event subscheduler";
  }

  virtual void
  handle(event* ev);

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
   * Set the eventmanager for this scheduler.  Unfortunately,
   * this always has to be called after an event_scheduler is constructed.
   * @param m the simulation eventmanager
  */
  virtual void
  set_event_parent(event_scheduler* m){
    parent_ = m;
    init_thread_id(parent_->thread_id());
  }

  event_scheduler*
  parent() const {
    return parent_;
  }

 protected:
  event_scheduler* parent_;
 };


} // end of namespace sstmac
#endif


