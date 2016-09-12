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
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/event_manager_fwd.h>
#include <sprockit/sim_parameters_fwd.h>

#if SSTMAC_INTEGRATED_SST_CORE
namespace sstmac {
sprockit::sim_parameters*
make_spkt_params_from_sst_params(const SST::Params& map);
}

#include <sstmac/sst_core/integrated_component.h>
#include <sst/core/link.h>
#  define DeclareIntegratedComponent(comp, docstring) \
  SST::Component* \
  create_##comp(SST::ComponentId_t id, SST::Params& params); \
  extern const SST::ElementInfoComponent comp##_element_info;
#  define ImplementIntegratedComponent(comp) \
  SST::Component* \
  create_##comp(SST::ComponentId_t id, SST::Params& params) { \
    return new comp(id, sstmac::make_spkt_params_from_sst_params(params)); \
  } \
  const SST::ElementInfoComponent comp##_element_info = { \
      #comp, \
      docstring, \
      NULL, \
      create_##comp, NULL, NULL, COMPONENT_CATEGORY_SYSTEM \
  };
#else
#  define DeclareIntegratedComponent(comp)
#  define ImplementIntegratedComponent(comp)
#include <sstmac/common/event_manager.h>
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
   * @brief ipc_schedule Should only be called on stub handlers for which handler->ipc_handler() returns true
   * @param t         The time the event will run at
   * @param handler   The handler to receive the event. This should always be a stub for a real handler on a remote process.
   * @param ev        The event to deliver
   */
  void
  ipc_schedule(timestamp t,
    event_handler* handler,
    event* ev);

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

 protected:
  event_scheduler(sprockit::sim_parameters* params, uint64_t id, event_manager* mgr) :
#if SSTMAC_INTEGRATED_SST_CORE
   SSTIntegratedComponent(id, params)
#else
    seqnum_(0),
    eventman_(mgr)
#endif
  {
    init_thread_id(mgr->thread_id());
  }

 private:
#if SSTMAC_INTEGRATED_SST_CORE
 public:
  timestamp
  now() const;

  virtual void
  init(unsigned int phase);

 private:
  void
  schedule(SST::Time_t delay, event_handler* handler, event* ev);

#else
 public:
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


 private:
  void sanity_check(timestamp t);

  void
  multithread_schedule(int src_thread, int dst_thread,
    timestamp t, event_queue_entry* ev);

 private:
  uint32_t seqnum_;
  event_manager* eventman_;
#endif

};

class event_subscheduler :
  public event_handler
{
 public:
  event_subscheduler(event_scheduler* parent) : parent_(parent)
  {
    init_thread_id(parent_->thread_id());
    init_loc_id(parent_->event_location());
  }

  /**
   * get the current time
   * @return a timestamp
   */
  timestamp now() const {
    return parent_->now();
  }

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

  event_scheduler*
  parent() const {
    return parent_;
  }

 private:
  event_scheduler* parent_;
 };


} // end of namespace sstmac
#endif


