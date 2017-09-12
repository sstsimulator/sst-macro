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

#ifndef SSTMAC_COMMON_event_scheduler_H_INCLUDED
#define SSTMAC_COMMON_event_scheduler_H_INCLUDED

#include <sstmac/common/timestamp.h>
#include <sstmac/common/stats/location_trace.h>
#include <sstmac/common/event_handler.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/event_manager_fwd.h>
#include <sstmac/sst_core/integrated_component.h>
#include <sprockit/sim_parameters_fwd.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/params.h>
#include <sst/core/link.h>

namespace sstmac {
typedef SST::Event::HandlerBase link_handler;
}
#else
namespace sstmac {
typedef event_handler link_handler;
}
#include <sstmac/common/event_manager_fwd.h>
#endif

namespace sstmac {

class event_link;

class event_scheduler :
  public sprockit::printable
{
  friend class event_subcomponent;
  friend class clock_cycle_event_map;
  friend class event_component;
  friend class local_link;
  friend class ipc_link;
  friend class multithread_link;

 public:
  uint32_t next_seqnum() {
    return seqnum_++;
  }

  uint32_t component_id() const {
    return id_;
  }

  void send_self_event_queue(timestamp arrival, event_queue_entry* ev);

  void send_delayed_self_event_queue(timestamp delay, event_queue_entry* ev);

  void send_now_self_event_queue(event_queue_entry* ev);

  /**
   * @brief send_to_link  The message should arrive now
   * @param lnk
   * @param ev
   */
  void send_to_link(event_handler* lnk, event* ev);

  /**
   * @brief send_to_link  The arrival time will be enter + lat
   * @param enter        The time the enters the link
   * @param lat
   * @param lnk
   * @param ev
   */
  void send_to_link(timestamp enter, timestamp lat,
               event_handler* lnk, event* ev);

  void send_delayed_to_link(timestamp extra_delay, timestamp lat,
               event_handler* lnk, event* ev);


  void send_delayed_to_link(timestamp extra_delay,
               event_handler* lnk, event* ev);

  void register_stat(stat_collector* coll, stat_descr_t* descr);

#if SSTMAC_INTEGRATED_SST_CORE
 public:
  timestamp now() const;

  SST::Link* self_link() const {
    return self_link_;
  }

  SST::Component* comp() const {
    return comp_;
  }

  void handle_self_event(SST::Event* ev);

 protected:
  event_scheduler(uint32_t loc) :
   self_link_(nullptr),
   comp_(nullptr),
   id_(loc),
   seqnum_(0)
  {
  }

  void init_self_link(SST::Component* comp);

  void init_self_link(SST::Component* comp, SST::Link* self_link){
    comp_ = comp;
    self_link_ = self_link;
  }

  static SST::TimeConverter* time_converter_;

 private:
  SST::SimTime_t extra_delay(timestamp t) const;

  void schedule(SST::SimTime_t delay, event_handler* handler, event* ev);

  SST::Link* self_link_;

  SST::Component* comp_;
#else
 public:
  timestamp now() const {
    return now_;
  }

  event_manager* event_mgr() const {
    return eventman_;
  }

  /**
   * @brief run_events
   * @param event_horizon
   * @return Whether no more events or just hit event horizon
   */
  timestamp run_events(timestamp event_horizon);

  void clear_events() {
    event_queue_.clear();
  }

  void schedule(timestamp t, event_queue_entry* ev);

 private:
  /**
  * Add an event to the event queue, where msg will get delivered to handler at time t.
  * @param t Time at which the event should happen
  * @param handler The handler for the event
  * @param msg The message to deliver to the handler
  */
  void schedule(timestamp t, event_handler* handler, event* ev);

  void schedule_now(event_queue_entry* ev);

  void schedule_now(event_handler* handler, event* ev);

  void schedule_delay(timestamp delay, event_handler* handler, event* ev);

  void schedule_delay(timestamp delay, event_queue_entry* ev);

  void multithread_append(timestamp t, event_handler* handler, event* ev){}

 protected:
  event_scheduler(event_manager* mgr, uint32_t comp_id) :
    eventman_(mgr),
    seqnum_(0),
    id_(comp_id)
  {
  }

 private:
  event_manager* eventman_;
  uint32_t seqnum_;
  uint32_t id_;
  bool registered_event_;
  timestamp event_horizion_;
  timestamp now_;

  struct event_compare {
    bool operator()(event_queue_entry* lhs, event_queue_entry* rhs) {
      bool neq = lhs->time() != rhs->time();
      if (neq) return lhs->time() < rhs->time();

      if (lhs->src_component_id() == rhs->src_component_id()){
        return lhs->seqnum() < rhs->seqnum();
      } else {
        return lhs->src_component_id() < rhs->src_component_id();
      }
    }

  };
  typedef std::set<event_queue_entry*, event_compare> queue_t;
  queue_t event_queue_;
#endif

};

/**
 * The interface for something that can schedule messages
 */
class event_component :
  public event_scheduler
#if SSTMAC_INTEGRATED_SST_CORE
  , public SSTIntegratedComponent
#endif
{
  friend class event_subcomponent;
 public:
  virtual ~event_component() {}

  void cancel_all_messages();

  virtual void deadlock_check() {}

  virtual void deadlock_check(event* ev) {}

  virtual void setup(); //needed for SST core compatibility

  virtual void init(unsigned int phase); //needed for SST core compatibility

#if SSTMAC_INTEGRATED_SST_CORE
 protected:
  event_component(sprockit::sim_parameters* params,
                 uint32_t cid,
                 event_manager* mgr);
#else
 protected:
  event_component(sprockit::sim_parameters* params,
                uint32_t cid,
                event_manager* mgr) :
   event_scheduler(mgr, cid)
  {
  }

  void init_links(sprockit::sim_parameters* params){} //need for SST core compatibility
#endif
};

class event_subcomponent
{

 public:
  virtual void setup(); //needed for SST core compatibility

  virtual void init(unsigned int phase); //needed for SST core compatibility

  virtual void deadlock_check() {}

  virtual void deadlock_check(event* ev){}

  virtual std::string to_string() const = 0;

  timestamp now() const {
    return parent_->now();
  }

  uint32_t component_id() const {
    return parent_->component_id();
  }

  void send_self_event_queue(timestamp t, event_queue_entry* ev){
    parent_->send_self_event_queue(t, ev);
  }

  void send_delayed_self_event_queue(timestamp delay, event_queue_entry* ev){
    parent_->send_delayed_self_event_queue(delay, ev);
  }

  void send_now_self_event_queue(event_queue_entry* ev){
    parent_->send_now_self_event_queue(ev);
  }

#if SSTMAC_INTEGRATED_SST_CORE
 public:
  event_subcomponent(event_scheduler* parent);
#else
 protected:
  event_subcomponent(event_scheduler* parent) :
    parent_(parent)
  {
  }

 private:
  event_scheduler* parent_;
#endif
};

#if SSTMAC_INTEGRATED_SST_CORE
template <class T, class Fxn>
link_handler* new_link_handler(const T* t, Fxn fxn){
  return new SST::Event::Handler<T>(const_cast<T*>(t), fxn);
}
#else
template <class T, class Fxn>
link_handler* new_link_handler(const T* t, Fxn fxn){
  return new_handler<T,Fxn>(const_cast<T*>(t), fxn);
}
#endif

class event_link {
 public:
  virtual void send(timestamp arrival, event* ev) = 0;

  virtual ~event_link(){}

  virtual std::string to_string() const = 0;

  virtual void deadlock_check() = 0;

  virtual void deadlock_check(event* ev) = 0;

  virtual void handle(event* ev) = 0;

  void send_delay(timestamp delay, event* ev){
    send(scheduler_->now() + delay, ev);
  }

  void send_now(event* ev){
    send(scheduler_->now(), ev);
  }

  void send_extra_delay(timestamp extra_delay, timestamp delay, event* ev){
    timestamp arr = extra_delay + delay + scheduler_->now();
    send(arr, ev);
  }

 protected:
  event_link(event_scheduler* sched) : scheduler_(sched) {}

  event_scheduler* scheduler_;

};

class local_link : public event_link {
 public:
  local_link(event_scheduler* es, event_handler* hand) :
    handler_(hand),
    event_link(es)
  {
  }

  std::string to_string() const override {
    return handler_->to_string();
  }

  void handle(event* ev) override {
    handler_->handle(ev);
  }

  void deadlock_check() override {
    handler_->deadlock_check();
  }

  void deadlock_check(event* ev) override {
    handler_->deadlock_check(ev);
  }

  void send(timestamp arrival, event* ev) override {
    scheduler_->schedule(arrival, handler_, ev);
  }

 private:
  event_handler* handler_;

};

class ipc_link : public event_link {
 public:
  ipc_link(event_manager* mgr, int rank,
           event_scheduler* src, uint32_t dst,
           int port, bool is_credit) :
    rank_(rank), src_(src->component_id()), dst_(dst),
    is_credit_(is_credit), mgr_(mgr),
    port_(port),
    event_link(src)
  {
  }

  std::string to_string() const override {
    return "ipc link";
  }

  void handle(event *ev) override {
    sprockit::abort("ipc_link should not directly handle events");
  }

  void deadlock_check() override {}

  void deadlock_check(event* ev) override {}

  void send(timestamp arrival, event* ev) override;

 private:
  bool is_credit_;
  int rank_;
  int port_;
  uint32_t dst_;
  uint32_t src_;
  event_manager* mgr_;

};

} // end of namespace sstmac
#endif
