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

#ifndef SSTMAC_COMMON_event_scheduler_H_INCLUDED
#define SSTMAC_COMMON_event_scheduler_H_INCLUDED

#include <sstmac/common/timestamp.h>
#include <sstmac/common/stats/location_trace.h>
#include <sstmac/common/event_handler.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/event_manager_fwd.h>
#include <sstmac/sst_core/integrated_component.h>
#include <sprockit/sim_parameters_fwd.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/params.h>
#include <sst/core/link.h>

namespace sstmac {
extern int run_standalone(int, char**);
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
  void send_self_event_queue(timestamp arrival, event_queue_entry* ev);

  void send_delayed_self_event_queue(timestamp delay, event_queue_entry* ev);

  void send_now_self_event_queue(event_queue_entry* ev);

  void register_stat(stat_collector* coll, stat_descr_t* descr);

  uint32_t component_id() const {
    return id_;
  }

#if SSTMAC_INTEGRATED_SST_CORE
 public:
  std::string to_string() const {
    return "event scheduler";
  }

  timestamp now() const;

  SST::Link* self_link() const {
    return self_link_;
  }

  SST::Component* comp() const {
    return comp_;
  }

  static SST::TimeConverter* time_converter() {
    return time_converter_;
  }

  void handle_self_event(SST::Event* ev);

 protected:
  friend int ::sstmac::run_standalone(int, char**);
  event_scheduler(uint32_t loc) :
    self_link_(nullptr),
    comp_(nullptr),
    id_(loc)
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
  uint32_t next_seqnum() {
    return seqnum_++;
  }

  int thread() const {
    return thread_id_;
  }

  int nthread() const {
    return nthread_;
  }

  timestamp now() const {
    return *now_;
  }

  const timestamp* now_ptr() const {
    return now_;
  }

  event_manager* event_mgr() const {
    return eventman_;
  }

 protected:
  event_scheduler(event_manager* mgr, uint32_t comp_id);

 private:
  event_manager* eventman_;
  uint32_t seqnum_;
  int thread_id_;
  int nthread_;
  const timestamp* now_;
#endif
 private:
  uint32_t id_;
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

  uint32_t component_id() const {
    return parent_->component_id();
  }

#if SSTMAC_INTEGRATED_SST_CORE
  timestamp now() const {
    return parent_->now();
  }

  int threadId() const {
    return 0;
  }

  int nthread() const {
    return 1;
  }
#else
  timestamp now() const {
    return *now_;
  }

  int threadId() const {
    return parent_->thread();
  }

  int nthread() const {
    return parent_->nthread();
  }
#endif
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
  event_subcomponent(event_scheduler* parent) :
    parent_(parent)
  {
  }

#else
 protected:
  event_subcomponent(event_scheduler* parent) :
    parent_(parent),
    now_(parent->now_ptr())
  {
  }

 private:
  const timestamp* now_;
#endif
 private:
  event_scheduler* parent_;
};

#if SSTMAC_INTEGRATED_SST_CORE
template <class T, class Fxn>
link_handler* new_link_handler(const T* t, Fxn fxn){
  return new SST::Event::Handler<T>(const_cast<T*>(t), fxn);
}

class event_link {
 public:
  virtual ~event_link(){}

  event_link(SST::Link* link, SST::Component* comp, event_handler* handler = nullptr) :
    link_(link), comp_(comp), handler_(handler)
  {
  }

  std::string to_string() const {
    return "SST core link";
  }

  void validate_latency(timestamp test_latency){}

  void send(event *ev){
    send_extra_delay(timestamp(0), ev);
  }

  void send_extra_delay(timestamp delay, event* ev);

  void multi_send_extra_delay(timestamp delay, event* ev, event_scheduler* src){
    send_extra_delay(delay, ev);
  }

 private:
  SST::Link* link_;

  SST::Component* comp_;

  event_handler* handler_;
};

static inline event_link* allocate_local_link(timestamp lat, event_component* comp, event_handler* handler)
{
  return new event_link(comp->self_link(), comp, handler);
}
#else
template <class T, class Fxn>
link_handler* new_link_handler(const T* t, Fxn fxn){
  return new_handler<T,Fxn>(const_cast<T*>(t), fxn);
}

class event_link {
 public:
  virtual ~event_link(){}

  void validate_latency(timestamp test_latency){
#if SSTMAC_SANITY_CHECK
    if (test_latency != latency_){
      spkt_abort_printf("link configured with incorrect latency: %8.4e != %8.4e",
                        test_latency.sec(), latency_.sec());
    }
#endif
  }

  virtual std::string to_string() const = 0;

  virtual bool is_external() const = 0;

  virtual void deadlock_check() = 0;

  virtual void deadlock_check(event* ev) = 0;

  virtual void handle(event* ev) = 0;

  virtual void multi_send_extra_delay(timestamp delay, event* ev, event_scheduler* src) = 0;

  virtual void send_extra_delay(timestamp delay, event *ev) = 0;

  void send(event* ev){
    send_extra_delay(timestamp(), ev);
  }

  static timestamp min_thread_latency() {
    return min_thread_latency_;
  }

  static timestamp min_remote_latency() {
    return min_remote_latency_;
  }

 protected:
  event_link(timestamp latency, event_scheduler* sched) :
    scheduler_(sched),
    latency_(latency)
  {}

  static void set_min_thread_latency(timestamp t){
    if (t.ticks() == 0){
      spkt_abort_printf("setting link latency to zero across threads!");
    }
    if (min_thread_latency_.ticks() == 0){
      min_thread_latency_ = t;
    } else {
      min_thread_latency_ = std::min(min_thread_latency_, t);
    }
  }

  static void set_min_remote_latency(timestamp t){
    if (t.ticks() == 0){
      spkt_abort_printf("setting link latency to zero across threads!");
    }
    if (min_remote_latency_.ticks() == 0){
      min_remote_latency_ = t;
    } else {
      min_remote_latency_ = std::min(min_remote_latency_, t);
    }
  }

  event_scheduler* scheduler_;
  timestamp latency_;
  static timestamp min_thread_latency_;
  static timestamp min_remote_latency_;

};

class local_link : public event_link {
 public:
  local_link(timestamp latency, event_scheduler* src, event_scheduler* dst, event_handler* hand) :
    handler_(hand),
    dst_(dst),
    event_link(latency, src)
  {
  }

  /**
   * For internal links
   * @brief local_link
   * @param es
   * @param hand
   */
  local_link(timestamp latency, event_scheduler* es, event_handler* hand) :
    handler_(hand),
    dst_(es),
    event_link(latency, es)
  {
  }

  bool is_external() const override {
    return false;
  }

  std::string to_string() const override {
    return dst_->to_string();
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

  void send_extra_delay(timestamp delay, event* ev) override {
    multi_send_extra_delay(delay, ev, scheduler_);
  }

  void multi_send_extra_delay(timestamp delay, event* ev, event_scheduler* src) override;

 protected:
  event_handler* handler_;
  event_scheduler* dst_;

};

class multithread_link : public local_link {
 public:
  multithread_link(event_handler* handler, timestamp latency,
                   event_scheduler* src, event_scheduler* dst) :
    local_link(latency, src, dst, handler)
  {
    set_min_thread_latency(latency);
  }

  bool is_external() const override {
    return true;
  }

  void send_extra_delay(timestamp delay, event *ev) override;

  void multi_send_extra_delay(timestamp arrival, event* ev, event_scheduler* src) override;

};

class ipc_link : public event_link {
 public:
  ipc_link(timestamp latency, int rank,
           event_scheduler* src, uint32_t dst,
           int port, bool is_credit) :
    rank_(rank), dst_(dst),
    is_credit_(is_credit),
    port_(port),
    event_link(latency, src)
  {
    set_min_remote_latency(latency);
  }

  bool is_external() const override {
    return true;
  }

  std::string to_string() const override {
    return "ipc link";
  }

  void handle(event *ev) override {
    sprockit::abort("ipc_link should not directly handle events");
  }

  void deadlock_check() override {}

  void deadlock_check(event* ev) override {}

  void send_extra_delay(timestamp delay, event* ev) override {
    multi_send_extra_delay(delay, ev, scheduler_);
  }

  void multi_send_extra_delay(timestamp delay, event* ev, event_scheduler* src) override;

 private:
  bool is_credit_;
  int rank_;
  int port_;
  uint32_t dst_;

};

static inline event_link* allocate_local_link(timestamp lat, event_component* comp, event_handler* handler)
{
  return new local_link(lat, comp, handler);
}
#endif

} // end of namespace sstmac
#endif
