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

#ifndef SSTMAC_COMMON_EventScheduler_H_INCLUDED
#define SSTMAC_COMMON_EventScheduler_H_INCLUDED

#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_handler.h>
#include <sstmac/common/handler_event_queue_entry.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/event_manager_fwd.h>
#include <sstmac/common/event_scheduler_fwd.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/sst_core/integrated_component.h>
#include <sprockit/sim_parameters_fwd.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/params.h>
#include <sst/core/link.h>
extern int run_standalone(int, char**);
namespace sstmac {
  using LinkHandler = SST::Event::HandlerBase;

class EventLink {
 public:
  EventLink(const std::string& name, TimeDelta selflat, SST::Link* link) :
    name_(name), selflat_(selflat), link_(link)
  {
  }

  using ptr = std::unique_ptr<EventLink>;

  virtual ~EventLink();

  std::string toString() const {
    return "self link: " + name_;
  }

  void send(TimeDelta delay, Event* ev){
    //the link should have a time converter built-in?
    link_->send(SST::SimTime_t((delay + selflat_).ticks()), ev);
  }

  void send(Event* ev){
    send(selflat_, ev);
  }

 private:
  SST::Link* link_;
  TimeDelta selflat_;
  std::string name_;
};
}

#else
namespace SST {
struct TimeConverter {};
namespace Event {
using HandlerBase = sstmac::EventHandler;
}
using sstmac::Component;
using BaseComponent = sstmac::EventScheduler;
using Link = sstmac::EventLink;
}
namespace sstmac {
  using LinkHandler = EventHandler;

class EventLink {
 public:
  virtual ~EventLink();

  using ptr = std::unique_ptr<EventLink>;

  virtual std::string toString() const = 0;

  virtual void send(TimeDelta delay, Event *ev) = 0;

  uint64_t id() const {
    return linkId_;
  }

  void send(Event* ev){
    send(TimeDelta(), ev);
  }

  static TimeDelta minThreadLatency() {
    return minThreadLatency_;
  }

  static TimeDelta minRemoteLatency() {
    return minRemoteLatency_;
  }

  static uint64_t allocateSelfLinkId();

 protected:
  EventLink(uint64_t linkId, TimeDelta latency) :
    linkId_(linkId), latency_(latency), seqnum_(0)
  {
  }

  static void setMinThreadLatency(TimeDelta t){
    if (t.ticks() == 0){
      spkt_abort_printf("setting link latency to zero across threads!");
    }
    if (minThreadLatency_.ticks() == 0){
      minThreadLatency_ = t;
    } else {
      minThreadLatency_ = std::min(minThreadLatency_, t);
    }
  }

  static void setMinRemoteLatency(TimeDelta t){
    if (t.ticks() == 0){
      spkt_abort_printf("setting link latency to zero across threads!");
    }
    if (minRemoteLatency_.ticks() == 0){
      minRemoteLatency_ = t;
    } else {
      minRemoteLatency_ = std::min(minRemoteLatency_, t);
    }
  }

  uint32_t seqnum_;
  uint64_t linkId_;
  TimeDelta latency_;
  static TimeDelta minThreadLatency_;
  static TimeDelta minRemoteLatency_;
  static uint32_t selfLinkIdCounter_;

};

}
#endif

namespace sstmac {

class EventScheduler : public sprockit::printable
{
 public:
#if SSTMAC_INTEGRATED_SST_CORE
  SST::SimTime_t getCurrentSimTime(SST::TimeConverter* tc) const {
    return comp_->getCurrentSimTime(tc);
  }

  void sendDelayedExecutionEvent(TimeDelta delay, ExecutionEvent* ev){
    self_link_->send(SST::SimTime_t(delay), time_converter_, ev);
  }

  void sendExecutionEventNow(ExecutionEvent* ev){
    self_link_->send(ev);
  }

  void sendExecutionEvent(Timestamp arrival, ExecutionEvent* ev){
    SST::SimTime_t delay = arrival.time.ticks() - getCurrentSimTime(time_converter_);
    self_link_->send(delay, time_converter_, ev);
  }
#else
  Component* getTrueComponent() const {
    return comp_;
  }

  template <class T> Statistic<T>*
  registerStatistic(SST::Params& params, const std::string& name, const std::string& subId = ""){
    return registerStatisticType<SST::Params,Statistic<T>>(params,name,subId);
  }

  template <class T> Statistic<T>*
  registerStatistic(const std::string& name, const std::string& subId = ""){
    return registerStatisticType<SST::Params,Statistic<T>>(getEmptyParams(),name,subId);
  }

  template <class... Args> SST::Statistics::Statistic<std::tuple<Args...>>*
  registerMultiStatistic(SST::Params& params, const std::string& name, const std::string& subId = ""){
    return registerStatisticType<SST::Params,SST::Statistics::Statistic<std::tuple<Args...>>>(params,name,subId);
  }

  template <class Params, class Stat> Stat*
  registerStatisticType(Params& params, const std::string& name, const std::string& subId){
    auto scoped_params = params.find_scoped_params(name);
    auto type = scoped_params.template find<std::string>("type", "null");
    auto* lib = Stat::getBuilderLibrary("macro");
    if (lib){
      auto* builder = lib->getBuilder(type);
      if (builder){
        Stat* stat = builder->create(this, name, subId, scoped_params);
        registerStatisticCore(stat, scoped_params);
        return stat;
      } else {
        statNotFound(scoped_params, name, type);
      }
    }
    return nullptr;
  }

  void sendDelayedExecutionEvent(TimeDelta delay, ExecutionEvent* ev){
    sendExecutionEvent(delay + now(), ev);
  }

  void sendExecutionEventNow(ExecutionEvent* ev){
    sendExecutionEvent(now(), ev);
  }

  void sendExecutionEvent(Timestamp arrival, ExecutionEvent* ev);
#endif

 public:
  std::string toString() const {
    return "event scheduler";
  }

  uint32_t componentId() const {
    return id_;
  }

  int nthread() const {
    return nthread_;
  }

  int threadId() const {
    return thread_id_;
  }

  Timestamp now() const {
#if SSTMAC_INTEGRATED_SST_CORE
    SST::SimTime_t nowTicks = getCurrentSimTime(time_converter_);
    return Timestamp(uint64_t(0), uint64_t(nowTicks));
#else
    return *now_;
#endif
  }

#if SSTMAC_INTEGRATED_SST_CORE
  static SST::TimeConverter* timeConverter() {
    return time_converter_;
  }
#else

  EventManager* mgr() const {
    return mgr_;
  }

  const Timestamp* nowPtr() const {
    return now_;
  }
#endif
  template <class Base, class... Args> Base* loadDerived(const std::string& name, Args&&... args){
    return Base::getBuilderLibrary("macro")->getBuilder(name)
                  ->create(std::forward<Args>(args)...);
  }


  void handleExecutionEvent(Event* ev){
    ExecutionEvent* sev = dynamic_cast<ExecutionEvent*>(ev);
    sev->execute();
    delete sev;
  }

 protected:
  //friend int ::sstmac::run_standalone(int, char**);

  EventScheduler(const std::string& selfname, uint32_t id, SST::Component* base) :
#if !SSTMAC_INTEGRATED_SST_CORE
   seqnum_(0), mgr_(nullptr), now_(nullptr), selfLinkId_(EventLink::allocateSelfLinkId()),
#endif
   comp_(base), id_(id), nthread_(1), thread_id_(0)
  {
#if SSTMAC_INTEGRATED_SST_CORE
    if (!time_converter_){
      time_converter_ = base->getTimeConverter(TimeDelta::tickIntervalString());
    }
    self_link_ = base->configureSelfLink(selfname, time_converter_,
          new SST::Event::Handler<EventScheduler>(this, &EventScheduler::handleExecutionEvent));
#else
    setManager();
#endif
  }

  EventScheduler(uint32_t id, SST::Component* base)
    : EventScheduler("self", id, base)
  {
  }

 private:
  uint32_t id_;
  int thread_id_;
  int nthread_;

#if SSTMAC_INTEGRATED_SST_CORE
  SST::Link* self_link_;
  static SST::TimeConverter* time_converter_;
#else

  void registerStatisticCore(StatisticBase* base, SST::Params& params);

  EventManager* mgr_;
  uint32_t seqnum_;
  uint32_t selfLinkId_;
  const Timestamp* now_;

 protected:
  void setManager();
#endif

 private:
  void statNotFound(SST::Params& params, const std::string& name, const std::string& type);

  static SST::Params& getEmptyParams();

  SST::Component* comp_;

};

/**
 * The interface for something that can schedule messages
 */
class Component :
#if SSTMAC_INTEGRATED_SST_CORE
  public SSTIntegratedComponent,
#endif
  public EventScheduler
{
 public:
  virtual ~Component() {}

  virtual void setup(); //needed for SST core compatibility

  virtual void init(unsigned int phase); //needed for SST core compatibility

 protected:
  Component(uint32_t cid, SST::Params& params) :
#if SSTMAC_INTEGRATED_SST_CORE
   SSTIntegratedComponent(params, cid),
   EventScheduler(cid, this)
#else
   EventScheduler(cid, nullptr)
#endif
  {
  }

#if SSTMAC_INTEGRATED_SST_CORE
  template <class T> T* loadSub(const std::string& name, SST::Params& params, const std::string& iface){
    auto* sub = loadSubComponent("macro." + name + "_" + iface, this, params);
    return dynamic_cast<T*>(sub);
  }

  template <class T> T* newSub(const std::string& name, SST::Params& params){
    auto* sub = loadSubComponent("macro." + name, this, params);
    return dynamic_cast<T*>(sub);
  }
#else
  void initLinks(SST::Params& params){} //need for SST core compatibility

  template <class T> T* loadSub(const std::string& name, SST::Params& params, const std::string& iface){
    return sprockit::create<T>("macro", name, this, params);
  }

  template <class T> T* newSub(const std::string& name, SST::Params& params){
    return new T(this, params);
  }
#endif

};



class SubComponent :
#if SSTMAC_INTEGRATED_SST_CORE
  public SST::SubComponent,
#endif
  public EventScheduler
{

 public:
  virtual void setup(); //needed for SST core compatibility

  virtual void init(unsigned int phase); //needed for SST core compatibility

  virtual std::string toString() const = 0;

 protected:
  SubComponent(const std::string& selfname, SST::Component* parent) :
#if SSTMAC_INTEGRATED_SST_CORE
    SST::SubComponent(parent),
    EventScheduler(selfname, 0, parent)
#else
    EventScheduler(selfname, parent->componentId(), parent)
#endif
  {
  }

};

#if SSTMAC_INTEGRATED_SST_CORE
template <class T, class Fxn>
SST::Event::HandlerBase* newLinkHandler(const T* t, Fxn fxn){
  return new SST::Event::Handler<T>(const_cast<T*>(t), fxn);
}

static inline EventLinkPtr allocateSubLink(const std::string& name, TimeDelta lat,
                                           sstmac::SubComponent* subcomp, LinkHandler* handler){
  SST::Link* self = subcomp->configureSelfLink(name, subcomp->timeConverter(), handler);
  return EventLink::ptr(new EventLink(name, lat, self));
}

static inline EventLinkPtr allocateSubLink(const std::string& name, TimeDelta lat,
                                           sstmac::Component* comp, LinkHandler* handler){
  SST::Link* self = comp->configureSelfLink(name, comp->timeConverter(), handler);
  return EventLink::ptr(new EventLink(name, lat, self));
}
#else
template <class T, class Fxn, class... Args>
SST::Event::HandlerBase* newLinkHandler(const T* t, Fxn fxn, Args&&... args){
  return new MemberFxnHandler<T, Fxn, Args...>(
        const_cast<T*>(t), fxn, std::forward<Args>(args)...);
}

class LocalLink : public EventLink {
 public:
  LocalLink(uint64_t linkId, TimeDelta latency, EventManager* mgr, EventHandler* hand) :
    EventLink(linkId, latency),
    handler_(hand),
    mgr_(mgr)
  {
  }

  virtual ~LocalLink() override {
    if (handler_) delete handler_;
  }

  std::string toString() const override {
    return handler_->toString();
  }

  void send(TimeDelta delay, Event* ev) override;

 protected:
  EventHandler* handler_;
  EventManager* mgr_;

};

class MultithreadLink : public LocalLink {
 public:
  MultithreadLink(uint64_t linkId, TimeDelta latency,
                  EventManager* src_mgr, EventManager* dst_mgr,
                  EventHandler* handler) :
    LocalLink(linkId, latency, src_mgr, handler),
    dst_mgr_(dst_mgr)
  {
    setMinThreadLatency(latency);
  }

  void send(TimeDelta delay, Event *ev) override;

 private:
  EventManager* dst_mgr_;

};

class IpcLink : public EventLink {
 public:
  IpcLink(uint64_t linkId,
         TimeDelta latency,
         int rank, int thread,
         EventManager* src_mgr,
         EventManager* ipc_mgr) :
    EventLink(linkId, latency),
    rank_(rank),
    thread_(thread),
    ev_mgr_(src_mgr),
    ipc_mgr_(ipc_mgr)
  {
    setMinRemoteLatency(latency);
  }

  std::string toString() const override {
    return "ipc link";
  }

  void send(TimeDelta delay, Event* ev) override;

 private:
  int rank_;
  int thread_;
  EventManager* ev_mgr_;
  EventManager* ipc_mgr_;

};

class SubLink : public EventLink
{
 public:
  SubLink(TimeDelta lat, Component* comp, EventHandler* handler) :
    EventLink(allocateSelfLinkId(), lat), //sub links have no latency
    comp_(comp), handler_(handler)
  {
  }

  ~SubLink(){
    if (handler_) delete handler_;
  }

  std::string toString() const override {
    return "sub link";
  }

  void send(TimeDelta delay, Event *ev) override {
    comp_->sendDelayedExecutionEvent(delay, new HandlerExecutionEvent(ev, handler_));
  }

 private:
  SST::Link* self_link_;
  Component* comp_;
  EventHandler* handler_;
};

static inline EventLink::ptr allocateSubLink(const std::string& name, TimeDelta lat,
                                             SST::Component* comp, LinkHandler* handler)
{
  return EventLink::ptr(new SubLink(lat, comp, handler));
}
#endif

} // end of namespace sstmac
#endif
