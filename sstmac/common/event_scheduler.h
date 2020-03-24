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
#include <unusedvariablemacro.h>


extern int run_standalone(int, char**);

#if SSTMAC_INTEGRATED_SST_CORE
namespace sstmac {
  using LinkHandler = SST::Event::HandlerBase;

class EventLink {
 public:
  EventLink(const std::string& name, TimeDelta selflat, SST::Link* link) :
    link_(link),
    selflat_(selflat), 
    name_(name)
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

  virtual void deliver(Event* ev) = 0;

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
    seqnum_(0),
    linkId_(linkId), 
    latency_(latency)
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

class SharedBaseComponent {
 public:
  uint32_t componentId() const {
    return id_;
  }

  int nthread() const {
    return nthread_;
  }

  int threadId() const {
    return thread_id_;
  }

 protected:
  SharedBaseComponent(uint32_t id) :
    id_(id),
    thread_id_(0),
    nthread_(1)
  {
  }

 private:
  int id_;
  int thread_id_;
  int nthread_;

#if SSTMAC_INTEGRATED_SST_CORE
 public:
  static SST::TimeConverter* timeConverter() {
    return time_converter_;
  }
 protected:
  static SST::TimeConverter* time_converter_;
#endif
};

#if SSTMAC_INTEGRATED_SST_CORE
template <class Base>
class IntegratedBaseComponent :
  public Base,
  public SharedBaseComponent
{
 public:
  virtual std::string toString() const = 0;

  template <class T, class... Args> T* loadSub(const std::string& name, const std::string& iface, int slot_id,
                                               SST::Params& params, Args&&... args){
    auto* sub = Base::template loadAnonymousSubComponent<T>("macro." + name + "_" + iface, iface, slot_id,
                                          SST::ComponentInfo::SHARE_PORTS | SST::ComponentInfo::SHARE_STATS,
                                          params, std::forward<Args>(args)...);
    return dynamic_cast<T*>(sub);
  }

  template <class T, class... Args> T* newSub(const std::string& name, int slot_id,
                               SST::Params& params, Args&&... args){
    auto* sub = Base::template loadAnonymousSubComponent<T>("macro." + name, name, slot_id,
                                          SST::ComponentInfo::SHARE_PORTS | SST::ComponentInfo::SHARE_STATS,
                                          params, std::forward<Args>(args)...);
    return dynamic_cast<T*>(sub);
  }

  SST::SimTime_t getCurrentSimTime(SST::TimeConverter* tc) const {
    return Base::getCurrentSimTime(tc);
  }

  EventLinkPtr allocateSubLink(const std::string& name, TimeDelta lat, LinkHandler* handler){
    SST::Link* self = Base::configureSelfLink(name, timeConverter(), handler);
    return EventLink::ptr(new EventLink(name, lat, self));
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

  Timestamp now() const {
    SST::SimTime_t nowTicks = getCurrentSimTime(time_converter_);
    return Timestamp(uint64_t(0), uint64_t(nowTicks));
  }

  void endSimulation() {
    spkt_abort_printf("intgrated core does not support stopping");
  }

  void handleExecutionEvent(Event* ev){
    ExecutionEvent* sev = dynamic_cast<ExecutionEvent*>(ev);
    sev->execute();
    delete sev;
  }

protected:
 IntegratedBaseComponent(const std::string& selfname, uint32_t id) :
   Base(id),
   SharedBaseComponent(id)
 {
   if (!time_converter_){
     time_converter_ = Base::getTimeConverter(TimeDelta::tickIntervalString());
   }
   self_link_ = Base::configureSelfLink(selfname, time_converter_,
         new SST::Event::Handler<IntegratedBaseComponent>(this, &IntegratedBaseComponent::handleExecutionEvent));
 }

 private:
  SST::Link* self_link_;

};

/**
 * @brief The SSTIntegratedComponent class  Provides common functionality
 * for converting an sst/macro standalone Component into a
 * a SST::Component compatible with integration
 */
class IntegratedComponent
  : public IntegratedBaseComponent<SST::Component>
{
 public:
  /**
   * @brief connectInput All of these classes should implement the
   *        Connectable interface
   * @param src_outport
   * @param dst_inport
   * @param mod
   */
  virtual void connectInput(int src_outport, int dst_inport, EventLinkPtr&& link) = 0;

  /**
   * @brief connectOutput  All of these classes should implement
   *                        the Connectable interface
   * @param src_outport
   * @param dst_inport
   * @param mod
   */
  virtual void connectOutput(int src_outport, int dst_inport, EventLinkPtr&& link) = 0;

  /**
   * @brief payloadHandler
   * @param port
   * @return The handler that will receive payloads from an SST link
   */
  virtual SST::Event::HandlerBase* payloadHandler(int port) = 0;

  /**
   * @brief creditHandler
   * @param port
   * @return The handler that will receive credits from an SST link
   */
  virtual SST::Event::HandlerBase* creditHandler(int port) = 0;

  void initLinks(SST::Params& params);

 protected:
  IntegratedComponent(uint32_t id);

  SST::LinkMap* link_map_;

};

using ComponentParent = IntegratedComponent;
using SubComponentParent = IntegratedBaseComponent<SST::SubComponent>;
#else
class MacroBaseComponent
{
 public:
  virtual std::string toString() const = 0;

  virtual void init(unsigned int /*phase*/) {}

  virtual void setup() {}

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

  void endSimulation();

  void initLinks(SST::Params&){} //need for SST core compatibility

  template <class T, class... Args> T* loadSub(const std::string& name, const std::string& /*iface*/, int slot_id,
                                SST::Params& params, Args&&... args){
    return sprockit::create<T>("macro", name, componentId(), params, std::forward<Args>(args)...);
  }

  template <class T, class... Args> T* newSub(const std::string& /*name*/, int slot_id,
                               SST::Params& params, Args&&... args){
    return new T(componentId(), params, std::forward<Args>(args)...);
  }

 public:
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
    return *now_;
  }

  EventManager* mgr() const {
    return mgr_;
  }

  const Timestamp* nowPtr() const {
    return now_;
  }

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

  MacroBaseComponent(const std::string& /*selfname*/, uint32_t id) :
    mgr_(nullptr), 
    seqnum_(0), 
    selfLinkId_(EventLink::allocateSelfLinkId()),
    now_(nullptr), 

    id_(id), 
    thread_id_(0),
    nthread_(1)
  {
    setManager();
  }

  MacroBaseComponent(uint32_t id)
    : MacroBaseComponent("self", id)
  {
  }

  EventLink::ptr allocateSubLink(const std::string& /*name*/, TimeDelta lat, LinkHandler* handler);

 private:
  void registerStatisticCore(StatisticBase* base, SST::Params& params);

  EventManager* mgr_;
  uint32_t seqnum_;
  uint32_t selfLinkId_;
  const Timestamp* now_;

 protected:
  void setManager();

 private:
  uint32_t id_;
  int thread_id_;
  int nthread_;

  void statNotFound(SST::Params& params, const std::string& name, const std::string& type);

  static SST::Params& getEmptyParams();

  SST::Component* comp_;

};

using ComponentParent = MacroBaseComponent;
using SubComponentParent = MacroBaseComponent;
#endif

/**
 * The interface for something that can schedule messages
 */
class Component : public ComponentParent
{
 public:
  virtual ~Component() {}

  virtual void setup(); //needed for SST core compatibility

  virtual void init(unsigned int phase); //needed for SST core compatibility

 protected:
  Component(uint32_t cid, SST::Params& /*params*/) :
   ComponentParent(cid)
  {
  }

};



class SubComponent : public SubComponentParent
{

 public:
  virtual void setup(); //needed for SST core compatibility

  virtual void init(unsigned int phase); //needed for SST core compatibility

 protected:
  SubComponent(uint32_t id, const std::string& selfname, SST::Component* /*parent*/) :
    SubComponentParent(selfname, id)
  {
  }

};

#if SSTMAC_INTEGRATED_SST_CORE
template <class T, class Fxn>
SST::Event::HandlerBase* newLinkHandler(const T* t, Fxn fxn){
  return new SST::Event::Handler<T>(const_cast<T*>(t), fxn);
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

  void deliver(Event* ev) override;

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

  void deliver(Event* ev) override;

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

  void deliver(Event* ev) override;

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
  SubLink(TimeDelta lat, MacroBaseComponent* comp, EventHandler* handler) :
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

  void deliver(Event* ev) override {
    handler_->handle(ev);
  }

  void send(TimeDelta delay, Event *ev) override {
    comp_->sendDelayedExecutionEvent(delay, new HandlerExecutionEvent(ev, handler_));
  }

 private:
  MacroBaseComponent* comp_;
  EventHandler* handler_;
};

#endif

} // end of namespace sstmac
#endif
