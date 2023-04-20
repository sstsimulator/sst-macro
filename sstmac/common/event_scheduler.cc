/**
Copyright 2009-2023 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2023, NTESS

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
#include <sprockit/output.h>
#include <unistd.h>
#include <limits>

#include <unusedvariablemacro.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/sst_core/connectable_wrapper.h>
#endif

#define test_schedule(x) \
  if (dynamic_cast<link_wrapper*>(x)) abort()

namespace sstmac {

EventLink::~EventLink()
{
}

#if SSTMAC_INTEGRATED_SST_CORE
SST::TimeConverter* SharedBaseComponent::time_converter_ = nullptr;

IntegratedComponent::IntegratedComponent(uint32_t id) :
  IntegratedBaseComponent<SST::Component>("self", id)
{
  sprockit::output::init_out0(&std::cout);
  sprockit::output::init_err0(&std::cerr);
  sprockit::output::init_outn(&std::cout);
  sprockit::output::init_errn(&std::cerr);

  TimeDelta::initStamps(100); //100 as per tick
}
#else
uint64_t
EventLink::allocateSelfLinkId()
{
  uint64_t max = std::numeric_limits<uint64_t>::max();
  uint32_t offset = selfLinkIdCounter_++;
  return max - offset;
}

EventLink::ptr
MacroBaseComponent::allocateSubLink(const std::string& /*name*/, TimeDelta lat, LinkHandler* handler)
{
  return EventLink::ptr(new SubLink(lat, this, handler));
}

void
MacroBaseComponent::endSimulation()
{
  mgr_->stop();
}

void
MacroBaseComponent::sendExecutionEvent(Timestamp arrival, ExecutionEvent *ev)
{
  ev->setTime(arrival);
  ev->setSeqnum(seqnum_++);
  ev->setLink(selfLinkId_);
  mgr_->schedule(ev);
}

SST::Params&
MacroBaseComponent::getEmptyParams()
{
  static SST::Params params{};
  return params;
}

void
MacroBaseComponent::statNotFound(SST::Params & /*params*/, const std::string &name, const std::string &type)
{
  spkt_abort_printf("Bad stat type '%s' given for statistic '%s'",
                    type.c_str(), name.c_str());
}

void
MacroBaseComponent::registerStatisticCore(StatisticBase *base, SST::Params& params)
{
  mgr_->registerStatisticCore(base, params);
}

void
MacroBaseComponent::setManager()
{
  mgr_ = EventManager::global->componentManager(id_);
  thread_id_ = mgr_->thread();
  nthread_ = EventManager::global->nthread();
  now_ = mgr_->nowPtr();
}

TimeDelta EventLink::minRemoteLatency_;
TimeDelta EventLink::minThreadLatency_;
uint32_t EventLink::selfLinkIdCounter_{0};
#endif

void
Component::init(unsigned int phase)
{
  ComponentParent::init(phase);
}

void
Component::setup()
{
  ComponentParent::setup();
}


void
SubComponent::init(unsigned int phase)
{
  SubComponentParent::init(phase);
}

void
SubComponent::setup()
{
  SubComponentParent::setup();
}

#if SSTMAC_INTEGRATED_SST_CORE
#else
void
LocalLink::deliver(Event *ev)
{
  handler_->handle(ev);
}

void
LocalLink::send(TimeDelta delay, Event *ev)
{
  Timestamp arrival = mgr_->now() + delay + latency_;
  ExecutionEvent* qev = new HandlerExecutionEvent(ev, handler_);
  qev->setSeqnum(seqnum_++);
  qev->setTime(arrival);
  qev->setLink(linkId_);
  mgr_->schedule(qev);
}

void
IpcLink::send(TimeDelta delay, Event *ev)
{
  Timestamp arrival = ev_mgr_->now() + delay + latency_;
  debug_printf(sprockit::dbg::parallel,
      "manager %d:%d sending IPC event at t=%10.7e on link=%" PRIu64 " seq=%" PRIu32 " to arrive at t=%10.7e on epoch %d",
       ev_mgr_->me(), ev_mgr_->thread(), ev_mgr_->now().sec(), linkId_, seqnum_, arrival.sec(), ev_mgr_->epoch());
  ev_mgr_->setMinIpcTime(arrival);
  IpcEvent iev;
  iev.seqnum = seqnum_++;
  iev.ev = ev;
  iev.t = arrival;
  iev.rank = rank_;
  iev.thread = thread_;
  iev.link = linkId_;
  ipc_mgr_->ipcSchedule(&iev);
  //this guy is gone
  delete ev;
}

void
IpcLink::deliver(Event* /*ev*/){
  spkt_abort_printf("IpcLink: cannot direct deliver events");
}

void
MultithreadLink::send(TimeDelta delay, Event* ev)
{
  ExecutionEvent* qev = new HandlerExecutionEvent(ev, handler_);
  Timestamp arrival = mgr_->now() + delay + latency_;
  mgr_->setMinIpcTime(arrival);
  qev->setTime(arrival);
  qev->setSeqnum(seqnum_++);
  qev->setLink(linkId_);
  dst_mgr_->multithreadSchedule(mgr_->pendingSlot(), mgr_->thread(), qev);
}

void
MultithreadLink::deliver(Event* ev)
{
  handler_->handle(ev);
}
#endif

} // end of namespace sstmac
