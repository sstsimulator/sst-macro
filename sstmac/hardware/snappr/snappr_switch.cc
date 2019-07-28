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

#ifndef __STDC_FORMAT_MACROS
#define __STDC_FORMAT_MACROS
#endif

#include <inttypes.h>

#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/snappr/snappr_switch.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/common/stats/ftq.h>
#include <sstmac/common/stats/ftq_tag.h>
#include <sstmac/common/event_callback.h>
#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/factory.h>
#endif
RegisterNamespaces("switch", "router", "xbar", "link");



#define pkt_debug(...) \
  debug_printf(sprockit::dbg::snappr, "snappr switch %d: %s", \
    int(addr()), sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

static FTQTag idle("idle_port", 0);
static FTQTag active("active_port", 0);
static FTQTag stalled("stalled_port", 0);

SnapprSwitch::SnapprSwitch(uint32_t id, SST::Params& params) :
  router_(nullptr),
  congestion_(true),
  send_credits_(false),
  NetworkSwitch(id, params)
{
  SST::Params rtr_params = params.find_scoped_params("router");
  rtr_params.insert("id", std::to_string(my_addr_));
  router_ = sprockit::create<Router>(
   "macro", rtr_params.find<std::string>("name"), rtr_params, top_, this);

  congestion_ = params.find<bool>("congestion", true);

  SST::Params link_params = params.find_scoped_params("link");
  link_bw_ = link_params.find<SST::UnitAlgebra>("bandwidth").getValue().toDouble();

  send_credits_ = params.find<bool>("flow_control", true);
  uint32_t credits = std::numeric_limits<uint32_t>::max();
  if (send_credits_){
    if (link_params.contains("credits")){
      credits = link_params.find<SST::UnitAlgebra>("credits").getRoundedValue();
    } else {
      spkt_abort_printf("must specify credits for Sculpin link when flow_control=true");
    }

    if (!congestion_){
      spkt_abort_printf("must specify flow_control=false when congestion=false");
    }
  }


  // Ensure topology is set
  Topology::staticTopology(params);

  //vtk_ = registerStatistic<uint64_t,int,double,int>("traffic_intensity", getName());
  //if (vtk_) vtk_->configure(my_addr_, top_);

  outports_.resize(top_->maxNumPorts());
  inports_.resize(top_->maxNumPorts());
  num_vc_ = router()->numVC();
  num_vl_ = num_vc_;
  switch_debug("initializing with %d VCs and %" PRIu32 " total credits",
               num_vc_, credits);

  ftq_idle_states_.resize(top_->maxNumPorts());
  ftq_active_states_.resize(top_->maxNumPorts());
  ftq_stalled_states_.resize(top_->maxNumPorts());

  for (int i=0; i < outports_.size(); ++i){
    std::string subId = sprockit::printf("Switch:%d.Port:%d", addr(), i);
    OutPort& p = outports_[i];
    p.number = i;
    p.setVirtualLanes(num_vl_, credits);
    p.xmit_active = registerStatistic<uint64_t>(link_params, "xmit_active", subId);
    p.xmit_idle = registerStatistic<uint64_t>(link_params, "xmit_idle", subId);
    p.xmit_stall = registerStatistic<uint64_t>(link_params, "xmit_stall", subId);
    p.bytes_sent = registerStatistic<uint64_t>(link_params, "bytes_sent", subId);
    p.state_ftq = dynamic_cast<FTQCalendar*>(registerMultiStatistic<int,uint64_t,uint64_t>(link_params, "state", subId));
    p.queue_depth_ftq = dynamic_cast<FTQCalendar*>(registerMultiStatistic<int,uint64_t,uint64_t>(link_params, "queue_depth", subId));

    std::string portName = top_->portTypeName(addr(), i);
    ftq_idle_states_[i] = FTQTag::allocateCategoryId("idle:" + portName);
    ftq_active_states_[i] = FTQTag::allocateCategoryId("active:" + portName);
    ftq_stalled_states_[i] = FTQTag::allocateCategoryId("stalled:" + portName);
  }
  for (int i=0; i < inports_.size(); ++i){
    inports_[i].number = i;
  }
  initLinks(params);
}

SnapprSwitch::~SnapprSwitch()
{
  if (router_) delete router_;
}

void
SnapprSwitch::connectOutput(int src_outport, int dst_inport, EventLink::ptr&& link)
{
  double scale_factor = top_->portScaleFactor(my_addr_, src_outport);
  double port_bw = scale_factor * link_bw_;
  OutPort& p = outports_[src_outport];
  p.link = std::move(link);
  p.byte_delay = TimeDelta(1.0/port_bw);
  p.dst_port = dst_inport;
  p.scaleBuffers(scale_factor);
  p.parent = this;
  switch_debug("connecting output port %d to input port %d with scale=%10.4f byte_delay=%10.5e",
               src_outport, dst_inport, scale_factor, p.byte_delay.sec());
}

void
SnapprSwitch::connectInput(int src_outport, int dst_inport, EventLink::ptr&& link)
{
  switch_debug("connecting input port %d to output port %d", dst_inport, src_outport);
  //no-op
  auto& port = inports_[dst_inport];
  port.src_outport = src_outport;
  port.link = std::move(link);
  port.parent = this;
}

int
SnapprSwitch::queueLength(int port, int vc) const
{
  auto& p = outports_[port];
  //VC basically ignored, all ports on "same" VC
  return p.queueLength();
}

void
SnapprSwitch::handleCredit(SnapprCredit* credit, int port)
{
  OutPort& p = outports_[port];
  p.addCredits(credit->virtualLane(), credit->numBytes());
  pkt_debug("crediting port %d with %" PRIu32" credits",
            port, credit->numBytes());
  delete credit;
  if (!p.arbitration_scheduled && !p.empty()){
    requestArbitration(p);
  }
}

void
SnapprSwitch::deadlockCheck()
{
}

void
SnapprSwitch::send(OutPort& p, SnapprPacket* pkt, Timestamp now)
{
#if SSTMAC_SANITY_CHECK
  if (p.next_free > now){
    spkt_abort_printf("Internal error: snappr switch %d port %d sending packet at time %llu < %llu",
                      addr(), p.number, now.time.ticks(), p.next_free.time.ticks());
  }
#endif

  if (!p.stall_start.empty()){
    TimeDelta stall_time = now - p.stall_start;
    p.xmit_stall->addData(stall_time.ticks());
    if (p.state_ftq){
      p.state_ftq->addData(ftq_stalled_states_[p.dst_port], p.stall_start.time.ticks(), stall_time.ticks());
    }
    if (p.stall_start > p.next_free){
      //we also have idle time
      TimeDelta idle_time = p.stall_start - p.next_free;
      p.xmit_idle->addData(idle_time.ticks());
      if (p.state_ftq){
        p.state_ftq->addData(ftq_idle_states_[p.dst_port], p.next_free.time.ticks(), idle_time.ticks());
      }
    }
    p.stall_start = Timestamp();
  } else if (now > p.next_free){
    TimeDelta idle_time = now - p.next_free;
    p.xmit_idle->addData(idle_time.ticks());
    if (p.state_ftq){
      p.state_ftq->addData(ftq_idle_states_[p.dst_port], p.next_free.time.ticks(), idle_time.ticks());
    }
  }

  TimeDelta time_to_send = pkt->numBytes() * p.byte_delay;
  p.bytes_sent->addData(pkt->numBytes());
  p.xmit_active->addData(time_to_send.ticks());
  if (p.state_ftq){
    p.state_ftq->addData(ftq_active_states_[p.dst_port], now.time.ticks(), time_to_send.ticks());
  }
  p.next_free = now + time_to_send;
  pkt->setTimeToSend(time_to_send);
  pkt->accumulateCongestionDelay(now);
  p.link->send(pkt);

  if (send_credits_){
    auto& inport = inports_[pkt->inport()];
    inport.link->send(time_to_send, new SnapprCredit(pkt->byteLength(), pkt->inputVirtualLane(), inport.src_outport));
  } else {
    //immediately add the credits back - we don't worry about credits here
    p.addCredits(pkt->virtualLane(), pkt->byteLength());
  }

  pkt_debug("packet leaving port %d at t=%8.4e: %s",
            p.number, p.next_free.sec(), pkt->toString().c_str());
  if (p.readyVirtualLanes()){
    scheduleArbitration(p);
  }
}

void
SnapprSwitch::scheduleArbitration(OutPort& p)
{
#if SSTMAC_SANITY_CHECK
  if (p.arbitration_scheduled){
    spkt_abort_printf("arbitration already scheduled on switch %d port %d", addr(), p.number);
  }
#endif
  pkt_debug("scheduling arbitrate from port %d at t=%8.4e with %d queued",
            p.number, p.next_free.sec(), p.queueLength());
  //schedule this port to pull another packet
  auto* ev = newCallback(this, &SnapprSwitch::arbitrate, p.number);
  sendExecutionEvent(p.next_free, ev);
  p.arbitration_scheduled = true;
}

void
SnapprSwitch::requestArbitration(OutPort& p)
{
#if SSTMAC_SANITY_CHECK
  if (p.empty()){
    spkt_abort_printf("SnapprSwitch::arbitrate: incorrectly requesting arbitrate on switch %d from empty port %d",
                      int(addr()), p.number);
  }
  if (p.arbitration_scheduled){
    spkt_abort_printf("SnapprSwitch::arbitrate: incorrectly requesting arbitrate on switch %d from port %d with arbitrate scheduled already",
                      int(addr()), p.number);
  }
#endif
  Timestamp now_ = now();
  if (p.next_free > now_){
    scheduleArbitration(p);
  } else {
    arbitrate(p.number);
  }
}

void
SnapprSwitch::arbitrate(int portnum)
{
  OutPort& p = outports_[portnum];
#if SSTMAC_SANITY_CHECK
  if (p.empty()){
    spkt_abort_printf("SnapprSwitch::arbitrate: incorrectly arbitrate switch %d from empty port %d",
                      int(addr()), p.number);
  }
  if (p.next_free > now()){
    spkt_abort_printf("SnapprSwitch::arbitrate: switch %d arbitrating before port %d is free to send",
                      int(addr()), p.number);
  }
#endif

  p.arbitration_scheduled = false;
  if (p.readyVirtualLanes()){
    logQueueDepth(p);
    SnapprPacket* pkt = p.popReady();
    pkt_debug("arbitrating packet from port %d:%d with %d queued",
              portnum, pkt->virtualLane(), p.queueLength());
    send(p, pkt, now());
  } else {
    if (p.stall_start.empty()){
      p.stall_start = now();
    }
    pkt_debug("insufficient credits to send on port %d with %d queued",
              portnum, p.queueLength());
  }
}

void
SnapprSwitch::logQueueDepth(OutPort& p)
{
  if (p.queue_depth_ftq){
    TimeDelta dt = now() - p.last_queue_depth_collection;
    p.queue_depth_ftq->addData(p.queueLength(), p.last_queue_depth_collection.time.ticks(), dt.ticks());
    p.last_queue_depth_collection = now();
  }
}

void
SnapprSwitch::tryToSendPacket(SnapprPacket* pkt)
{
  Timestamp now_ = now();

  pkt->setArrival(now_);
  OutPort& p = outports_[pkt->nextPort()];

  if (!congestion_){
    TimeDelta time_to_send = pkt->numBytes() * p.byte_delay;
    pkt->setTimeToSend(time_to_send);
    p.link->send(pkt);
  } else {
    logQueueDepth(p);
    p.queue(pkt);
    pkt_debug("incoming packet on port %d -> queue=%d packets now: %s",
              p.number, p.queueLength(), pkt->toString().c_str());
    if (!p.arbitration_scheduled){
      requestArbitration(p);
    }
  }
}

void
SnapprSwitch::handlePayload(SnapprPacket* pkt, int inport)
{
  pkt->setInport(inport);
  pkt->saveInputVirtualLane();
  router_->route(pkt);
  int vl = pkt->qos() * num_vc_ + pkt->deadlockVC();
  pkt->setVirtualLane(vl);
  pkt_debug("handling payload %s on inport %d:%d going to port %d:%d",
            pkt->toString().c_str(), inport, pkt->inputVirtualLane(), pkt->nextPort(), vl);

  OutPort& p = outports_[pkt->nextPort()];
  TimeDelta time_to_send = p.byte_delay * pkt->numBytes();
  /** I am processing the head flit - so I assume compatibility with wormhole routing
    The tail flit cannot leave THIS switch prior to its departure time in the prev switch */
  if (pkt->timeToSend() > time_to_send){
    //delay the packet
    auto ev = newCallback(this, &SnapprSwitch::tryToSendPacket, pkt);
    TimeDelta delta_t = pkt->timeToSend() - time_to_send;
    sendDelayedExecutionEvent(delta_t, ev);
    pkt_debug("delaying packet on port %d for %8.4es so tail flit can arrive", inport, delta_t.sec());
  } else {
    tryToSendPacket(pkt);
  }
}

std::string
SnapprSwitch::toString() const
{
  return sprockit::printf("snappr switch %d", int(my_addr_));
}

LinkHandler*
SnapprSwitch::creditHandler(int port)
{
  switch_debug("returning credit handler on output port %d", port);
  return newLinkHandler(&outports_[port], &OutPort::handle);
}

LinkHandler*
SnapprSwitch::payloadHandler(int port)
{
  switch_debug("returning payload handler on input port %d", port);
  return newLinkHandler(&inports_[port], &InPort::handle);
}

void
SnapprSwitch::InPort::handle(Event *ev)
{
  parent->handlePayload(static_cast<SnapprPacket*>(ev), number);
}

std::string
SnapprSwitch::InPort::toString() const
{
  return sprockit::printf("SNAPPR InPort %d", number);
}

void
SnapprSwitch::OutPort::handle(Event *ev)
{
  parent->handleCredit(static_cast<SnapprCredit*>(ev), number);
}

std::string
SnapprSwitch::OutPort::toString() const
{
  return sprockit::printf("SNAPPR OutPort %d", number);
}

}
}
