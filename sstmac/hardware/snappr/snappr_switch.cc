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


namespace sstmac {
namespace hw {

static FTQTag idle("idle_port", 0);
static FTQTag active("active_port", 0);
static FTQTag stalled("stalled_port", 0);

SnapprSwitch::SnapprSwitch(uint32_t id, SST::Params& params) :
  router_(nullptr),
  NetworkSwitch(id, params)
{
  SST::Params rtr_params = params.find_scoped_params("router");
  rtr_params.insert("id", std::to_string(my_addr_));
  router_ = sprockit::create<Router>(
   "macro", rtr_params.find<std::string>("name"), rtr_params, top_, this);

  bool congestion = params.find<bool>("congestion", true);

  SST::Params link_params = params.find_scoped_params("link");
  link_bw_ = link_params.find<SST::UnitAlgebra>("bandwidth").getValue().toDouble();

  bool flow_control = params.find<bool>("flow_control", true);
  uint32_t credits = std::numeric_limits<uint32_t>::max();
  if (flow_control){
    if (link_params.contains("credits")){
      credits = link_params.find<SST::UnitAlgebra>("credits").getRoundedValue();
    } else {
      spkt_abort_printf("must specify credits for Sculpin link when flow_control=true");
    }

    if (!congestion){
      spkt_abort_printf("must specify flow_control=false when congestion=false");
    }
  }


  // Ensure topology is set
  Topology::staticTopology(params);

  //vtk_ = registerStatistic<uint64_t,int,double,int>("traffic_intensity", getName());
  //if (vtk_) vtk_->configure(my_addr_, top_);

  qos_levels_ = params.find<int>("qos_levels", 1);

  std::string arbtype = params.find<std::string>("arbitrator", "fifo");
  outports_.reserve(top_->maxNumPorts());
  for (int i=0; i < top_->maxNumPorts(); ++i){
    std::string portTypeName = top_->portTypeName(addr(), i);
    std::string portName = sprockit::printf("Switch%d:%s:%d", addr(), portTypeName.c_str(), i);
    outports_.emplace_back(arbtype, portName, i,
                           congestion, flow_control, this);
  }

  inports_.resize(top_->maxNumPorts());
  num_vc_ = router()->numVC();
  num_vl_ = num_vc_ * qos_levels_;
  switch_debug("initializing with %d VCs and %" PRIu32 " total credits",
               num_vc_, credits);

  for (int i=0; i < outports_.size(); ++i){
    std::string subId = sprockit::printf("Switch:%d.Port:%d", addr(), i);
    SnapprOutPort& p = outports_[i];
    p.setVirtualLanes(num_vl_, credits);
    p.xmit_active = registerStatistic<uint64_t>(link_params, "xmit_active", subId);
    p.xmit_idle = registerStatistic<uint64_t>(link_params, "xmit_idle", subId);
    p.xmit_stall = registerStatistic<uint64_t>(link_params, "xmit_stall", subId);
    p.bytes_sent = registerStatistic<uint64_t>(link_params, "bytes_sent", subId);
    p.state_ftq = dynamic_cast<FTQCalendar*>(registerMultiStatistic<int,uint64_t,uint64_t>(link_params, "state", subId));
    p.queue_depth_ftq = dynamic_cast<FTQCalendar*>(registerMultiStatistic<int,uint64_t,uint64_t>(link_params, "queue_depth", subId));

    std::string portName = top_->portTypeName(addr(), i);
    p.ftq_idle_state = FTQTag::allocateCategoryId("idle:" + portName);
    p.ftq_active_state = FTQTag::allocateCategoryId("active:" + portName);
    p.ftq_stalled_state = FTQTag::allocateCategoryId("stalled:" + portName);
    p.inports = inports_.data();
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
  SnapprOutPort& p = outports_[src_outport];
  p.link = std::move(link);
  p.byte_delay = TimeDelta(1.0/port_bw);
  p.dst_port = dst_inport;
  p.scaleBuffers(scale_factor);
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
SnapprSwitch::deadlockCheck()
{
}

void
SnapprSwitch::handlePayload(SnapprPacket* pkt, int inport)
{
  pkt->setInport(inport);
  pkt->saveInputVirtualLane();
  router_->route(pkt);
  int vl = pkt->qos() * num_vc_ + pkt->deadlockVC();
  pkt->setVirtualLane(vl);
  if (vl >= (qos_levels_*num_vc_)){
    spkt_abort_printf("Bad QoS %d > max=%d", vl, (qos_levels_-1));
  }

  SnapprOutPort& p = outports_[pkt->nextPort()];
  TimeDelta time_to_send = p.byte_delay * pkt->numBytes();
  /** I am processing the head flit - so I assume compatibility with wormhole routing
    The tail flit cannot leave THIS switch prior to its departure time in the prev switch */
  if (pkt->timeToSend() > time_to_send){
    //delay the packet
    auto ev = newCallback(&p, &SnapprOutPort::tryToSendPacket, pkt);
    TimeDelta delta_t = pkt->timeToSend() - time_to_send;
    sendDelayedExecutionEvent(delta_t, ev);
  } else {
    p.tryToSendPacket(pkt);
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
  return newLinkHandler(&outports_[port], &SnapprOutPort::handle);
}

LinkHandler*
SnapprSwitch::payloadHandler(int port)
{
  switch_debug("returning payload handler on input port %d", port);
  return newLinkHandler(&inports_[port], &SnapprInPort::handle);
}




}
}
