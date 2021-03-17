/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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
  NetworkSwitch(id, params)
{
  SST::Params rtr_params = params.find_scoped_params("router");
  rtr_params.insert("id", std::to_string(my_addr_));
  qos_levels_ = params.find<int>("qos_levels", 1);
  if (rtr_params.contains("names")){
    std::vector<std::string> names;
    rtr_params.find_array("names", names);
    if (names.size() != qos_levels_){
      spkt_abort_printf("Given %d router types, but %d QoS levels",
        qos_levels_, int(names.size()));
    }
    routers_.resize(qos_levels_);
    for (int i=0; i < qos_levels_; ++i){
      routers_[i] = sprockit::create<Router>(
        "macro", names[i], rtr_params, top_, this);
    }
  } else {
    routers_.resize(qos_levels_);
    for (int i=0; i < qos_levels_; ++i){
      Router* rtr = sprockit::create<Router>(
       "macro", rtr_params.find<std::string>("name"), rtr_params, top_, this);
      routers_[i] = rtr;
    }
  }
  int vl_offset = 0;
  for (Router* rtr : routers_){
    rtr->setVlOffset(vl_offset);
    vl_offset += rtr->numVC();
  }
  num_vl_ = vl_offset;

  bool congestion = params.find<bool>("congestion", true);

  SST::Params link_params = params.find_scoped_params("link");
  link_bw_ = link_params.find<SST::UnitAlgebra>("bandwidth").getValue().toDouble();

  bool flow_control = params.find<bool>("flow_control", true);
  std::vector<uint32_t> credits_per_vl(num_vl_);
  if (flow_control){
    if (link_params.contains("vl_credits")){
      std::vector<std::string> vl_credits;
      link_params.find_array("vl_credits", vl_credits);
      if (vl_credits.size() != num_vl_){
        spkt_abort_printf("Have %d VLs, but given credit array of size %d",
          num_vl_, int(vl_credits.size()));
      }
      for (int vl=0; vl < num_vl_; ++vl){
        uint32_t credits = SST::UnitAlgebra(vl_credits[vl]).getRoundedValue();
        credits_per_vl[vl] = credits;
      }
    } else if (link_params.contains("qos_credits")){
      std::vector<std::string> qos_credits;
      link_params.find_array("qos_credits", qos_credits);
      if (qos_levels_ != qos_credits.size()){
        spkt_abort_printf("Have %d QoS levels, but given credit array of size %d",
          qos_levels_, int(qos_credits.size()));
      }
      int vl_offset = 0;
      for (int q=0; q < qos_levels_; ++q){
        Router* rtr = routers_[q];
        uint32_t credits_per = 
          SST::UnitAlgebra(qos_credits[q]).getRoundedValue() / rtr->numVC();
        for (int vc=0; vc < rtr->numVC(); ++vc){
          credits_per_vl[vc + vl_offset] = credits_per;
        }
        vl_offset += rtr->numVC();
      }
    } else if (link_params.contains("credits")){
      uint32_t credits = link_params.find<SST::UnitAlgebra>("credits").getRoundedValue();
      uint32_t credits_per = credits / num_vl_;
      for (int vl=0; vl < num_vl_; ++vl){
        credits_per_vl[vl] = credits_per;
      }
    } else {
      spkt_abort_printf("must specify credits for SNAPPR link when flow_control=true");
    }

    if (!congestion){
      spkt_abort_printf("must specify flow_control=false when congestion=false");
    }
  } else {
    uint32_t credits = std::numeric_limits<uint32_t>::max();
    for (int vl=0; vl < num_vl_; ++vl){
      credits_per_vl[vl] = credits;
    }
  }

  std::vector<int> vls_per_qos(qos_levels_);
  for (int q=0; q < qos_levels_; ++q){
    vls_per_qos[q] = routers_[q]->numVC();
  }

  // Ensure topology is set
  Topology::staticTopology(params);

  //vtk_ = registerStatistic<uint64_t,int,double,int>("traffic_intensity", getName());
  //if (vtk_) vtk_->configure(my_addr_, top_);

  TimeDelta byte_delay(1.0/link_bw_);
  std::string sw_arbtype = params.find<std::string>("arbitrator", "fifo");
  std::string link_arbtype = link_params.find<std::string>("arbitrator", sw_arbtype);
  outports_.resize(top_->maxNumPorts());
  inports_.resize(top_->maxNumPorts());
  for (int i=0; i < top_->maxNumPorts(); ++i){
    std::string subId = sprockit::sprintf("Switch:%d.Port:%d", addr(), i);
    std::string portName = top_->portTypeName(addr(), i);
    outports_[i] = loadSub<SnapprOutPort>("snappr", "outport", i, link_params,
                                          link_arbtype, subId, portName, i,
                                          byte_delay, congestion, flow_control, this,
                                          vls_per_qos);
    outports_[i]->setVirtualLanes(credits_per_vl);
    outports_[i]->inports = inports_.data();
  }

  for (int i=0; i < inports_.size(); ++i){
    inports_[i].number = i;
  }

  configureLinks();
}

SnapprSwitch::~SnapprSwitch()
{
  for (Router* rtr : routers_){
    if (rtr) delete rtr;
  }
}

void
SnapprSwitch::connectOutput(int src_outport, int dst_inport, EventLink::ptr&& link)
{
  double scale_factor = top_->portScaleFactor(my_addr_, src_outport);
  double port_bw = scale_factor * link_bw_;
  SnapprOutPort* p = outports_[src_outport];
  if (p->link){
    spkt_abort_printf("Bad connection on switch outport %d:%d -> %d - port already occupied",
                      addr(), src_outport, dst_inport);
  }
  p->link = std::move(link);
  p->byte_delay = TimeDelta(1.0/port_bw);
  p->dst_port = dst_inport;
  p->scaleBuffers(scale_factor);
  switch_debug("connecting output port %d to input port %d with scale=%10.4f byte_delay=%10.5e",
               src_outport, dst_inport, scale_factor, p->byte_delay.sec());
}

void
SnapprSwitch::connectInput(int src_outport, int dst_inport, EventLink::ptr&& link)
{
  switch_debug("connecting input port %d to output port %d", dst_inport, src_outport);
  auto& port = inports_[dst_inport];
  if (port.link){
    spkt_abort_printf("Bad connection on switch inport %d:%d -> %d - port already occupied",
                      addr(), dst_inport, src_outport);
  }
  port.src_outport = src_outport;
  port.link = std::move(link);
  port.parent = this;
}

int
SnapprSwitch::queueLength(int port, int  /*vc*/) const
{
  return outports_[port]->queueLength();
}


void
SnapprSwitch::deadlockCheck()
{
  for (int vl=0; vl < num_vl_; ++vl){
    deadlockCheck(vl);
  }
}

void
SnapprSwitch::deadlockCheck(int vl)
{
  for (auto* p : outports_){
    p->deadlockCheck(vl);
  }
}

void
SnapprSwitch::handlePayload(SnapprPacket* pkt, int inport)
{
  if (pkt->deadlocked()){
    std::cerr << "Switch " << addr() << " is part of deadlock on inport" << std::endl;
    deadlockCheck(pkt->virtualLane());
    return;
  }

  pkt->setInport(inport);
  pkt->saveInputVirtualLane();
  Router* rtr = routers_[pkt->qos()];
  rtr->route(pkt);
  int vl = rtr->vlOffset() + pkt->deadlockVC();
  pkt->setVirtualLane(vl);

  SnapprOutPort* p = outports_[pkt->nextPort()];
  TimeDelta time_to_send = p->byte_delay * pkt->numBytes();
  /** I am processing the head flit - so I assume compatibility with wormhole routing
    The tail flit cannot leave THIS switch prior to its departure time in the prev switch */
  if (pkt->timeToSend() > time_to_send){
    //delay the packet
    auto ev = newCallback(p, &SnapprOutPort::tryToSendPacket, pkt);
    TimeDelta delta_t = pkt->timeToSend() - time_to_send;
    sendDelayedExecutionEvent(delta_t, ev);
  } else {
    p->tryToSendPacket(pkt);
  }
}

std::string
SnapprSwitch::toString() const
{
  return sprockit::sprintf("snappr switch %d", int(my_addr_));
}

LinkHandler*
SnapprSwitch::creditHandler(int port)
{
  switch_debug("returning credit handler on output port %d", port);
  return newLinkHandler(outports_[port], &SnapprOutPort::handle);
}

LinkHandler*
SnapprSwitch::payloadHandler(int port)
{
  switch_debug("returning payload handler on input port %d", port);
  return newLinkHandler(&inports_[port], &SnapprInPort::handle);
}




}
}
