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

#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/pisces/pisces_switch.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/hardware/topology/topology.h>

RegisterNamespaces("switch", "router", "congestion_stats", "xbar", "link",
                   "output_buffer");

RegisterKeywords(
{ "stats", "specify the statistics collection to be performed on this switch" },
{ "latency", "latency to traverse a portion of the switch - sets both credit/send" },
{ "credits", "the number of initial credits available to switch component" },
{ "num_vc", "the number of virtual channels a switch must allow" },
);


namespace sstmac {
namespace hw {

PiscesAbstractSwitch::PiscesAbstractSwitch(uint32_t id, SST::Params& params) :
  router_(nullptr),
  NetworkSwitch(id, params)
{
  SST::Params xbar_params = params.find_prefix_params("xbar");

  SST::Params buf_params = params.find_prefix_params("output_buffer");

  SST::Params rtr_params = params.find_prefix_params("router");
  rtr_params.insert("id", std::to_string(my_addr_));
  router_ = sprockit::create<Router>(
     "macro", rtr_params.find<std::string>("name"), rtr_params, top_, this);

  SST::Params ej_params = params.find_prefix_params("ejection");
  std::vector<Topology::InjectionPort> conns;
  top_->endpointsConnectedToEjectionSwitch(my_addr_, conns);
  if (!ej_params.contains("credits")){
    //never be limited by credits
    ej_params.insert("credits", "1GB");
  }
}


PiscesAbstractSwitch::~PiscesAbstractSwitch()
{
  if (router_) delete router_;
}

PiscesSwitch::PiscesSwitch(uint32_t id, SST::Params& params)
: PiscesAbstractSwitch(id, params),
  xbar_(nullptr)
{
  SST::Params xbar_params = params.find_prefix_params("xbar");
  SST::Params link_params = params.find_prefix_params("link");

  if (params.contains("arbitrator")){
    arbType_ = params.find<std::string>("arbitrator");
  } else {
    arbType_ = link_params.find<std::string>("arbitrator");
  }

  double xbar_bw = xbar_params.find<SST::UnitAlgebra>("bandwidth").getValue().toDouble();

  std::string xbar_arb = xbar_params.find<std::string>("arbitrator", arbType_);

  link_bw_ = link_params.find<SST::UnitAlgebra>("bandwidth").getValue().toDouble();
  if (link_params.contains("credits")){
    link_credits_ = link_params.find<SST::UnitAlgebra>("credits").getRoundedValue();
  } else {
    double lat_s = link_params.find<SST::UnitAlgebra>("latency").getValue().toDouble();
    //use 4*RTT as buffer size
    link_credits_ = 8*link_bw_*lat_s;
  }

  if (xbar_params.contains("credits")){
    xbar_credits_ = xbar_params.find<SST::UnitAlgebra>("credits").getRoundedValue();
  } else {
    xbar_credits_ = link_credits_;
  }

  xbar_ = new PiscesCrossbar("xbar", xbar_arb, xbar_bw, this,
                             top_->maxNumPorts(), top_->maxNumPorts(),
                             router_->numVC(), true/*yes, update vc*/);
  out_buffers_.resize(top_->maxNumPorts());
  inports_.resize(top_->maxNumPorts());
  for (int i=0; i < inports_.size(); ++i){
    InputPort& inp = inports_[i];
    inp.port = i;
    inp.parent = this;
  }

  mtu_ = params.find<SST::UnitAlgebra>("mtu").getRoundedValue();

  if (link_credits_ < mtu_){
    spkt_abort_printf("MTU %d is larger than credits %d", mtu_, link_credits_);
  }

  initLinks(params);
}

PiscesSwitch::~PiscesSwitch()
{
  if (xbar_) delete xbar_;
  int nbuffers = out_buffers_.size();
  for (int i=0; i < nbuffers; ++i){
    auto* buf = out_buffers_[i];
    if (buf) delete buf;
  }
}

void
PiscesSwitch::connectOutput(
  int src_outport,
  int dst_inport,
  EventLink* link)
{
  double scale_factor = top_->portScaleFactor(my_addr_, src_outport);

  PiscesBuffer* out_buffer = new PiscesBuffer(sprockit::printf("buffer%d", src_outport),
                                              arbType_, link_bw_ * scale_factor, mtu_,
                                              this, router_->numVC());
  int buffer_inport = 0;
  std::string out_port_name = sprockit::printf("buffer-out%d", src_outport);
  auto out_link = allocateSubLink(out_port_name, Timestamp(), this, //don't put latency on xbar
                newLinkHandler(out_buffer, &PiscesBuffer::handlePayload));
  xbar_->setOutput(src_outport, buffer_inport, out_link, link_credits_ * scale_factor);

  std::string in_port_name = sprockit::printf("xbar-credit%d", src_outport);
  auto in_link = allocateSubLink(in_port_name, Timestamp(), this, xbar_->creditHandler()); //don't put latency on internal credits
  out_buffer->setInput(buffer_inport, src_outport, in_link);
  out_buffers_[src_outport] = out_buffer;

  out_buffer->setOutput(src_outport, dst_inport, link, link_credits_ * scale_factor);
  out_buffers_[src_outport] = out_buffer;
}

void
PiscesSwitch::InputPort::handle(Event *ev)
{
  PiscesPacket* payload = static_cast<PiscesPacket*>(ev);
  parent->router()->route(payload);
  payload->resetStages(payload->edgeOutport(), 0);
  payload->setInport(this->port);
  parent->xbar()->handlePayload(payload);
}

void
PiscesSwitch::connectInput(int src_outport, int dst_inport, EventLink* link)
{
  int buffer_port = 0;
  xbar_->setInput(dst_inport, buffer_port, link);
}

int
PiscesSwitch::queueLength(int port, int vc) const
{
  PiscesBuffer* buf = static_cast<PiscesBuffer*>(out_buffers_[port]);
  return buf->queueLength(vc);
}

std::string
PiscesSwitch::toString() const
{
  return sprockit::printf("packet flow switch %d", int(my_addr_));
}

void
PiscesSwitch::setup()
{
  for (auto* buf : out_buffers_){
    if (buf) buf->setup();
  }
  xbar_->setup();
  PiscesAbstractSwitch::setup();
}

void
PiscesSwitch::init(unsigned int phase)
{
  for (auto* buf : out_buffers_){
    if (buf) buf->init(phase);
  }
  xbar_->init(phase);
}

LinkHandler*
PiscesSwitch::creditHandler(int port)
{
  if (port >= out_buffers_.size()){
    spkt_abort_printf("Got invalid port %d request for credit handler - max is %d",
                      port, out_buffers_.size() - 1);
  }
  return newLinkHandler(out_buffers_[port], &PiscesSender::handleCredit);
}

LinkHandler*
PiscesSwitch::payloadHandler(int port)
{
  InputPort* inp = &inports_[port];
  return newLinkHandler(inp, &InputPort::handle);
}

}
}
