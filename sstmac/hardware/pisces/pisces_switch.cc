/**
Copyright 2009-2022 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2022, NTESS

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
  NetworkSwitch(id, params),
  router_(nullptr)
{
  SST::Params rtr_params = params.get_scoped_params("router");
  rtr_params.insert("id", std::to_string(my_addr_));
  router_ = sprockit::create<Router>(
     "macro", rtr_params.find<std::string>("name"), rtr_params, top_, this);
}


PiscesAbstractSwitch::~PiscesAbstractSwitch()
{
  if (router_) delete router_;
}

PiscesSwitch::PiscesSwitch(uint32_t id, SST::Params& params)
: PiscesAbstractSwitch(id, params),
  xbar_(nullptr)
{
  mtu_ = params.find<SST::UnitAlgebra>("mtu").getRoundedValue();

  SST::Params xbar_params = params.get_scoped_params("xbar");
  SST::Params link_params = params.get_scoped_params("link");

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

  xbar_ = new PiscesCrossbar("xbar", componentId(), xbar_arb, xbar_bw, this,
                             top_->maxNumPorts(), top_->maxNumPorts(),
                             router_->numVC(), true/*yes, update vc*/);
  int num_ports = top_->maxNumPorts();
  out_buffers_.resize(num_ports);
  for (int src_outport=0; src_outport < num_ports; ++src_outport){
    double scale_factor = top_->portScaleFactor(my_addr_, src_outport);
    std::string bufname = sprockit::sprintf(
        "%s:port%d",top_->switchIdToName(my_addr_).c_str(), src_outport);
    PiscesBuffer* out_buffer = new PiscesBuffer(link_params, bufname, componentId(),
              arbType_, link_bw_ * scale_factor, mtu_, this, router_->numVC());
    out_buffers_[src_outport] = out_buffer;
  }



  inports_.resize(top_->maxNumPorts());
  for (int i=0; i < inports_.size(); ++i){
    InputPort& inp = inports_[i];
    inp.port = i;
    inp.parent = this;
    std::string subname = sprockit::sprintf("%s:port%d",
             top_->switchIdToName(my_addr_).c_str(), inp.port);
    inp.recv_bytes_ = registerStatistic<uint64_t>("recv_bytes", subname);
  }

  if (link_credits_ < mtu_){
    spkt_abort_printf("MTU %d is larger than credits %d", mtu_, link_credits_);
  }

  configureLinks();
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
  EventLink::ptr&& link)
{
  auto* out_buffer = out_buffers_[src_outport];
  double scale_factor = top_->portScaleFactor(my_addr_, src_outport);
  int buffer_inport = 0;
  std::string out_port_name = sprockit::sprintf("buffer-out%d", src_outport);
  auto out_link = allocateSubLink(out_port_name, TimeDelta(), //don't put latency on xbar
                    newLinkHandler(out_buffer, &PiscesBuffer::handlePayload));
  xbar_->setOutput(src_outport, buffer_inport, std::move(out_link), link_credits_ * scale_factor);

  std::string in_port_name = sprockit::sprintf("xbar-credit%d", src_outport);
  auto in_link = allocateSubLink(in_port_name, TimeDelta(), //don't put latency on internal credits
                                 xbar_->creditHandler());
  out_buffer->setInput(buffer_inport, src_outport, std::move(in_link));
  out_buffers_[src_outport] = out_buffer;

  out_buffer->setOutput(src_outport, dst_inport, std::move(link), link_credits_ * scale_factor);
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
  recv_bytes_->addData(payload->byteLength());
}

void
PiscesSwitch::connectInput(int  /*src_outport*/, int dst_inport, EventLink::ptr&& link)
{
  int buffer_port = 0;
  xbar_->setInput(dst_inport, buffer_port, std::move(link));
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
  return sprockit::sprintf("packet flow switch %d", int(my_addr_));
}

void
PiscesSwitch::setup()
{
  PiscesAbstractSwitch::setup();
}

void
PiscesSwitch::init(unsigned int /*phase*/)
{
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
