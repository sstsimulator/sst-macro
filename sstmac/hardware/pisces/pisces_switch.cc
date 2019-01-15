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
#include <sstmac/hardware/pisces/pisces_stats.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sstmac/common/stats/stat_global_int.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/hardware/topology/topology.h>

RegisterNamespaces("switch", "router", "congestion_stats", "xbar", "link",
                   "output_buffer");

RegisterKeywords(
{ "stats", "specify the statistics collection to be performed on this switch" },
{ "latency", "latency to traverse a portion of the switch - sets both credit/send" },
{ "sendLatency", "latency to send data to the next stage" },
{ "creditLatency", "latency to send credit to the previous stage" },
{ "credits", "the number of initial credits available to switch component" },
{ "num_vc", "the number of virtual channels a switch must allow" },
);


namespace sstmac {
namespace hw {

PiscesAbstractSwitch::PiscesAbstractSwitch(sprockit::sim_parameters::ptr& params, uint32_t id) :
  buf_stats_(nullptr),
  xbar_stats_(nullptr),
  router_(nullptr),
  NetworkSwitch(params, id)
{
  sprockit::sim_parameters::ptr xbar_params = params->get_optional_namespace("xbar");
  xbar_stats_ = PacketStatsCallback::factory::get_optional_param("stats", "null",
                                             xbar_params, this);

  sprockit::sim_parameters::ptr buf_params = params->get_optional_namespace("output_buffer");
  buf_stats_ = PacketStatsCallback::factory::get_optional_param("stats", "null",
                                             buf_params, this);

  sprockit::sim_parameters::ptr rtr_params = params->get_optional_namespace("router");
  rtr_params->add_param_override_recursive("id", int(my_addr_));
  router_ = Router::factory::get_param("name", rtr_params, top_, this);

  sprockit::sim_parameters::ptr ej_params = params->get_optional_namespace("ejection");
  std::vector<Topology::injection_port> conns;
  top_->endpointsConnectedToEjectionSwitch(my_addr_, conns);
  if (!ej_params->has_param("credits")){
    //never be limited by credits
    ej_params->add_param_override("credits", "1GB");
  }

  PiscesSender::configurePayloadPortLatency(ej_params);

  for (Topology::injection_port& conn : conns){
    auto port_ns = Topology::getPortNamespace(conn.switch_port);
    sprockit::sim_parameters::ptr port_params = params->get_optional_namespace(port_ns);
    ej_params->combine_into(port_params);
  }
}


PiscesAbstractSwitch::~PiscesAbstractSwitch()
{
  if (buf_stats_) delete buf_stats_;
  if (xbar_stats_) delete xbar_stats_;
  if (router_) delete router_;
}

PiscesSwitch::PiscesSwitch(sprockit::sim_parameters::ptr& params, uint32_t id)
: PiscesAbstractSwitch(params, id),
  xbar_(nullptr)
{
  sprockit::sim_parameters::ptr xbar_params = params->get_namespace("xbar");
  xbar_params->add_param_override("num_vc", router_->numVC());
  xbar_ = new PiscesCrossbar(xbar_params, this,
                              top_->maxNumPorts(), top_->maxNumPorts(),
                              router_->numVC(), true/*yes, update vc*/);
  xbar_->setStatCollector(xbar_stats_);
  out_buffers_.resize(top_->maxNumPorts());
  inports_.resize(top_->maxNumPorts());
  for (int i=0; i < inports_.size(); ++i){
    input_port& inp = inports_[i];
    inp.port = i;
    inp.parent = this;
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
  sprockit::sim_parameters::ptr& port_params,
  int src_outport,
  int dst_inport,
  EventLink* link)
{
  PiscesBuffer* out_buffer = new PiscesBuffer(port_params, this, router_->numVC());
  out_buffer->setStatCollector(buf_stats_);
  int buffer_inport = 0;
  auto out_link = allocateSubLink(xbar_->sendLatency(), this,
                newHandler(out_buffer, &PiscesBuffer::handlePayload));
  xbar_->setOutput(port_params, src_outport, buffer_inport, out_link);
  auto in_link = allocateSubLink(out_buffer->creditLatency(), this, xbar_->creditHandler());
  out_buffer->setInput(port_params, buffer_inport, src_outport, in_link);
  out_buffers_[src_outport] = out_buffer;


  out_buffer->setOutput(port_params, src_outport, dst_inport, link);
  out_buffers_[src_outport] = out_buffer;
}

void
PiscesSwitch::input_port::handle(Event *ev)
{
  PiscesPacket* payload = static_cast<PiscesPacket*>(ev);
  parent->router()->route(payload);
  payload->resetStages(payload->edgeOutport(), 0);
  payload->setInport(this->port);
  parent->xbar()->handlePayload(payload);
}

void
PiscesSwitch::connectInput(
  sprockit::sim_parameters::ptr& port_params,
  int src_outport,
  int dst_inport,
  EventLink* link)
{
  int buffer_port = 0;
  xbar_->setInput(port_params, dst_inport, buffer_port, link);
}

Timestamp
PiscesSwitch::sendLatency(sprockit::sim_parameters::ptr& params) const
{
  return params->get_time_param("sendLatency");
}

Timestamp
PiscesSwitch::creditLatency(sprockit::sim_parameters::ptr& params) const
{
  return params->get_namespace("xbar")->get_time_param("creditLatency");
}

int
PiscesSwitch::queueLength(int port) const
{
  PiscesBuffer* buf = static_cast<PiscesBuffer*>(out_buffers_[port]);
  return buf->queueLength();
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
  input_port* inp = &inports_[port];
  return newLinkHandler(inp, &input_port::handle);
}

}
}
