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

#include <sstmac/hardware/pisces/pisces_crossbar.h>
#include <sstmac/hardware/pisces/pisces_stats.h>
#include <sstmac/hardware/pisces/pisces_tiled_switch.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/common/stats/stat_global_int.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/runtime.h>
#include <iomanip>
#include <sprockit/util.h>

RegisterNamespaces("bytes_sent");

namespace sstmac {
namespace hw {

PiscesCrossbar::PiscesCrossbar(
  const std::string& arb, double bw,
  SST::Component* parent,
  int num_in_ports, int num_out_ports, int num_vc,
  bool update_vc) :
  PiscesNtoMQueue(arb, bw, parent, num_in_ports, num_out_ports,
                    num_vc, update_vc)
{
}

PiscesDemuxer::PiscesDemuxer(
  const std::string& arb, double bw,
  SST::Component* parent, int num_out_ports, int num_vc,
  bool update_vc) :
  PiscesNtoMQueue(arb, bw, parent, 1, num_out_ports,
                    num_vc, update_vc)
{
}

PiscesMuxer::PiscesMuxer(
  const std::string& arb, double bw,
  SST::Component* parent, int num_in_ports, int num_vc,
  bool update_vc) :
  PiscesNtoMQueue(arb, bw, parent, num_in_ports, 1,
                    num_vc, update_vc)
{
}

PiscesNtoMQueue::
PiscesNtoMQueue(const std::string& arb, double bw, SST::Component* parent,
                int num_in_ports, int num_out_ports, int num_vc,
                bool update_vc)
  : PiscesSender(parent, update_vc),
    num_vc_(num_vc),
    creditHandler_(nullptr),
    payloadHandler_(nullptr),
#if SSTMAC_SANITY_CHECK
    initial_credits_(num_vc * num_out_ports),
#endif
    queues_(num_vc * num_out_ports),
    credits_(num_vc * num_out_ports),
    inputs_(num_in_ports),
    outputs_(num_out_ports)

{
  arb_ = PiscesBandwidthArbitrator::factory::get_value(arb, bw);
}

EventHandler*
PiscesNtoMQueue::creditHandler()
{
  if (!creditHandler_){
    creditHandler_ = newHandler(this, &PiscesNtoMQueue::handleCredit);
  }
  return creditHandler_;
}

EventHandler*
PiscesNtoMQueue::payloadHandler()
{
  if (!payloadHandler_){
    payloadHandler_ = newHandler(this, &PiscesNtoMQueue::handlePayload);
  }
  return payloadHandler_;
}

PiscesNtoMQueue::~PiscesNtoMQueue()
{
  if (arb_) delete arb_;
  if (creditHandler_) delete creditHandler_;
  for (auto& inp : inputs_){
    if (inp.link) delete inp.link;
  }
  for (auto& out : outputs_){
    if (out.link) delete out.link;
  }
}

std::string
PiscesNtoMQueue::inputName(PiscesPacket* pkt)
{
  EventLink* link = inputs_[pkt->nextLocalInport()].link;
  return link->toString();
}

EventLink*
PiscesNtoMQueue::outputLink(PiscesPacket* pkt)
{
  return outputs_[pkt->nextLocalOutport()].link;
}

std::string
PiscesNtoMQueue::outputName(PiscesPacket* pkt)
{
  return outputLink(pkt)->toString();
}

void
PiscesNtoMQueue::sendPayload(PiscesPacket* pkt)
{
#if SSTMAC_SANITY_CHECK
  int port = pkt->nextLocalOutport();
  if (port >= outputs_.size() || outputs_[port].link == nullptr){
    spkt_abort_printf("got bad outport %d on stage %d", port, int(pkt->stage()));
  }
  port = pkt->nextLocalInport();
  if (port >= inputs_.size() || inputs_[port].link == nullptr){
    spkt_abort_printf("got bad inport %d on stage %d", port, int(pkt->stage()));
  }
#endif
  send(arb_, pkt, inputs_[pkt->nextLocalInport()], outputs_[pkt->nextLocalOutport()]);
}

void
PiscesNtoMQueue::handleCredit(Event *ev)
{
  PiscesCredit* pkt = static_cast<PiscesCredit*>(ev);
  int channel = pkt->port() * num_vc_ + pkt->vc();

#if SSTMAC_SANITY_CHECK
  if (slot(pkt->port(),pkt->vc()) >= credits_.size()){
    spkt_abort_printf("bad port/vc on credit -> port=%d vc=%d",
                      pkt->port(), pkt->vc());
  }
#endif

  int& num_credits = credit(pkt->port(), pkt->vc());

  pisces_debug(
    "On %s:%p with %d credits, handling credit for local port:%d vc:%d channel:%d",
     toString().c_str(), this,
     num_credits, pkt->port(), pkt->vc(), channel);

  num_credits += pkt->numCredits();

#if SSTMAC_SANITY_CHECK
  if (num_credits > initial_credits_[slot(pkt->port(),pkt->vc())]){
    spkt_abort_printf("initial credits exceeded");
  }
#endif

  PiscesPacket* payload = queue(pkt->port(), pkt->vc()).pop(num_credits);
  if (payload) {
    num_credits -= payload->numBytes();
    sendPayload(payload);
  }
  delete pkt;
}

void
PiscesNtoMQueue::handlePayload(Event* ev)
{
  auto pkt = static_cast<PiscesPacket*>(ev);
  pkt->setArrival(now());

  int dst_vc = update_vc_ ? pkt->nextVC() : pkt->vc();
  int loc_port = pkt->nextLocalOutport();
  pisces_debug(
   "On %s:%p, handling payload {%s} for vc:%d local_port:%d on stage %d",
    toString().c_str(), this,
    pkt->toString().c_str(), dst_vc, loc_port, int(pkt->stage()));

  if (dst_vc < 0 || loc_port < 0){
    spkt_abort_printf("On %s handling {%s}, got negative vc,local_port %d,%d",
        toString().c_str(), pkt->toString().c_str(), loc_port, dst_vc);
  }

  int& num_credits = credit(loc_port, dst_vc);
   pisces_debug(
    "On %s:%p with %d credits, handling {%s} for local port:%d vc:%d",
     toString().c_str(), this,
     num_credits,
     pkt->toString().c_str(),
     loc_port, dst_vc);

  if (num_credits >= pkt->numBytes()) {
    num_credits -= pkt->numBytes();
    sendPayload(pkt);
  } else {
    pisces_debug(
      "On %s:%p, pushing back %s on queue %d=(%d,%d) for nq=%d nvc=%d",
      toString().c_str(), this, pkt->toString().c_str(),
      slot(loc_port, dst_vc), loc_port, dst_vc, queues_.size(), num_vc_);
    queue(loc_port, dst_vc).push_back(pkt);
  }
}

void
PiscesNtoMQueue::resizeOutports(int num_ports)
{
  outputs_.resize(num_ports);
  queues_.resize(num_ports*num_vc_);
  credits_.resize(num_ports*num_vc_);
#if SSTMAC_SANITY_CHECK
  initial_credits_.resize(num_ports*num_vc_);
#endif
}

void
PiscesNtoMQueue::setInput(
  int my_inport, int src_outport,
  EventLink* link)
{
  // ports are local for local links and global otherwise

  debug_printf(sprockit::dbg::pisces_config,
    "On %s:%d setting input %s:%d",
    toString().c_str(), my_inport,
    link ? link->toString().c_str() : "null", src_outport);
  Input& inp = inputs_[my_inport];
  inp.link = link;
  inp.port_to_credit = src_outport;
}

void
PiscesNtoMQueue::setOutput(int my_outport, int dst_inport, EventLink* link, int num_credits)
{
  // must be called with local my_outport (if there's a difference)
  // global port numbers aren't unique for individual elements of
  // a tiled switch, for instance, so this is the only logical approach

  // dst_inport is local for local links and global otherwise

  debug_printf(sprockit::dbg::pisces_config,
    "On %s setting output %s:%d for local port %d of %d",
    toString().c_str(),
    link->toString().c_str(), dst_inport,
    outputs_.size());

  if (my_outport > outputs_.size()) {
    spkt_throw_printf(sprockit::value_error,
                      "PiscesCrossbar: my_outport %i > outputs_.size() %i",
                      my_outport, outputs_.size());
  }
  Output& out = outputs_[my_outport];
  out.link = link;
  out.arrival_port = dst_inport;

  int num_credits_per_vc = num_credits / num_vc_;
  for (int i=0; i < num_vc_; ++i) {
    debug_printf(sprockit::dbg::pisces_config,
                 "On %s:%p, initing %d credits on port:%d vc:%d",
                 toString().c_str(), this,
                 num_credits_per_vc,
                 my_outport, i);
    credit(my_outport, i) = num_credits_per_vc;
#if SSTMAC_SANITY_CHECK
    initial_credits_[slot(my_outport,i)] = num_credits_per_vc;
#endif
  }
}


}
}
