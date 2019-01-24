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

#include <sstmac/hardware/pisces/pisces_sender.h>
#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/output.h>
#include <sprockit/util.h>

MakeDebugSlot(pisces_timeline)

static int num_sends = 0;
static int num_credits = 0;

namespace sstmac {
namespace hw {

PiscesPacket*
payload_queue::pop(int num_credits)
{
  auto it = queue.begin(), end = queue.end();
  for (; it != end; ++it){
    PiscesPacket* pkt = *it;
    if (pkt->numBytes() <= num_credits){
      queue.erase(it);
      return pkt;
    }
  }
  return nullptr;
}

PiscesSender::PiscesSender(
  SST::Params& params,
  SST::Component* parent,
  bool update_vc) :
  SubComponent(parent), //no self handlers
  stat_collector_(nullptr),
  update_vc_(update_vc)
{
  send_lat_ = Timestamp(params.findUnits("sendLatency").toDouble());
  credit_lat_ = Timestamp(params.findUnits("creditLatency").toDouble());
}

void
PiscesSender::configurePayloadPortLatency(SST::Params& params)
{
  if (!params.contains("sendLatency")){
    params.insert("sendLatency", params.find<std::string>("latency"));
  }
  if (!params.contains("creditLatency")){
    params.insert("creditLatency", "0ns");
  }
}

void
PiscesSender::configureCreditPortLatency(SST::Params& params)
{
  if (!params.contains("sendLatency")){
    params.insert("sendLatency", "0ns");
  }
  if (!params.contains("creditLatency")){
    params.insert("creditLatency", params.find<std::string>("latency"));
  }
}

void
PiscesSender::sendCredit(
  Input& inp, PiscesPacket* payload,
  GlobalTimestamp credits_ready)
{
  int src_vc = payload->vc(); //we have not updated to the new virtual channel
  PiscesCredit* credit = new PiscesCredit(inp.port_to_credit,
                                            src_vc, payload->numBytes());

  Timestamp credit_departure_delay = credits_ready - now();
  if (credit_departure_delay <= credit_lat_){
    credit_departure_delay = Timestamp();
  } else {
    //assume credits pipeline to arrive exactly when ready
    credit_departure_delay -= credit_lat_;
  }

  pisces_debug(
      "On %s:%p on inport %d, crediting %s:%p port:%d:%d vc:%d {%s}"
      "after delay %9.5e after latency %9.5e with %p",
      toString().c_str(), this, int(payload->nextLocalInport()),
      inp.link->toString().c_str(), inp.link,
      payload->edgeOutport(), payload->nextLocalOutport(), src_vc,
      payload->toString().c_str(),
      credit_departure_delay.sec(), credit_lat_.sec(),
      credit);
  //simulate more realistic pipelining of credits
  inp.link->send(credit_departure_delay, credit);
}

GlobalTimestamp
PiscesSender::send(
  PiscesBandwidthArbitrator* arb,
  PiscesPacket* pkt,
  Input& to_credit, Output& to_send)
{
  GlobalTimestamp now_ = now();
  pkt_arbitration_t st;
  st.incoming_byte_delay = pkt->byteDelay();
  st.now = now_;
  st.pkt = pkt;
  st.src_outport = pkt->nextLocalInport();
  st.dst_inport = pkt->nextLocalInport();

  if (arb) {
    arb->arbitrate(st);
  } else {
    st.head_leaves = st.tail_leaves = st.credit_leaves = now_;
  }

  if (stat_collector_) stat_collector_->collectSingleEvent(st);

  if (to_credit.link) {
    sendCredit(to_credit, pkt, st.credit_leaves);
  } else {
    pisces_debug("On %s:%p no link to credit for port:%d vc:%d -> %s",
                 toString().c_str(), this, pkt->nextLocalInport(), pkt->nextVC(),
                 pkt->toString().c_str());
  }

  pisces_debug(
    "On %s:%p, sending on local port:%d vc:%d {%s} to handler %s:%p on inport %d",
    toString().c_str(), this,
    pkt->nextLocalOutport(), pkt->nextVC(),
    pkt->toString().c_str(),
    to_send.link->toString().c_str(), to_send.link,
    pkt->nextLocalInport());

  if (pkt->nextVC() < 0){
    spkt_abort_printf("packet VC did not get set before sending: %s",
                      pkt->toString().c_str());
  }

  //weird hack to update vc from routing
  if (update_vc_) pkt->updateVC();
  pkt->advanceStage();

  Timestamp departure_delay = st.head_leaves - now_;
  to_send.link->send(departure_delay, pkt);

  return st.tail_leaves;
}

std::string
PiscesSender::toString() const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return piscesName();
#else
  return piscesName() + Topology::global()->label(componentId());
#endif
}

}
}
