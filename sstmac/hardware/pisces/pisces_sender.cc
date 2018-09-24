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
#include <sstmac/hardware/router/valiant_routing.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/output.h>
#include <sprockit/util.h>

MakeDebugSlot(pisces_timeline)

namespace sstmac {
namespace hw {

pisces_payload*
payload_queue::front()
{
  if (queue.empty()){
    return NULL;
  }

  return queue.front();
}

void
payload_queue::push_back(pisces_payload *payload)
{
  queue.push_back(payload);
}

pisces_payload*
payload_queue::pop(int num_credits)
{
  auto it = queue.begin(), end = queue.end();
  for (; it != end; ++it){
    pisces_payload* pkt = *it;
    if (pkt->num_bytes() <= num_credits){
      queue.erase(it);
      return pkt;
    }
  }
  return nullptr;
}

pisces_sender::pisces_sender(
  sprockit::sim_parameters* params,
  event_scheduler* parent) :
  event_subcomponent(parent), //no self handlers
  stat_collector_(nullptr),
  update_vc_(true)
{
  send_lat_ = params->get_time_param("send_latency");
  credit_lat_ = params->get_time_param("credit_latency");
}

void
pisces_sender::configure_payload_port_latency(sprockit::sim_parameters* params)
{
  if (!params->has_param("send_latency")){
    params->add_param_override("send_latency", params->get_param("latency"));
  }
  if (!params->has_param("credit_latency")){
    params->add_param_override("credit_latency", "0ns");
  }
}

void
pisces_sender::configure_credit_port_latency(sprockit::sim_parameters* params)
{
  if (!params->has_param("send_latency")){
    params->add_param_override("send_latency", "0ns");
  }
  if (!params->has_param("credit_latency")){
    params->add_param_override("credit_latency", params->get_param("latency"));
  }
}

void
pisces_sender::send_credit(
  const pisces_input& src,
  pisces_payload* payload,
  timestamp credits_ready)
{
  int src_vc = payload->vc(); //we have not updated to the new virtual channel
  pisces_credit* credit = new pisces_credit(src.src_outport,
                                   src_vc, payload->num_bytes());
  //there is a certain minimum latency on credits
  timestamp now_ = now();
  timestamp credit_departure_delay = credits_ready - now_;
  if (credit_departure_delay < credit_lat_){
    credit_departure_delay = timestamp();
  } else {
    //assume credits pipeline to arrive exactly when ready
    credit_departure_delay -= credit_lat_;
  }
  pisces_debug(
      "On %s:%p on inport %d, crediting %s:%p port:%d vc:%d {%s} after delay %9.5e after latency %9.5e with %p",
      to_string().c_str(), this, payload->inport(),
      src.link->to_string().c_str(), src.link,
      src.src_outport, src_vc,
      payload->to_string().c_str(),
      credit_departure_delay.sec(), credit_lat_.sec(),
      credit);
  //simulate more realistic pipelining of credits
  src.link->validate_latency(credit_lat_);
  src.link->send_extra_delay(credit_departure_delay, credit);
}

void
pisces_sender::send(
  pisces_bandwidth_arbitrator* arb,
  pisces_payload* pkt,
  const pisces_input& src,
  const pisces_output& dest)
{
  timestamp now_ = now();
  pkt_arbitration_t st;
  st.incoming_bw = pkt->bw();
  st.now = now_;
  st.pkt = pkt;
  st.src_outport = src.src_outport;
  st.dst_inport = dest.dst_inport;

  if (arb) {
    arb->arbitrate(st);
  } else {
    st.head_leaves = st.tail_leaves = st.credit_leaves = now_;
  }

  if (stat_collector_) stat_collector_->collect_single_event(st);

#if SSTMAC_SANITY_CHECK
  if (pkt->bw() <= 0 && pkt->bw() != pisces_payload::uninitialized_bw) {
    spkt_throw_printf(sprockit::value_error,
                     "On %s, got negative bandwidth for msg %s",
                     to_string().c_str(),
                     pkt->to_string().c_str());
  }
#endif

  if (src.link) {
    send_credit(src, pkt, st.credit_leaves);
  }

  pisces_debug(
    "On %s:%p, sending on local port:%d vc:%d {%s} to handler %s:%p on "
    "inport %d at head_leaves=%9.5e tail_leaves=%9.5e",
    to_string().c_str(), this,
    pkt->local_outport(), pkt->next_vc(),
    pkt->to_string().c_str(),
    dest.link->to_string().c_str(), dest.link,
    dest.dst_inport,
    st.head_leaves.sec(),
    st.tail_leaves.sec());

  if (pkt->next_vc() < 0){
    spkt_abort_printf("packet VC did not get set before sending: %s",
                      pkt->to_string().c_str());
  }

  // leaving me, on arrival at dest message will occupy a specific inport at dest
  pkt->set_inport(dest.dst_inport);
  //weird hack to update vc from routing
  if (update_vc_) pkt->update_vc();

  timestamp departure_delay = st.head_leaves - now_;
  dest.link->validate_latency(send_lat_);
  dest.link->send_extra_delay(departure_delay, pkt);
}

std::string
pisces_sender::to_string() const
{
  return pisces_name() + topology::global()->label(component_id());
}

}
}
