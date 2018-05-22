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

#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/sculpin/sculpin_nic.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/event_callback.h>
#include <sprockit/errors.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

#include <stddef.h>

#define pkt_debug(...) \
  debug_printf(sprockit::dbg::sculpin, "sculpin NIC %d: %s", \
    int(addr()), sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

sculpin_nic::sculpin_nic(sprockit::sim_parameters* params, node* parent) :
  nic(params, parent)
{
  sprockit::sim_parameters* inj_params = params->get_namespace("injection");

  packet_size_ = inj_params->get_byte_length_param("mtu");
  double inj_bw = inj_params->get_bandwidth_param("bandwidth");
  inj_inv_bw_ = 1.0/inj_bw;

  //make port 0 a copy of the injection params
  sprockit::sim_parameters* port0_params = params->get_optional_namespace("port0");
  inj_params->combine_into(port0_params);

#if !SSTMAC_INTEGRATED_SST_CORE
  ack_handler_ = new_handler(this, &sculpin_nic::handle_credit);
  payload_handler_ = new_handler(this, &sculpin_nic::handle_payload);
#endif
}

timestamp
sculpin_nic::send_latency(sprockit::sim_parameters *params) const
{
  return params->get_namespace("injection")->get_time_param("send_latency");
}

timestamp
sculpin_nic::credit_latency(sprockit::sim_parameters *params) const
{
  return params->get_namespace("injection")->get_time_param("send_latency");
}

void
sculpin_nic::init(unsigned int phase)
{
}

void
sculpin_nic::setup()
{
}

sculpin_nic::~sculpin_nic() throw ()
{
#if !SSTMAC_INTEGRATED_SST_CORE
  delete ack_handler_;
  delete payload_handler_;
#endif
}

link_handler*
sculpin_nic::payload_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  if (port == nic::LogP){
    return new_link_handler(this, &nic::mtl_handle);
  } else {
    return new_link_handler(this, &sculpin_nic::handle_payload);
  }
#else
  if (port == nic::LogP){
    return link_mtl_handler_;
  } else {
    return payload_handler_;
  }
#endif
}

link_handler*
sculpin_nic::credit_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new_link_handler(this, &sculpin_nic::handle_credit);
#else
  return ack_handler_;
#endif
}

void
sculpin_nic::connect_output(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_link* link)
{
  if (src_outport == Injection){
    inj_link_ = link;
#if SSTMAC_INTEGRATED_SST_CORE
  } else if (src_outport == LogP) {
    logp_switch_ = link;
#endif
  } else {
    spkt_abort_printf("Invalid switch port %d in pisces_nic::connect_output", src_outport);
  }
}

void
sculpin_nic::connect_input(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_link* link)
{
  //nothing to do
}

void
sculpin_nic::do_send(network_message* payload)
{
  nic_debug("sculpin: sending %s", payload->to_string().c_str());

  uint64_t bytes_left = payload->byte_length();
  uint64_t byte_offset = 0;

  timestamp now_ = now();
  if (now_ > inj_next_free_){
    inj_next_free_ = now_;
  }

  node_id to = payload->toaddr();
  node_id from = payload->fromaddr();
  uint64_t fid = payload->flow_id();
  while (bytes_left > 0){
    uint32_t pkt_size = std::min(uint32_t(bytes_left), packet_size_);
    bytes_left -= pkt_size;
    bool is_tail = bytes_left == 0;
    byte_offset += pkt_size;
    sculpin_packet* pkt = new sculpin_packet(is_tail ? payload : nullptr, pkt_size, is_tail,
                                             fid, to, from);
    timestamp extra_delay = inj_next_free_ - now_;
    timestamp time_to_send = pkt_size * inj_inv_bw_;
    inj_next_free_ += time_to_send;
    pkt_debug("packet injecting at t=%8.4e: %s",
              inj_next_free_.sec(), pkt->to_string().c_str());
    pkt->set_time_to_send(time_to_send);
    inj_link_->send_extra_delay(extra_delay, pkt);
  }

  if (payload->needs_ack()){
    network_message* ack = payload->clone_injection_ack();
    auto ev = new_callback(this, &nic::send_to_node, ack);
    send_self_event_queue(inj_next_free_, ev);
  }
}

void
sculpin_nic::cq_handle(sculpin_packet* pkt)
{
  message* msg = cq_.recv(pkt);
  if (msg){
    recv_message(msg);
  }
}

void
sculpin_nic::eject(sculpin_packet* pkt)
{
  timestamp now_ = now();
  if (now_ > ej_next_free_){
    ej_next_free_ = now_;
  }
  pkt_debug("incoming packet - ejection next free at t=%8.4e: %s",
            ej_next_free_.sec(), pkt->to_string().c_str());
  timestamp time_to_send = pkt->byte_length() * inj_inv_bw_;
  ej_next_free_ = ej_next_free_ + time_to_send;
  auto qev = new_callback(this, &sculpin_nic::cq_handle, pkt);
  send_self_event_queue(ej_next_free_, qev);
}

void
sculpin_nic::handle_payload(event *ev)
{
  sculpin_packet* pkt = static_cast<sculpin_packet*>(ev);

  timestamp time_to_send = pkt->byte_length() * inj_inv_bw_;
  if (time_to_send < pkt->time_to_send()){
    //tail flit cannot arrive here before it leaves the prev switch
    auto ev = new_callback(this, &sculpin_nic::eject, pkt);
    timestamp delta_t = pkt->time_to_send() - time_to_send;
    send_delayed_self_event_queue(delta_t, ev);
  } else {
    eject(pkt);
  }
}

void
sculpin_nic::handle_credit(event *ev)
{
  spkt_abort_printf("sculpin_nic::handle_credit: should not handle credits in sculpin model");
}

void
sculpin_nic::deadlock_check()
{
  //pass - nothing to do in sculpin
}

void
sculpin_nic::deadlock_check(event* ev)
{
  //pass - nothing to di in sculpin
}


}
} // end of namespace sstmac.
