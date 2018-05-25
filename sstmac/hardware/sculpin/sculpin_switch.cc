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
#include <sstmac/hardware/sculpin/sculpin_switch.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/common/event_callback.h>

RegisterNamespaces("switch", "router", "xbar", "link");

RegisterKeywords(
{ "latency", "latency to traverse a portion of the switch - sets both credit/send" },
{ "bandwidth", "the bandwidth of a given link sending" },
{ "congestion", "whether to include congestion modeling on switches" },
);


#define pkt_debug(...) \
  debug_printf(sprockit::dbg::sculpin, "sculpin switch %d: %s", \
    int(addr()), sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

sculpin_switch::sculpin_switch(
  sprockit::sim_parameters *params, uint32_t id, event_manager *mgr) :
  router_(nullptr),
  congestion_(true),
  network_switch(params, id, mgr)
{
  sprockit::sim_parameters* rtr_params = params->get_optional_namespace("router");
  rtr_params->add_param_override_recursive("id", int(my_addr_));
  router_ = router::factory::get_param("name", rtr_params, top_, this);

  congestion_ = params->get_optional_bool_param("congestion", true);

  sprockit::sim_parameters* ej_params = params->get_optional_namespace("ejection");
  std::vector<topology::injection_port> inj_conns;
  top_->nodes_connected_to_ejection_switch(my_addr_, inj_conns);
  for (topology::injection_port& conn : inj_conns){
    sprockit::sim_parameters* port_params = topology::get_port_params(params, conn.port);
    ej_params->combine_into(port_params);
  }

#if !SSTMAC_INTEGRATED_SST_CORE
  payload_handler_ = new_handler(this, &sculpin_switch::handle_payload);
  credit_handler_ = new_handler(this, &sculpin_switch::handle_credit);
#endif

  ports_.resize(top_->max_num_ports());
  for (int i=0; i < ports_.size(); ++i){
    ports_[i].id = i;
    ports_[i].seqnum = 0;
  }
}

sculpin_switch::~sculpin_switch()
{
  if (router_) delete router_;
#if !SSTMAC_INTEGRATED_SST_CORE
  if (credit_handler_) delete credit_handler_;
  if (payload_handler_) delete payload_handler_;
#endif
}

void
sculpin_switch::connect_output(
  sprockit::sim_parameters* port_params,
  int src_outport,
  int dst_inport,
  event_link* link)
{
  double bw = port_params->get_bandwidth_param("bandwidth");
  port& p = ports_[src_outport];
  p.link = link;
  p.inv_bw = 1.0/bw;
}

void
sculpin_switch::connect_input(
  sprockit::sim_parameters* port_params,
  int src_outport,
  int dst_inport,
  event_link* link)
{
  //no-op
}

timestamp
sculpin_switch::send_latency(sprockit::sim_parameters *params) const
{
  return params->get_time_param("send_latency");
}

timestamp
sculpin_switch::credit_latency(sprockit::sim_parameters *params) const
{
  return params->get_time_param("send_latency");
}

void
sculpin_switch::compatibility_check() const
{
  router_->compatibility_check();
}

void
sculpin_switch::deadlock_check()
{
  //pass - no deadlock check in sculpin
}

void
sculpin_switch::deadlock_check(event *ev)
{
  //pass - no deadlock check in sculpin
}

int
sculpin_switch::queue_length(int portnum) const
{
  auto& p = ports_[portnum];
  return p.priority_queue.size();
}

void
sculpin_switch::handle_credit(event *ev)
{
  spkt_abort_printf("sculpin_switch::handle_credit: should never be called");
}

void
sculpin_switch::send(port& p, sculpin_packet* pkt, timestamp now)
{
  if (now > p.next_free){
    p.next_free = now;
  }

  timestamp extra_delay = p.next_free - now;
  timestamp time_to_send = pkt->num_bytes() * p.inv_bw;
  p.next_free += time_to_send;
  pkt->set_time_to_send(time_to_send);
  p.link->send_extra_delay(extra_delay, pkt);

  pkt_debug("packet leaving port %d at t=%8.4e: %s",
            p.id, p.next_free.sec(), pkt->to_string().c_str());
  if (p.priority_queue.empty()){
    //reset the sequence number for ordering packets
    p.seqnum = 0;
  } else {
    pkt_debug("scheduling pull from port %d at t=%8.4e with %d queued",
              p.id, p.next_free.sec(), p.priority_queue.size());
    //schedule this port to pull another packet
    auto* ev = new_callback(this, &sculpin_switch::pull_next, p.id);
    send_self_event_queue(p.next_free, ev);
  }

}

void
sculpin_switch::pull_next(int portnum)
{
  port& p = ports_[portnum];
  if (p.priority_queue.empty()){
    spkt_abort_printf("sculpin_switch::pull_next: incorrectly scheduled pull on switch %d from empty port %d",
                      int(addr()), p.id);
  }
  pkt_debug("pulling pending packet from port %d with %d queued", portnum, p.priority_queue.size());
  auto iter = p.priority_queue.begin();
  sculpin_packet* pkt = *iter;
  p.priority_queue.erase(iter);
  send(p, pkt, now());
}

void
sculpin_switch::try_to_send_packet(sculpin_packet* pkt)
{
  timestamp now_ = now();
  pkt->set_arrival(now_);
  port& p = ports_[pkt->next_port()];
  pkt->set_seqnum(p.seqnum++);
  if (!congestion_){
    timestamp time_to_send = pkt->num_bytes() * p.inv_bw;
    p.next_free += time_to_send;
    pkt->set_time_to_send(time_to_send);
    p.link->send(pkt);
  } else if (p.next_free > now_){
    //oh, well, I have to wait
    if (p.priority_queue.empty()){
      //nothing else is waiting - which means I have to schedule my own pull
      //schedule this port to pull another packet
      auto* ev = new_callback(this, &sculpin_switch::pull_next, p.id);
      send_self_event_queue(p.next_free, ev);
      pkt_debug("new packet has to schedule pull from port %d at t=%8.4e", p.id, p.next_free.sec());
    }
    pkt_debug("new packet has to wait on queue on port %d with %d queued", p.id, p.priority_queue.size());
    p.priority_queue.insert(pkt);
  } else if (p.priority_queue.empty()){
    //nothing there - go ahead and send
    send(p, pkt, now_);
  } else {
    //race condition - there is something in the queue
    //I must hop in the queue as well
    pkt_debug("new packet has to wait on queue on port %d with %d queued", p.id, p.priority_queue.size());
    p.priority_queue.insert(pkt);
  }
}

void
sculpin_switch::handle_payload(event *ev)
{
  sculpin_packet* pkt = safe_cast(sculpin_packet, ev);
  switch_debug("handling payload %s", pkt->to_string().c_str());
  router_->route(pkt);

  port& p = ports_[pkt->next_port()];
  timestamp time_to_send = p.inv_bw * pkt->num_bytes();
  /** I am processing the head flit - so I assume compatibility with wormhole routing
   * The tail flit cannot leave THIS switch prior to its departure time in the prev switch */
  if (pkt->time_to_send() > time_to_send){
    //delay the packet
    auto ev = new_callback(this, &sculpin_switch::try_to_send_packet, pkt);
    timestamp delta_t = pkt->time_to_send() - time_to_send;
    send_delayed_self_event_queue(delta_t, ev);
  } else {
    try_to_send_packet(pkt);
  }
}

std::string
sculpin_switch::to_string() const
{
  return sprockit::printf("sculpin switch %d", int(my_addr_));
}

link_handler*
sculpin_switch::credit_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new_link_handler(this, &sculpin_switch::handle_credit);
#else
  return credit_handler_;
#endif
}

link_handler*
sculpin_switch::payload_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new_link_handler(this, &sculpin_switch::handle_payload);
#else
  return payload_handler_;
#endif
}

}
}
