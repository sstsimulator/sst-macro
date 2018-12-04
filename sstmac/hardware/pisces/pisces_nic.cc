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
#include <sstmac/hardware/pisces/pisces_nic.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/common/recv_cq.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sprockit/errors.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

#include <stddef.h>

RegisterNamespaces("congestion_delays", "congestion_matrix");

namespace sstmac {
namespace hw {

pisces_nic::pisces_nic(sprockit::sim_parameters* params, node* parent) :
  nic(params, parent),
  pending_inject_(1)
{
  sprockit::sim_parameters* inj_params = params->get_namespace("injection");
  sprockit::sim_parameters* ej_params = params->get_optional_namespace("ejection");

  self_mtl_link_ = allocate_local_link(timestamp(), parent,
                                    new_handler(this, &nic::mtl_handle));

  //make port 0 a copy of the injection params
  //this looks pointless, but is needed for integrated core (I think)
  sprockit::sim_parameters* port0_params = params->get_optional_namespace("port0");
  inj_params->combine_into(port0_params);

  pisces_sender::configure_payload_port_latency(inj_params);
  inj_buffer_ = new pisces_buffer(inj_params, parent, 1/*single vc for inj*/);
  inj_stats_ = packet_stats_callback::factory::
                get_optional_param("stats", "null", params, parent);
  inj_buffer_->set_stat_collector(inj_stats_);

  credit_lat_ = inj_params->get_time_param("send_latency");
  ej_stats_ = packet_stats_callback::factory::
                        get_optional_param("stats", "null", ej_params, parent);

  packet_size_ = inj_params->get_byte_length_param("mtu");

}

timestamp
pisces_nic::send_latency(sprockit::sim_parameters *params) const
{
  return params->get_namespace("injection")->get_time_param("latency");
}

timestamp
pisces_nic::credit_latency(sprockit::sim_parameters *params) const
{
  return params->get_namespace("injection")->get_time_param("latency");
}

void
pisces_nic::init(unsigned int phase)
{
}

void
pisces_nic::setup()
{
}

pisces_nic::~pisces_nic() throw ()
{
  if (inj_buffer_) delete inj_buffer_;
  if (inj_stats_) delete inj_stats_;
  if (ej_stats_) delete ej_stats_;
  if (self_mtl_link_) delete self_mtl_link_;
}

link_handler*
pisces_nic::payload_handler(int port)
{
  if (port == nic::LogP){
    return new_link_handler(this, &nic::mtl_handle);
  } else {
    return new_link_handler(this, &pisces_nic::incoming_packet);
  }
}

link_handler*
pisces_nic::credit_handler(int port)
{
  return new_link_handler(this, &pisces_nic::packet_sent);
}

void
pisces_nic::connect_output(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_link* link)
{
  if (src_outport == Injection){
    inj_buffer_->set_output(params, src_outport, dst_inport, link);
  } else if (src_outport == LogP) {
    logp_link_ = link;
  } else {
    spkt_abort_printf("Invalid port %d in pisces_nic::connect_output", src_outport);
  }
}

void
pisces_nic::connect_input(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_link* link)
{
  credit_link_ = link;
}

uint64_t
pisces_nic::inject(int vn, uint64_t offset, network_message* netmsg)
{
  pisces_debug("On %s, trying to inject %s",
               to_string().c_str(), netmsg->to_string().c_str());
  while (inj_buffer_->space_to_send(vn, packet_size_)){
    uint64_t bytes = std::min(uint64_t(packet_size_), netmsg->byte_length() - offset);
    bool is_tail = (offset + bytes) == netmsg->byte_length();
    //only carry the payload if you're the tail packet
    pisces_packet* payload = new pisces_packet(is_tail ? netmsg : nullptr,
                                               bytes, netmsg->flow_id(), is_tail,
                                               netmsg->fromaddr(), netmsg->toaddr());
    //start on a singel virtual channel (0)
    payload->set_deadlock_vc(0);
    payload->update_vc();
    payload->reset_stages(0);
    payload->set_inport(0);
    timestamp t = inj_buffer_->send_payload(payload);

    pisces_debug("On %s, injecting from %lu->%lu on %s",
                 to_string().c_str(), offset, offset + bytes, netmsg->to_string().c_str());

    offset += bytes;

    if (offset == netmsg->byte_length()){
      if (netmsg->needs_ack()){
        send_self_event_queue(t, new_callback(this, &nic::mtl_handle,
                     netmsg->clone_injection_ack()));
      }
      return offset;
    }
  }
  return offset;
}

void
pisces_nic::do_send(network_message* netmsg)
{
  nic_debug("packet flow: sending %s", netmsg->to_string().c_str());
  int vn = 0; //we only ever use one virtual network


  uint64_t offset = inject(vn, 0, netmsg);
  if (offset < netmsg->byte_length()){
    pending_inject_[vn].emplace(offset, netmsg->byte_length(), netmsg);
  }
}

void
pisces_nic::packet_arrived(pisces_packet* pkt)
{
  ej_stats_->collect_final_event(pkt);
  auto* msg = completion_queue_.recv(pkt);
  if (msg){
    recv_message(static_cast<network_message*>(msg));
  }
  delete pkt;
  int buffer_port = 0;
  pisces_credit* credit = new pisces_credit(buffer_port, pkt->vc(), pkt->byte_length());
  credit_link_->send(credit);
}

void
pisces_nic::packet_sent(event* ev)
{
  pisces_debug("On %s, packet sent notification", to_string().c_str());
  pisces_credit* credit = safe_cast(pisces_credit, ev);
  int vc = credit->vc();
  inj_buffer_->handle_credit(ev);
  while (!pending_inject_[vc].empty()){
    auto& pending = pending_inject_[vc].front();
    pending.bytes_sent = inject(vc, pending.bytes_sent, pending.msg);
    if (pending.bytes_sent == pending.msg->byte_length()){
      pending_inject_[vc].pop();
    } else {
      //ran out of space - jump out
      return;
    }
  }
}

void
pisces_nic::incoming_packet(event* ev)
{
  pisces_packet* pkt = safe_cast(pisces_packet, ev);
  if (cut_through_){
    //these are pipelined
    double delay = pkt->byte_length() / pkt->bw();
    send_delayed_self_event_queue(delay, new_callback(this, &pisces_nic::packet_arrived, pkt));
  } else {
    packet_arrived(pkt);
  }
}

void
pisces_nic::deadlock_check()
{
  for (auto& queue : pending_inject_){
    while (!queue.empty()){
      pending& p = queue.front();
      std::cerr << "Message " << p.msg->to_string()
         << " has not finished injecting: " << p.bytes_sent
         << " sent out of " << p.bytes_total << std::endl;
      queue.pop();
    }
  }
}

void
pisces_nic::deadlock_check(event* ev)
{
}

}
} // end of namespace sstmac.
