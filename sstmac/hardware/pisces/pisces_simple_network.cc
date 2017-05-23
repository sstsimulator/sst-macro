/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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

#include <sstmac/hardware/pisces/pisces_simple_network.h>
#include <sstmac/sst_core/connectable_wrapper.h>

MakeDebugSlot(simple_network)

#define debug(str,...) debug_printf(sprockit::dbg::simple_network, "Simple network Node %d: " str, nid_, __VA_ARGS__)

namespace sstmac {
namespace hw {

pisces_simple_network::pisces_simple_network(sprockit::sim_parameters *params, SST::Component *comp) :
  SST::Interfaces::SimpleNetwork(comp),
  recv_functor_(nullptr),
  send_functor_(nullptr),
  credit_link_(nullptr),
  logp_link_(nullptr),
  event_scheduler(init_loc(params)),
  inj_buffer_(nullptr)
{
  //we need a self link
  event_scheduler::init_self_link(comp);

  init_links(params);

  sprockit::sim_parameters* inj_params = params->get_optional_namespace("injection");
  pisces_sender::configure_payload_port_latency(inj_params);
  inj_buffer_ = new pisces_injection_buffer(inj_params, this);
  inj_buffer_->set_input(inj_params, 0, 0, new_handler(this, &pisces_simple_network::credit_arrived));

  sprockit::sim_parameters* ej_params = params->get_optional_namespace("ejection");
  arb_ = pisces_bandwidth_arbitrator::factory::get_param("arbitrator", params);
}

void
pisces_simple_network::init_links(sprockit::sim_parameters* params)
{
  sprockit::sim_parameters* inj_params = params->get_optional_namespace("injection");
  SST::LinkMap* link_map = SST::Simulation::getSimulation()->getComponentLinkMap(comp()->getId());
  for (auto& pair : link_map->getLinkMap()){
    debug("initialized simple network link %s", pair.first.c_str());
    SST::Link* link = pair.second;
    std::istringstream istr(pair.first);
    std::string port_type;
    int src_outport, dst_inport;
    istr >> port_type;
    istr >> src_outport;
    istr >> dst_inport;
    if (port_type == "input"){
      configureLink(pair.first,
       new SST::Event::Handler<pisces_simple_network>(this, &pisces_simple_network::packet_head_arrived));
      credit_link_ = link;
    } else if (port_type == "output"){
      link_wrapper* wrapper = new link_wrapper(link);
      inj_buffer_->set_output(inj_params, src_outport, dst_inport, wrapper);
      configureLink(pair.first,
       new SST::Event::Handler<pisces_injection_buffer>(inj_buffer_, &pisces_injection_buffer::handle_credit));
    } else if (port_type == "in-out"){
      logp_link_ = link;
      configureLink(pair.first,
        new SST::Event::Handler<pisces_simple_network>(this, &pisces_simple_network::ctrl_msg_arrived));
    }
  }
}

void
pisces_simple_network::sendInitData(SST::Interfaces::SimpleNetwork::Request* req)
{
  spkt_abort_printf("pisces simple network cannot send init data");
}

SST::Interfaces::SimpleNetwork::Request*
pisces_simple_network::recvInitData()
{
  spkt_abort_printf("pisces simple network cannot recv init data");
  return nullptr;
}

device_id
pisces_simple_network::init_loc(sprockit::sim_parameters* params)
{
  nid_ = params->get_int_param("id");
  return device_id(nid_, device_id::node);
}

bool
pisces_simple_network::send_pisces_network(Request* req, int vn)
{
  int bytes = req->size_in_bits / 8;
  if (!inj_buffer_->space_to_send(bytes))
    return false;

  uint64_t ignore_flow_id = 0;

  simple_network_packet* pkt = new simple_network_packet(req, bytes, req->tail,
                                                    req->dest, nid_, vn);
  inj_buffer_->handle_payload(pkt);
  return true;
}

bool
pisces_simple_network::send_logp_network(SST::Interfaces::SimpleNetwork::Request *req, int vn)
{
  simple_network_message* msg = new simple_network_message(req, req->dest, nid_, req->size_in_bits/8);
  //control network
  logp_link_->send(0, time_converter_, msg);
  if (send_functor_) (*send_functor_)(vn);
  return true;
}

bool
pisces_simple_network::send(Request *req, int vn)
{
  debug("sending request of size %ld bits on vn %d", req->size_in_bits, vn);
  if (vn == 0){
    return send_pisces_network(req, vn);
  } else if (vn == 1){
    return send_logp_network(req, vn);
  } else {
    spkt_abort_printf("PISCES cannot handle vn's other than 0 and 1");
    return true;
  }
}

void
pisces_simple_network::packet_tail_arrived(simple_network_packet* pkt)
{
  vn0_pkts_.push_back(pkt);
  if (recv_functor_) {
    bool keep = (*recv_functor_)(pkt->vn());
    if (!keep) recv_functor_ = nullptr;
  }
}

void
pisces_simple_network::packet_head_arrived(event* ev)
{
  simple_network_packet* pkt = safe_cast(simple_network_packet, ev);
  timestamp delay = arb_->head_tail_delay(pkt);
  schedule_delay(delay, new_callback(this, &pisces_simple_network::packet_tail_arrived, pkt));
}

void
pisces_simple_network::credit_arrived(event *ev)
{
  pisces_credit* credit = safe_cast(pisces_credit,ev);
  num_injection_credits_ += credit->num_credits();
  //this means we sent on vn 0
  if (send_functor_) (*send_functor_)(0);
  delete credit;
}

void
pisces_simple_network::ctrl_msg_arrived(event* ev)
{
  simple_network_message* msg = safe_cast(simple_network_message, ev);
  vn1_msgs_.push_back(msg);
  if (recv_functor_) (*recv_functor_)(1);
}

SST::Interfaces::SimpleNetwork::Request*
pisces_simple_network::recv(int vn)
{
  if (vn == 0){
    if (vn0_pkts_.empty()) return nullptr;
    simple_network_packet* pkt = vn0_pkts_.front();
    vn0_pkts_.pop_front();
    int num_bytes = pkt->byte_length();
    pisces_credit* credit = new pisces_credit(pkt->next_port(), pkt->pisces_payload::vc(), num_bytes);
    credit_link_->send(0, time_converter_, credit);
    auto req = pkt->request();
    delete pkt;
    return req;
  } else {
    if (vn1_msgs_.empty()) return nullptr;
    simple_network_message* msg = vn1_msgs_.front();
    vn1_msgs_.pop_front();
    auto req = msg->req();
    delete msg;
    return req;
  }
}

bool
pisces_simple_network::requestToReceive(int vn)
{
  if (vn == 0){
    debug("requesting to receive on main network - %d pending", vn0_pkts_.size());
    return !vn0_pkts_.empty();
  } else {
    debug("requesting to receive on ctrl msg network - %d pending", vn1_msgs_.size());
    return !vn1_msgs_.empty();
  }
}

bool
pisces_simple_network::spaceToSend(int vn, int num_bits)
{
  debug("checking for space to send %d bits on vn %d", vn, num_bits);
  if (vn == 0){
    int bytes = num_bits / 8;
    return bytes <= num_injection_credits_;
  } else {
    return true; //always able to send
  }

}

bool
pisces_simple_network::initialize(const std::string &portName,
                              const SST::UnitAlgebra &link_bw, int vns,
                              const SST::UnitAlgebra &in_buf_size,
                              const SST::UnitAlgebra &out_buf_size)
{
  if (vns > 2){
    spkt_abort_printf("PISCES simple network cannot support %d virtual networks - max is 2", vns);
  }

  SST::UnitAlgebra one_byte("1B");
  SST::UnitAlgebra bytes_per_s("1B/s");
  num_injection_credits_ = (out_buf_size / one_byte).getRoundedValue();
  sst_link_bw_ = link_bw;
  //(link_bw / bytes_per_s).getRoundedValue();
  initialized_ = true;

  debug("initialized with %d injection credits", num_injection_credits_);

  return true;
}

}
}