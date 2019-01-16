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

#include <sstmac/hardware/pisces/pisces_simple_network.h>
#include <sstmac/sst_core/connectable_wrapper.h>
#include <sstmac/common/event_callback.h>

MakeDebugSlot(simple_network)

#define debug(str,...) debug_printf(sprockit::dbg::simple_network, "Simple network Node %d: " str, nid_, __VA_ARGS__)

namespace sstmac {
namespace hw {

PiscesSimpleNetwork::PiscesSimpleNetwork(SST::Params& params, SST::Component *comp) :
  SST::Interfaces::SimpleNetwork(comp),
  recv_functor_(nullptr),
  send_functor_(nullptr),
  credit_link_(nullptr),
  logp_link_(nullptr),
  EventScheduler(params->get_int_param("id")),
  inj_buffer_(nullptr)
{
  //we need a self link
  EventScheduler::initSelfLink(comp);

  initLinks(params);

  SST::Params inj_params = params->get_optional_namespace("injection");
  PiscesSender::configurePayloadPortLatency(inj_params);
  inj_buffer_ = new PiscesBuffer(inj_params, this, 1);

  EventHandler* handler = newHandler(this, &PiscesSimpleNetwork::creditArrived);
  inj_buffer_->setInput(inj_params, 0, 0, new EventLink(self_link(), comp));

  SST::Params ej_params = params->get_optional_namespace("ejection");
  arb_ = PiscesBandwidthArbitrator::factory::get_param("arbitrator", params);
}

void
PiscesSimpleNetwork::initLinks(SST::Params& params)
{
  SST::Params inj_params = params->get_optional_namespace("injection");
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
      configureLink(pair.first, newLinkHandler(this, &PiscesSimpleNetwork::packetHeadArrived));
      credit_link_ = link;
    } else if (port_type == "output"){
      inj_buffer_->setOutput(inj_params, src_outport, dst_inport, new EventLink(link, comp()));
      configureLink(pair.first, newLinkHandler(inj_buffer_, &PiscesBuffer::handleCredit));
    } else if (port_type == "in-out"){
      logp_link_ = link;
      configureLink(pair.first, newLinkHandler(this, &PiscesSimpleNetwork::ctrlMsgArrived));
    }
  }
}

void
PiscesSimpleNetwork::sendInitData(SST::Interfaces::SimpleNetwork::Request* req)
{
  spkt_abort_printf("pisces simple network cannot send init data");
}

SST::Interfaces::SimpleNetwork::Request*
PiscesSimpleNetwork::recvInitData()
{
  spkt_abort_printf("pisces simple network cannot recv init data");
  return nullptr;
}

bool
PiscesSimpleNetwork::sendPiscesNetwork(Request* req, int vn)
{
  int bytes = req->size_in_bits / 8;
  if (!inj_buffer_->spaceToSend(vn, bytes))
    return false;

  uint64_t ignore_flow_id = 0;

  SimpleNetworkPacket* pkt = new SimpleNetworkPacket(req, bytes, req->tail,
                                                    req->dest, nid_, vn);
  inj_buffer_->handlePayload(pkt);
  return true;
}

bool
PiscesSimpleNetwork::sendLogpNetwork(SST::Interfaces::SimpleNetwork::Request *req, int vn)
{
  SimpleNetworkmessage* msg = new SimpleNetworkmessage(
        req, req->dest, nid_, req->size_in_bits/8);
  //control network
  logp_link_->send(0, time_converter_, msg);
  if (send_functor_) (*send_functor_)(vn);
  return true;
}

bool
PiscesSimpleNetwork::send(Request *req, int vn)
{
  debug("sending request of size %ld bits on vn %d", req->size_in_bits, vn);
  if (vn == 0){
    return sendPiscesNetwork(req, vn);
  } else if (vn == 1){
    return sendLogpNetwork(req, vn);
  } else {
    spkt_abort_printf("PISCES cannot handle vn's other than 0 and 1");
    return true;
  }
}

void
PiscesSimpleNetwork::packetTailArrived(SimpleNetworkPacket* pkt)
{
  vn0_pkts_.push_back(pkt);
  if (recv_functor_) {
    bool keep = (*recv_functor_)(pkt->vn());
    if (!keep) recv_functor_ = nullptr;
  }
}

void
PiscesSimpleNetwork::packetHeadArrived(Event* ev)
{
  SimpleNetworkPacket* pkt = safe_cast(SimpleNetworkPacket, ev);
  Timestamp delay = arb_->headTailDelay(pkt);
  sendDelayedExecutionEvent(delay, newCallback(this, &PiscesSimpleNetwork::packetTailArrived, pkt));
}

void
PiscesSimpleNetwork::creditArrived(Event *ev)
{
  PiscesCredit* credit = safe_cast(PiscesCredit,ev);
  num_injection_credits_ += credit->numCredits();
  //this means we sent on vn 0
  if (send_functor_) (*send_functor_)(0);
  delete credit;
}

void
PiscesSimpleNetwork::ctrlMsgArrived(Event* ev)
{
  SimpleNetworkmessage* msg = safe_cast(SimpleNetworkmessage, ev);
  vn1_msgs_.push_back(msg);
  if (recv_functor_) (*recv_functor_)(1);
}

SST::Interfaces::SimpleNetwork::Request*
PiscesSimpleNetwork::recv(int vn)
{
  if (vn == 0){
    if (vn0_pkts_.empty()) return nullptr;
    SimpleNetworkPacket* pkt = vn0_pkts_.front();
    vn0_pkts_.pop_front();
    int num_bytes = pkt->byteLength();
    PiscesCredit* credit = new PiscesCredit(pkt->edgeOutport(), pkt->PiscesPacket::vc(), num_bytes);
    credit_link_->send(0, time_converter_, credit);
    auto req = pkt->request();
    delete pkt;
    return req;
  } else {
    if (vn1_msgs_.empty()) return nullptr;
    SimpleNetworkmessage* msg = vn1_msgs_.front();
    vn1_msgs_.pop_front();
    auto req = msg->req();
    delete msg;
    return req;
  }
}

bool
PiscesSimpleNetwork::requestToReceive(int vn)
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
PiscesSimpleNetwork::spaceToSend(int vn, int num_bits)
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
PiscesSimpleNetwork::initialize(const std::string &portName,
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
