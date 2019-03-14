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
  EventScheduler(params.find<int>("id"), comp),
  inj_buffer_(nullptr)
{
  initLinks(params);

  SST::Params inj_params = params.find_scoped_params("injection");
  double bw = inj_params.find<SST::UnitAlgebra>("bandwidth").getValue().toDouble();
  //PiscesSender::configurePayloadPortLatency(inj_params);
  std::string arb = inj_params.find<std::string>("arbirtrator");
  inj_buffer_ = new PiscesBuffer("merlin-inj", arb, bw, inj_params.find<SST::UnitAlgebra>("mtu").getRoundedValue(),
                                 comp, 1);

  LinkHandler* handler = newLinkHandler(this, &PiscesSimpleNetwork::creditArrived);
  SST::Link* selflink = comp->configureSelfLink("simple-inject", timeConverter(), handler);
  EventLink* sublink = new EventLink("pisces-inject", Timestamp(), selflink);
  inj_buffer_->setInput(0, 0, sublink);
  arb_ = sprockit::create<PiscesBandwidthArbitrator>("macro", arb, bw);
}

void
PiscesSimpleNetwork::initLinks(SST::Params& params)
{
  SST::Params inj_params = params.find_scoped_params("injection");
  int credits = inj_params.find<SST::UnitAlgebra>("credits").getRoundedValue();
  SST::Component* parent = getTrueComponent();
  SST::LinkMap* link_map = SST::Simulation::getSimulation()->getComponentLinkMap(parent->getId());
  for (auto& pair : link_map->getLinkMap()){
    debug("initialized simple network link %s", pair.first.c_str());
    SST::Link* link = pair.second;
    link->setDefaultTimeBase(timeConverter());
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
      inj_buffer_->setOutput(src_outport, dst_inport, new EventLink(pair.first, Timestamp(), link), credits);
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
  SimpleNetworkMessage* msg = new SimpleNetworkMessage(
        req, req->dest, nid_, req->size_in_bits/8);
  //control network
  logp_link_->send(new NicEvent(msg));
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
  NicEvent* nev = static_cast<NicEvent*>(ev);
  SimpleNetworkMessage* msg = static_cast<SimpleNetworkMessage*>(nev->msg());
  delete nev;
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
    credit_link_->send(credit);
    auto req = pkt->request();
    delete pkt;
    return req;
  } else {
    if (vn1_msgs_.empty()) return nullptr;
    SimpleNetworkMessage* msg = vn1_msgs_.front();
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
