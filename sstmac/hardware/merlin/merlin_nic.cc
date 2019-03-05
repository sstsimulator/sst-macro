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
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/sculpin/sculpin_switch.h>
#include <sstmac/hardware/common/recv_cq.h>

#include <sst/core/interfaces/simpleNetwork.h>

#include <stddef.h>

#define pkt_debug(...) \
  debug_printf(sprockit::dbg::sculpin, "sculpin NIC %d: %s", \
    int(addr()), sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

/**
 @class SculpinNIC
 Network interface compatible with sculpin network model
 */
class MerlinNIC :
  public NIC
{
  struct MyRequest : public SST::Interfaces::SimpleNetwork::Request {
    uint64_t flow_id;
  };

  struct MessageEvent : public Event {
    NotSerializable(MessageEvent)
    MessageEvent(NetworkMessage* msg) :
      msg_(msg)
    {
    }

    NetworkMessage* msg() const {
      return msg_;
    }

   private:
    NetworkMessage*  msg_;
  };

 public:
  SST_ELI_REGISTER_DERIVED(
    NIC,
    MerlinNIC ,
    "macro",
    "merlin",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "A NIC wrapping Merlin")

  MerlinNIC(SST::Params& params, Node* parent) :
    NIC(params, parent),
    vns_(2)
  {
    auto* link_ctrl = parent->loadSubComponent(params.find<std::string>("module"), parent, params);
    link_control_ = dynamic_cast<SST::Interfaces::SimpleNetwork*>(link_ctrl);
    if (!link_control_){
      sprockit::abort("Failed to dynamic cast link control");
    }

    pending_.resize(vns_);
    mtu_ = params.find<SST::UnitAlgebra>("mtu").getRoundedValue();

    auto recv_notify = new SST::Interfaces::SimpleNetwork::Handler<MerlinNIC>(this,&MerlinNIC::incomingPacket);
    auto send_notify = new SST::Interfaces::SimpleNetwork::Handler<MerlinNIC>(this,&MerlinNIC::incomingCredit);

    link_control_->setNotifyOnReceive(recv_notify);
    link_control_->setNotifyOnSend(send_notify);

    SST::UnitAlgebra bw("10GB/s");
    SST::UnitAlgebra buf_size("64KB");
    link_control_->initialize("rtr", bw, 2, buf_size, buf_size);
  }

  std::string toString() const override {
    return sprockit::printf("sculpin nic(%d)", int(addr()));
  }

  void sendManagerMsg(NetworkMessage *msg) override {
    inject(1, msg);
  }

  void init(unsigned int phase) override {
    link_control_->init(phase);
  }

  void setup() override {
    link_control_->setup();
  }

  void complete(unsigned int phase) override {
    link_control_->complete(phase);
  }

  void finish() override {
    link_control_->finish();
  }

  virtual ~MerlinNIC() throw () {}

  bool incomingCredit(int vn){
    sendWhatYouCan(vn);
    return true; //keep me
  }

  bool incomingPacket(int vn){
    auto* req = link_control_->recv(vn);
    while (req){
      MyRequest* myreq = static_cast<MyRequest*>(req);
      auto* payload = myreq->takePayload();
      MessageEvent* ev = payload ? static_cast<MessageEvent*>(payload) : nullptr;
      nic_debug("receiving packet of size %d on vn %d", (myreq->size_in_bits/8), vn);
      Flow* flow = cq_.recv(myreq->flow_id, myreq->size_in_bits/8,
                            ev ? ev->msg() : nullptr);
      if (flow){
        auto* msg = static_cast<NetworkMessage*>(flow);
        nic_debug("fully received message %s", msg->toString().c_str());
        recvMessage(msg);
      }
      req = link_control_->recv(vn);
    }
    return true; //keep me active
  }

  void connectOutput(int src_outport, int dst_inport, EventLink* link) override {
    sprockit::abort("should never be called on Merlin NIC");
  }

  void connectInput(int src_outport, int dst_inport, EventLink* link) override {
    sprockit::abort("should never be called on Merlin NIC");
  }

  LinkHandler* creditHandler(int port) override {
    sprockit::abort("should never be called on Merlin NIC");
    return nullptr;
  }

  LinkHandler* payloadHandler(int port) override {
    sprockit::abort("should never be called on Merlin NIC");
    return nullptr;
  }

 private:
  struct Pending {
    NetworkMessage* payload;
    uint64_t bytesLeft;
    Pending(NetworkMessage* p) :
      payload(p),
      bytesLeft(p->byteLength())
    {
    }
  };

  void inject(int vn, NetworkMessage* payload){
    if (payload->byteLength() == 0){
      //spkt_abort_printf("Got zero-sized message: %s", payload->toString().c_str());
    }
    pending_[vn].emplace(payload);
    nic_debug("sending message on vn %d: %s", vn, payload->toString().c_str());
    sendWhatYouCan(vn);
  }

  void doSend(NetworkMessage* payload) override {
    inject(0, payload);
  }

  void sendWhatYouCan(int vn){
    auto& pendingList = pending_[vn];
    while (!pendingList.empty()){
      Pending& p = pendingList.front();
      bool done = sendWhatYouCan(vn,p);
      if (done){
        pendingList.pop();
      } else {
        return;
      }
    }
  }

  bool sendWhatYouCan(int vn, Pending& p){
    uint64_t next_bytes = std::min(uint64_t(mtu_), p.bytesLeft);
    uint32_t next_bits = next_bytes * 8; //this is fine for 32-bits
    while (link_control_->spaceToSend(vn, next_bits)){
      auto* req = new SST::Interfaces::SimpleNetwork::Request;
      req->head = p.bytesLeft == p.payload->byteLength();
      p.bytesLeft -= next_bytes;
      req->tail = p.bytesLeft == 0;
      if (req->tail){
        req->givePayload(new MessageEvent(p.payload));
      } else {
        req->givePayload(nullptr);
      }
      req->src = p.payload->fromaddr();
      req->dest = p.payload->toaddr();
      req->size_in_bits = next_bits;
      req->vn = 0;

      link_control_->send(req, vn);

      next_bytes = std::min(uint64_t(mtu_), p.bytesLeft);
      next_bits = next_bytes * 8; //this is fine for 32-bits
      if (next_bytes == 0) return true;
    }
    return false;
  }

 private:
  SST::Interfaces::SimpleNetwork* link_control_;
  std::vector<std::queue<Pending>> pending_;
  uint32_t mtu_;
  RecvCQ cq_;
  int vns_;
};


}
} // end of namespace sstmac.
