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

#include <sumi-mpi/mpi_message.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/memory/memory_model.h>
#include <sstmac/hardware/snappr/snappr_nic.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/stats/ftq.h>
#include <sprockit/errors.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

#include <limits>

#include <stddef.h>

#define pkt_debug(...) \
  debug_printf(sprockit::dbg::snappr | sprockit::dbg::nic, "snappr NIC on node %d: %s", \
    int(addr()), sprockit::printf(__VA_ARGS__).c_str())


namespace sstmac {
namespace hw {

struct SnapprRequest : public MemoryModel::Request {
  uint64_t offset;
  NetworkMessage* payload;
};

SnapprNIC::SnapprNIC(SST::Component* parent, SST::Params& params) :
  NIC(parent, params)
{
  SST::Params inj_params = params.find_scoped_params("injection");

  //configure for a single port for now
  outports_.reserve(1);

  packet_size_ = inj_params.find<SST::UnitAlgebra>("mtu").getRoundedValue();

  int num_ports = 1;
  std::string arbtype = inj_params.find<std::string>("arbitrator", "fifo");
  uint32_t credits = inj_params.find<SST::UnitAlgebra>("credits").getRoundedValue();
  int qosLevels = params.find<int>("qos_levels", 1);
  flow_control_ = inj_params.find<bool>("flow_control", true);
  inj_byte_delay_ = TimeDelta(inj_params.find<SST::UnitAlgebra>("bandwidth").getValue().inverse().toDouble());
  for (int i=0; i < num_ports; ++i){
    std::string subId = sprockit::printf("NIC%d:%d", addr(), i);
    outports_.emplace_back(inj_params, arbtype, subId, "NIC_send", i, inj_byte_delay_,
                           true/*always need congestion on NIC*/, flow_control_, NIC::parent());
    SnapprOutPort& p = outports_[i];
    p.setVirtualLanes(qosLevels, credits);
    p.addTailNotifier(this, &SnapprNIC::handleTailPacket);
  }


  std::string queuetype = params.find<std::string>("queue", "fifo");
  inject_queue_ = sprockit::create<InjectionQueue>("macro", queuetype, params);

  Node* nd = safe_cast(Node,parent);
  mem_model_ = nd->mem();
  auto* handler = mem_model_->makeHandler(this, &SnapprNIC::handleMemoryResponse);
  mem_req_id_ = mem_model_->initialize(handler);

  buffer_remaining_ = params.find<SST::UnitAlgebra>("buffer", "4MB").getRoundedValue();
  ignore_memory_ = params.find<bool>("ignore_memory", true);
}

void
SnapprNIC::init(unsigned int phase)
{
}

void
SnapprNIC::setup()
{
  SubComponent::setup();
}

SnapprNIC::~SnapprNIC() throw ()
{
}

LinkHandler*
SnapprNIC::payloadHandler(int port)
{
  if (port == NIC::LogP){
    return newLinkHandler(this, &NIC::mtlHandle);
  } else {
    return newLinkHandler(this, &SnapprNIC::handlePayload);
  }
}

LinkHandler*
SnapprNIC::creditHandler(int port)
{
  return newLinkHandler(this, &SnapprNIC::handleCredit);
}

void
SnapprNIC::connectOutput(int src_outport, int dst_inport, EventLink::ptr&& link)
{
  if (src_outport == Injection){
    outports_[src_outport].link = std::move(link);
    outports_[src_outport].dst_port = dst_inport;
  } else if (src_outport == LogP) {
    logp_link_ = std::move(link);
  } else {
    spkt_abort_printf("Invalid switch port %d in PiscesNIC::connectOutput", src_outport);
  }
}

void
SnapprNIC::connectInput(int src_outport, int dst_inport, EventLink::ptr&& link)
{
  if (dst_inport == Injection){
    switch_outport_ = src_outport;
    credit_link_ = std::move(link);
  }
}

void
SnapprNIC::copyToNicBuffer()
{
  while (!inject_queue_->empty()){ //copy as much as you can
    auto pair = inject_queue_->top();
    uint64_t byte_offset = pair.first;
    NetworkMessage* payload = pair.second;
    uint64_t bytes_left = payload->byteLength() - byte_offset;
    uint32_t pkt_size = std::min(bytes_left, uint64_t(packet_size_));
    if (pkt_size <= buffer_remaining_){
      nic_debug("packet of size=%" PRIu32 " at offset=%" PRIu64 " ready to inject: %s",
                pkt_size, byte_offset, payload->toString().c_str());
      //if flow control is off we won't get credits
      //so just don't worry about buffer space
      if (flow_control_){
        buffer_remaining_ -= pkt_size;
      }
      if (ignore_memory_){
        injectPacket(pkt_size, byte_offset, payload);
      } else {
        SnapprRequest* req = new SnapprRequest;
        req->bytes = pkt_size;
        req->offset = byte_offset;
        req->payload = payload;
        mem_model_->accessRequest(mem_req_id_, req);
      }
      if (pkt_size == bytes_left){
        inject_queue_->pop(); //the tail packet is being copied
      } else {
        //update the inject queue with the new state
        inject_queue_->adjustTop(byte_offset + pkt_size);
      }
    } else {
      nic_debug("packet of size=%" PRIu32 " at offset=%" PRIu64 " lacks buffer space to inject: %s",
                pkt_size, byte_offset, payload->toString().c_str());
      break; //no more room in NIC buffer
    }
  }
}

void
SnapprNIC::doSend(NetworkMessage* payload)
{
  nic_debug("snappr: sending %s", payload->toString().c_str());

  payload->setInjectionStarted(now());
  inject_queue_->insert(0, payload);
  copyToNicBuffer();

  int vl = payload->qos();

}

void
SnapprNIC::cqHandle(SnapprPacket* pkt)
{
  Flow* msg = cq_.recv(pkt);
  if (msg){
    NetworkMessage* netmsg = static_cast<NetworkMessage*>(msg);
    TimeDelta total_delay = now() - netmsg->timeStarted();
    TimeDelta congestion_delay = total_delay - netmsg->minDelay() - netmsg->injectionDelay();
    netmsg->setCongestionDelay(congestion_delay);
    //this is the last packet to arrive
    recvMessage(netmsg);
  }
  delete pkt;
}

void
SnapprNIC::eject(SnapprPacket* pkt)
{
  //on the eject side, assume cache injection for now
  //so we don't want to create memory traffic right now
  Timestamp now_ = now();
  if (now_ > ej_next_free_){
    ej_next_free_ = now_;
  } else {
    TimeDelta ejection_delay = ej_next_free_ - now_;
    pkt->accumulateCongestionDelay(ejection_delay);
  }

  pkt_debug("incoming packet - ejection next free at t=%8.4e: %s",
            ej_next_free_.sec(), pkt->toString().c_str());
  TimeDelta time_to_send; //TODO = pkt->byteLength() * inj_byte_delay_;
  ej_next_free_ = ej_next_free_ + time_to_send;
  auto qev = newCallback(this, &SnapprNIC::cqHandle, pkt);
  if (pkt->isTail()){
    NetworkMessage* netmsg = static_cast<NetworkMessage*>(pkt->flow());
    TimeDelta total_delay = ej_next_free_ - netmsg->timeStarted();
    TimeDelta min_delay = total_delay - netmsg->injectionDelay() 
                                      - pkt->congestionDelay() 
                                      - netmsg->congestionDelay();
    netmsg->setMinDelay(min_delay);
  }

  sendExecutionEvent(ej_next_free_, qev);
  if (flow_control_){
    auto* credit = new SnapprCredit(pkt->byteLength(), pkt->virtualLane(), switch_outport_);
    pkt_debug("crediting with switch port %d:%d for %" PRIu64 " offset=%" PRIu64,
              switch_outport_, pkt->virtualLane(), pkt->flowId(), pkt->offset());
    credit_link_->send(credit);
  }
}

void
SnapprNIC::handlePayload(Event *ev)
{
  SnapprPacket* pkt = static_cast<SnapprPacket*>(ev);

  TimeDelta time_to_send = pkt->byteLength() * inj_byte_delay_;
  if (time_to_send < pkt->timeToSend()){
    pkt_debug("delaying packet ejection - time to arrive=%10.4e, time to inject=%10.4e: %s",
              pkt->timeToSend().sec(), time_to_send.sec(), pkt->toString().c_str());
    //tail flit cannot arrive here before it leaves the prev switch
    auto ev = newCallback(this, &SnapprNIC::eject, pkt);
    TimeDelta delta_t = pkt->timeToSend() - time_to_send;
    sendDelayedExecutionEvent(delta_t, ev);
  } else {
    eject(pkt);
  }
}

void
SnapprNIC::handleCredit(Event *ev)
{
  SnapprCredit* credit = static_cast<SnapprCredit*>(ev);
  buffer_remaining_ += credit->numBytes();
  nic_debug("received %" PRIu32 " credits, buffer now %" PRIu64,
            credit->numBytes(), buffer_remaining_);
  copyToNicBuffer();
  //this transfers ownership - don't delete here
  outports_[credit->port()].handleCredit(credit);
}

void
SnapprNIC::handleTailPacket(Timestamp done, SnapprPacket* pkt)
{
  NetworkMessage* payload = static_cast<NetworkMessage*>(pkt->flow());
  TimeDelta min_send_delay = payload->byteLength() * inj_byte_delay_;
  TimeDelta actual_delay = done - payload->injectionStarted();
  TimeDelta inj_contend_delay = actual_delay - min_send_delay;
  payload->addInjectionDelay(inj_contend_delay);
  if (payload->needsAck()){
    NetworkMessage* ack = payload->cloneInjectionAck();
    auto* ev = newCallback(this, &NIC::sendToNode, ack);
    sendExecutionEvent(done, ev);
  }
  //congestion delays on the NIC outport should not be added here
  pkt->clearCongestionDelay();
}

void
SnapprNIC::injectPacket(uint32_t ptk_size, uint64_t byte_offset, NetworkMessage* payload)
{
  uint64_t bytes_left = payload->byteLength() - byte_offset;
  uint32_t pkt_size = std::min(bytes_left, uint64_t(packet_size_));
  bool is_tail = bytes_left == pkt_size;
  NodeId to = payload->toaddr();
  NodeId from = payload->fromaddr();
  uint64_t fid = payload->flowId();
  SnapprPacket* pkt = new SnapprPacket(is_tail ? payload : nullptr, pkt_size, is_tail,
                                       fid, byte_offset, to, from, payload->qos());
  pkt->setVirtualLane(payload->qos());

  //no multi-rail or multi-injection for now
  outports_[0].tryToSendPacket(pkt);
}

void
SnapprNIC::handleMemoryResponse(MemoryModel::Request* req)
{
  SnapprRequest* nreq = static_cast<SnapprRequest*>(req);
  nic_debug("received memory response for offset %" PRIu64 " for payload %s",
            nreq->offset, nreq->payload->toString().c_str());
  injectPacket(req->bytes, nreq->offset, nreq->payload);
  delete nreq;
}

struct FIFOQueue : public SnapprNIC::InjectionQueue {
 public:
  SPKT_REGISTER_DERIVED(
    SnapprNIC::InjectionQueue,
    FIFOQueue,
    "macro",
    "fifo",
    "implements a FIFO strategy for injecting packets")

  FIFOQueue(SST::Params& p){}

  std::pair<uint64_t, NetworkMessage*> top() override {
    return queue_.front();
  }

  void pop() override {
    queue_.pop();
  }

  void adjustTop(uint64_t offset) override {
    queue_.front().first = offset;
  }

  bool empty() const override {
    return queue_.empty();
  }

  void insert(uint64_t byte_offset, NetworkMessage *msg) override {
    queue_.emplace(byte_offset, msg);
  }

 private:
  std::queue<std::pair<uint64_t,NetworkMessage*>> queue_;

};

struct PriorityFIFOQueue : public SnapprNIC::InjectionQueue {
 public:
  SPKT_REGISTER_DERIVED(
    SnapprNIC::InjectionQueue,
    FIFOQueue,
    "macro",
    "priority_fifo",
    "implements a FIFO strategy for injecting packets")

  PriorityFIFOQueue(SST::Params& p){
    queues_.resize(p.find<int>("qos_levels", 1));
  }

  std::pair<uint64_t, NetworkMessage*> top() override {
    int qos = ready_queues_.top();
    return queues_[qos].front();
  }

  void pop() override {
    int qos = ready_queues_.top();
    queues_[qos].pop();
    if (queues_[qos].empty()){
      ready_queues_.pop();
    }
  }

  void adjustTop(uint64_t offset) override {
    int qos = ready_queues_.top();
    queues_[qos].front().first = offset;
  }

  bool empty() const override {
    return ready_queues_.empty();
  }

  void insert(uint64_t byte_offset, NetworkMessage *msg) override {
    queues_[msg->qos()].emplace(byte_offset, msg);
  }

 private:
  std::vector<std::queue<std::pair<uint64_t,NetworkMessage*>>> queues_;
  std::priority_queue<int, std::vector<int>> ready_queues_;

};


struct RoundRobinQueue : public SnapprNIC::InjectionQueue {

 public:
  SPKT_REGISTER_DERIVED(
    SnapprNIC::InjectionQueue,
    RoundRobinQueue,
    "macro",
    "round_robin",
    "implements a round-robin strategy for injecting packets")

  RoundRobinQueue(SST::Params& p) :
    begin_(0), end_(0), queue_(1024){}

  std::pair<uint64_t, NetworkMessage*> top() override {
    return queue_[begin_];
  }

  void pop() override {
    begin_++;
    if (begin_ == queue_.size()){
      begin_ = 0;
    }
  }

  bool empty() const override {
    return begin_ == end_;
  }

  void adjustTop(uint64_t offset) override {
    auto& e = queue_[begin_];
    insert(offset, e.second);
    pop();
  }

  void insert(uint64_t bytes, NetworkMessage* msg) override {
    queue_[end_] = {bytes,msg};
    end_++;
    if (end_ == queue_.size()){
      end_ = 0;
    }
  }

 private:
  int begin_;
  int end_;
  std::vector<std::pair<uint64_t,NetworkMessage*>> queue_;

};


}
} // end of namespace sstmac.
