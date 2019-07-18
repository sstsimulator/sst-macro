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

SnapprNIC::SnapprNIC(SST::Component* parent, SST::Params& params) :
  NIC(parent, params), send_state_(ACTIVE), //basically, not stalled
  arbitrate_scheduled_(false)
{
  SST::Params inj_params = params.find_scoped_params("injection");

  packet_size_ = inj_params.find<SST::UnitAlgebra>("mtu").getRoundedValue();
  inj_byte_delay_ = TimeDelta(inj_params.find<SST::UnitAlgebra>("bandwidth").getValue().inverse().toDouble());

  uint32_t credits = inj_params.find<SST::UnitAlgebra>("credits").getRoundedValue();

  int qosLevels = inj_params.find<int>("qos_levels", 1);
  credits_.resize(qosLevels);
  inject_queues_.resize(qosLevels);
  std::string arb = params.find<std::string>("arbitration", "fifo");
  for (int i=0; i < qosLevels; ++i){
    inject_queues_[i] = sprockit::create<InjectionQueue>("macro", arb, params);
  }

  send_credits_ = inj_params.find<bool>("flow_control", true);

  uint32_t credits_per_vl = credits / qosLevels;
  for (int q=0; q < qosLevels; ++q){
    if (send_credits_){
      credits_[q] = credits_per_vl;
    } else {
      credits_[q] = std::numeric_limits<uint32_t>::max();
    }
  }

  ftq_active_send_state_ = FTQTag::allocateCategoryId("active:NIC_send");
  ftq_idle_send_state_ = FTQTag::allocateCategoryId("idle:NIC_send");
  ftq_stalled_send_state_ = FTQTag::allocateCategoryId("stalled:NIC_send");
  ftq_active_recv_state_ = FTQTag::allocateCategoryId("active:NIC_recv");
  ftq_idle_recv_state_ = FTQTag::allocateCategoryId("idle:NIC_recv");


  std::string subId = sprockit::printf("NIC:%d", addr());
  send_state_ftq_ = dynamic_cast<FTQCalendar*>(
        registerMultiStatistic<int,uint64_t,uint64_t>(inj_params, "send_state", subId));
  recv_state_ftq_ = dynamic_cast<FTQCalendar*>(
        registerMultiStatistic<int,uint64_t,uint64_t>(inj_params, "recv_state", subId));
  xmit_active_ = registerStatistic<uint64_t>(inj_params, "xmit_active", subId);
  xmit_idle_ = registerStatistic<uint64_t>(inj_params, "xmit_idle", subId);
  xmit_stall_ = registerStatistic<uint64_t>(inj_params, "xmit_stall", subId);
  bytes_sent_ = registerStatistic<uint64_t>(inj_params, "bytes_sent", subId);
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
    inj_link_ = std::move(link);
  } else if (src_outport == LogP) {
    logp_link_ = std::move(link);
  } else {
    spkt_abort_printf("Invalid switch port %d in PiscesNIC::connectOutput", src_outport);
  }
  switch_inport_ = dst_inport;
}

void
SnapprNIC::connectInput(int src_outport, int dst_inport, EventLink::ptr&& link)
{
  switch_outport_ = src_outport;
  credit_link_ = std::move(link);
}

uint64_t
SnapprNIC::inject(NetworkMessage* payload, uint64_t byte_offset)
{
  int vl = 0; //for now

  Timestamp now_ = now();
  if (now_ > inj_next_free_){
    if (send_state_ftq_){
      TimeDelta incr = now_ - inj_next_free_;
      if (send_state_ == STALLED){
        send_state_ftq_->addData(ftq_stalled_send_state_, inj_next_free_.time.ticks(), incr.ticks());
        xmit_stall_->addData(incr.ticks());
      } else {
        send_state_ftq_->addData(ftq_idle_send_state_, inj_next_free_.time.ticks(), incr.ticks());
        xmit_idle_->addData(incr.ticks());
      }
    }
    inj_next_free_ = now_;
  }

  uint64_t bytes_left = payload->byteLength() - byte_offset;
  uint32_t pkt_size = std::min(uint32_t(bytes_left), packet_size_);
  if (send_credits_){
    if (pkt_size > credits_[vl]){
      send_state_ = STALLED;
      return byte_offset;
    } else {
      credits_[vl] -= pkt_size;
    }
  }

  bytes_left -= pkt_size;
  pkt_debug("packet injecting at offset=%" PRIu64 ",left=%" PRIu64 " t=%8.4e: %s",
            byte_offset, bytes_left, inj_next_free_.sec(), payload->toString().c_str());

  byte_offset += pkt_size;

  bool is_tail = bytes_left == 0;
  NodeId to = payload->toaddr();
  NodeId from = payload->fromaddr();
  uint64_t fid = payload->flowId();
  SnapprPacket* pkt = new SnapprPacket(is_tail ? payload : nullptr, pkt_size, is_tail,
                                       fid, to, from);

  pkt->setVirtualLane(0);
  pkt->setInport(switch_inport_);
  TimeDelta extra_delay = inj_next_free_ - now_;
  TimeDelta time_to_send = pkt_size * inj_byte_delay_;
  if (send_state_ftq_){
    send_state_ftq_->addData(ftq_active_send_state_, inj_next_free_.time.ticks(), time_to_send.ticks());
  }
  xmit_active_->addData(time_to_send.ticks());
  inj_next_free_ += time_to_send;


  pkt->setTimeToSend(time_to_send);
  inj_link_->send(extra_delay, pkt);

  send_state_ = ACTIVE;


  if (bytes_left == 0 && payload->needsAck()){
    NetworkMessage* ack = payload->cloneInjectionAck();
    auto* ev = newCallback(this, &NIC::sendToNode, ack);
    sendExecutionEvent(inj_next_free_, ev);
  }

  return byte_offset;
}

void
SnapprNIC::doSend(NetworkMessage* payload)
{
  nic_debug("snappr: sending %s", payload->toString().c_str());

  bytes_sent_->addData(payload->byteLength());

  int vl = 0;
  if (!arbitrate_scheduled_ && inj_next_free_ <= now()){
    uint64_t byte_offset = inject(payload, 0);
    if (byte_offset < payload->byteLength()){
      inject_queues_[vl]->insert(byte_offset, payload);
      scheduleArbitration(vl);
    }
  } else {
    inject_queues_[vl]->insert(0, payload);
  }

}

void
SnapprNIC::cqHandle(SnapprPacket* pkt)
{
  Flow* msg = cq_.recv(pkt);
  if (msg){
    recvMessage(static_cast<NetworkMessage*>(msg));
  }
  delete pkt;
}

void
SnapprNIC::eject(SnapprPacket* pkt)
{
  Timestamp now_ = now();
  if (now_ > ej_next_free_){
    if (recv_state_ftq_){
      TimeDelta idle = now_ - ej_next_free_;
      recv_state_ftq_->addData(ftq_idle_recv_state_, ej_next_free_.time.ticks(), idle.ticks());
    }
    ej_next_free_ = now_;
  }
  pkt_debug("incoming packet - ejection next free at t=%8.4e: %s",
            ej_next_free_.sec(), pkt->toString().c_str());
  TimeDelta time_to_send = pkt->byteLength() * inj_byte_delay_;
  if (recv_state_ftq_){
    recv_state_ftq_->addData(ftq_active_recv_state_, ej_next_free_.time.ticks(), time_to_send.ticks());
  }
  ej_next_free_ = ej_next_free_ + time_to_send;
  auto qev = newCallback(this, &SnapprNIC::cqHandle, pkt);
  sendExecutionEvent(ej_next_free_, qev);
  if (send_credits_){
    credit_link_->send(new SnapprCredit(pkt->byteLength(), pkt->virtualLane(), switch_outport_));
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
SnapprNIC::arbitrate(int vl)
{
  nic_debug("snappr: arbitrating VL=%d", vl);

#if SSTMAC_SANITY_CHECK
  if (inject_queues_[vl]->empty()){
    spkt_abort_printf("NIC %d arbitrating empty queue on VL=%d", addr(), vl);
  }
#endif

  arbitrate_scheduled_ = false;
  auto pair = inject_queues_[vl]->top();
  uint64_t byte_offset = pair.first;
  NetworkMessage* msg = pair.second;
  byte_offset = inject(msg, byte_offset);
  if (send_state_ == ACTIVE){
    if (msg->byteLength() == byte_offset){
      inject_queues_[vl]->pop();
      if (!inject_queues_[vl]->empty()){
        scheduleArbitration(vl);
      }
    } else {
      inject_queues_[vl]->adjustTop(byte_offset);
      scheduleArbitration(vl);
    }
  }
  //else stalled - can't schedule another arbitration
}

void
SnapprNIC::scheduleArbitration(int vl)
{
  nic_debug("snappr: scheduling arbitration on VL=%d for %10.6e", vl, inj_next_free_.sec());
  if (arbitrate_scheduled_){
    spkt_abort_printf("cannot reschedule arbitration on Snappr NIC");
  }

  auto* ev = newCallback(this, &SnapprNIC::arbitrate, vl);
  sendExecutionEvent(inj_next_free_, ev);
  arbitrate_scheduled_ = true;
}

void
SnapprNIC::handleCredit(Event *ev)
{
  SnapprCredit* credit = static_cast<SnapprCredit*>(ev);
  int vl = credit->virtualLane();
  credits_[vl] += credit->numBytes();
  if (!arbitrate_scheduled_ && !inject_queues_[vl]->empty()){
    arbitrate(vl);
  }
  delete credit;
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
