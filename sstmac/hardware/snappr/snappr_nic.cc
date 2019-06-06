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
#include <sprockit/errors.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

#include <stddef.h>

#define pkt_debug(...) \
  debug_printf(sprockit::dbg::snappr, "snappr NIC %d: %s", \
    int(addr()), sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

SnapprNIC::SnapprNIC(SST::Component* parent, SST::Params& params) :
  NIC(parent, params)
{
  SST::Params inj_params = params.find_scoped_params("injection");

  packet_size_ = inj_params.find<SST::UnitAlgebra>("mtu").getRoundedValue();
  inj_byte_delay_ = TimeDelta(inj_params.find<SST::UnitAlgebra>("bandwidth").getValue().inverse().toDouble());

  uint32_t credits = inj_params.find<SST::UnitAlgebra>("credits").getRoundedValue();

  int qosLevels = inj_params.find<int>("qos_levels", 1);
  credits_.resize(qosLevels);
  inject_queues_.resize(qosLevels);

  send_credits_ = inj_params.find<bool>("flow_control", true);

  uint32_t credits_per_vl = credits / qosLevels;
  for (int q=0; q < qosLevels; ++q){
    if (send_credits_){
      credits_[q] = credits_per_vl;
    } else {
      credits_[q] = std::numeric_limits<uint32_t>::max();
    }
  }
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
  Timestamp now_ = now();
  if (now_ > inj_next_free_){
    inj_next_free_ = now_;
  }

  NodeId to = payload->toaddr();
  NodeId from = payload->fromaddr();
  uint64_t fid = payload->flowId();

  uint64_t bytes_left = payload->byteLength() - byte_offset;
  int vl = 0; //only zero for now
  while (bytes_left > 0){
    uint32_t pkt_size = std::min(uint32_t(bytes_left), packet_size_);
    if (send_credits_){
      if (pkt_size > credits_[vl]){
        return byte_offset;
      } else {
        credits_[vl] -= pkt_size;
      }
    }
    bytes_left -= pkt_size;
    bool is_tail = bytes_left == 0;
    byte_offset += pkt_size;

    SnapprPacket* pkt = new SnapprPacket(is_tail ? payload : nullptr, pkt_size, is_tail,
                                         fid, to, from);
    pkt->setVirtualLane(0);
    pkt->setInport(switch_inport_);
    TimeDelta extra_delay = inj_next_free_ - now_;
    TimeDelta time_to_send = pkt_size * inj_byte_delay_;
    inj_next_free_ += time_to_send;
    pkt_debug("packet injecting at t=%8.4e: %s",
              inj_next_free_.sec(), pkt->toString().c_str());
    pkt->setTimeToSend(time_to_send);
    inj_link_->send(extra_delay, pkt);
  }

  if (payload->needsAck()){
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

  uint64_t byte_offset = inject(payload,0);
  int vl = 0;
  if (byte_offset < payload->byteLength()){
    inject_queues_[vl].emplace(payload, byte_offset);
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
    ej_next_free_ = now_;
  }
  pkt_debug("incoming packet - ejection next free at t=%8.4e: %s",
            ej_next_free_.sec(), pkt->toString().c_str());
  TimeDelta time_to_send = pkt->byteLength() * inj_byte_delay_;
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
  int vl = credit->virtualLane();
  credits_[vl] += credit->numBytes();
  if (!inject_queues_[vl].empty()){
    auto& pair = inject_queues_[vl].front();
    NetworkMessage* msg = pair.first;
    uint64_t byte_offset = pair.second;
    byte_offset = inject(msg, byte_offset);
    if (pair.first->byteLength() == byte_offset){
      inject_queues_[vl].pop();
    }
  }
  delete credit;
}


}
} // end of namespace sstmac.
