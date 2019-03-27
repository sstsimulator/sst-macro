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

PiscesNIC::PiscesNIC(SST::Params& params, Node* parent) :
  NIC(params, parent),
  pending_inject_(1)
{
  SST::Params inj_params = params.find_scoped_params("injection");
  SST::Params ej_params = params.find_scoped_params("ejection");

  self_mtl_link_ = allocateSubLink("mtl", Timestamp(), parent,
                                    newLinkHandler(this, &NIC::mtlHandle));

  inj_credits_ = inj_params.find<SST::UnitAlgebra>("credits").getRoundedValue();
  auto arb = inj_params.find<std::string>("arbitrator");
  double inj_bw = inj_params.find<SST::UnitAlgebra>("bandwidth").getValue().toDouble();
  packet_size_ = inj_params.find<SST::UnitAlgebra>("mtu").getRoundedValue();

  //PiscesSender::configurePayloadPortLatency(inj_params);
  auto buf_name = sprockit::printf("injection%d",parent->addr());
  inj_buffer_ = new PiscesBuffer(buf_name, arb, inj_bw, packet_size_, parent, 1/*single vc for inj*/);
}

void
PiscesNIC::init(unsigned int phase)
{
  inj_buffer_->init(phase);
}

void
PiscesNIC::setup()
{
  SubComponent::setup();
  inj_buffer_->setup();
}

PiscesNIC::~PiscesNIC() throw ()
{
  if (inj_buffer_) delete inj_buffer_;
}

LinkHandler*
PiscesNIC::payloadHandler(int port)
{
  if (port == NIC::LogP){
    return newLinkHandler(this, &NIC::mtlHandle);
  } else {
    return newLinkHandler(this, &PiscesNIC::incomingPacket);
  }
}

LinkHandler*
PiscesNIC::creditHandler(int port)
{
  return newLinkHandler(this, &PiscesNIC::packetSent);
}

void
PiscesNIC::connectOutput(int src_outport, int dst_inport, EventLink* link)
{
  if (src_outport == Injection){
    inj_buffer_->setOutput(src_outport, dst_inport, link, inj_credits_);
  } else if (src_outport == LogP) {
    logp_link_ = link;
  } else {
    spkt_abort_printf("Invalid port %d in PiscesNIC::connectOutput", src_outport);
  }
}

void
PiscesNIC::connectInput(int src_outport, int dst_inport, EventLink* link)
{
  if (dst_inport == Injection){ //the logp port is not for credits!
    credit_link_ = link;
  }
}

uint64_t
PiscesNIC::inject(int vn, uint64_t offset, NetworkMessage* netmsg)
{
  pisces_debug("On %s, trying to inject %s: %d credits available",
               toString().c_str(), netmsg->toString().c_str(),
               inj_buffer_->numCredit(vn));
  while (inj_buffer_->spaceToSend(vn, packet_size_)){
    uint64_t bytes = std::min(uint64_t(packet_size_), netmsg->byteLength() - offset);
    bool is_tail = (offset + bytes) == netmsg->byteLength();
    //only carry the payload if you're the tail packet
    PiscesPacket* payload = new PiscesPacket(is_tail ? netmsg : nullptr,
                                               bytes, netmsg->flowId(), is_tail,
                                               netmsg->fromaddr(), netmsg->toaddr());
    //start on a singel virtual channel (0)
    payload->setDeadlockVC(0);
    payload->updateVC();
    payload->resetStages(0);
    payload->setInport(0);
    GlobalTimestamp t = inj_buffer_->sendPayload(payload);

    pisces_debug("On %s, injecting from %lu->%lu on %s",
                 toString().c_str(), offset, offset + bytes, netmsg->toString().c_str());

    offset += bytes;

    if (offset == netmsg->byteLength()){
      if (netmsg->needsAck()){
        sendExecutionEvent(t, newCallback(this, &NIC::mtlHandle,
                     new NicEvent(netmsg->cloneInjectionAck())));
      }
      return offset;
    }
  }
  return offset;
}

void
PiscesNIC::doSend(NetworkMessage* netmsg)
{
  nic_debug("packet flow: sending %s", netmsg->toString().c_str());
  int vn = 0; //we only ever use one virtual network


  uint64_t offset = inject(vn, 0, netmsg);
  if (offset < netmsg->byteLength()){
    pending_inject_[vn].emplace(offset, netmsg->byteLength(), netmsg);
  }
}

void
PiscesNIC::packetArrived(PiscesPacket* pkt)
{
  auto* msg = completion_queue_.recv(pkt);
  if (msg){
    recvMessage(static_cast<NetworkMessage*>(msg));
  }
  delete pkt;
  int buffer_port = 0;
  PiscesCredit* credit = new PiscesCredit(buffer_port, pkt->vc(), pkt->byteLength());
  credit_link_->send(credit);
}

void
PiscesNIC::packetSent(Event* ev)
{
  pisces_debug("On %s, packet sent notification", toString().c_str());
  PiscesCredit* credit = safe_cast(PiscesCredit, ev);
  int vc = credit->vc();
  inj_buffer_->handleCredit(ev);
  while (!pending_inject_[vc].empty()){
    auto& pending = pending_inject_[vc].front();
    inj_buffer_->collectIdleTicks();
    pending.bytes_sent = inject(vc, pending.bytes_sent, pending.msg);
    if (pending.bytes_sent == pending.msg->byteLength()){
      pending_inject_[vc].pop();
    } else {
      //ran out of space - jump out
      return;
    }
  }
}

void
PiscesNIC::incomingPacket(Event* ev)
{
  PiscesPacket* pkt = safe_cast(PiscesPacket, ev);
//depending on arbitration, this might only by the head flit
  if (pkt->byteDelay().ticks() != 0){
    //these are pipelined
    Timestamp delay = pkt->byteLength() * pkt->byteDelay();
    nic_debug("delaying packet arrival of size %u for %9.5e secs", pkt->byteLength(), delay.sec());
    sendDelayedExecutionEvent(delay, newCallback(this, &PiscesNIC::packetArrived, pkt));
  } else {
    packetArrived(pkt);
  }
}


}
} // end of namespace sstmac.
