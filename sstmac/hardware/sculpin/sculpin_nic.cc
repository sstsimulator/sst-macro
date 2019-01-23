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

#include <stddef.h>

#define pkt_debug(...) \
  debug_printf(sprockit::dbg::sculpin, "sculpin NIC %d: %s", \
    int(addr()), sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

SculpinNIC::SculpinNIC(SST::Params& params, Node* parent) :
  NIC(params, parent)
{
  SST::Params inj_params = params.get_namespace("injection");

  packet_size_ = inj_params.findUnits("mtu").getRoundedValue();
  inj_byte_delay_ = Timestamp(inj_params.findUnits("bandwidth").inverse().toDouble());

  //make port 0 a copy of the injection params
  SST::Params port0_params = params.find_prefix_params("port0");
  inj_params.combine_into(port0_params);
}

Timestamp
SculpinNIC::sendLatency(SST::Params& params) const
{
  return Timestamp(params.get_namespace("injection")->get_time_param("sendLatency"));
}

Timestamp
SculpinNIC::creditLatency(SST::Params& params) const
{
  return Timestamp(params.get_namespace("injection")->get_time_param("sendLatency"));
}

void
SculpinNIC::init(unsigned int phase)
{
}

void
SculpinNIC::setup()
{
  SubComponent::setup();
}

SculpinNIC::~SculpinNIC() throw ()
{
  if (inj_link_) delete inj_link_;  
}

LinkHandler*
SculpinNIC::payloadHandler(int port)
{
  if (port == NIC::LogP){
    return newLinkHandler(this, &NIC::mtlHandle);
  } else {
    return newLinkHandler(this, &SculpinNIC::handlePayload);
  }
}

LinkHandler*
SculpinNIC::creditHandler(int port)
{
  return newLinkHandler(this, &SculpinNIC::handleCredit);
}

void
SculpinNIC::connectOutput(
  SST::Params& params,
  int src_outport,
  int dst_inport,
  EventLink* link)
{
  if (src_outport == Injection){
    inj_link_ = link;
  } else if (src_outport == LogP) {
    logp_link_ = link;
  } else {
    spkt_abort_printf("Invalid switch port %d in PiscesNIC::connectOutput", src_outport);
  }
}

void
SculpinNIC::connectInput(
  SST::Params& params,
  int src_outport,
  int dst_inport,
  EventLink* link)
{
  //nothing to do
  //but we own the link now so have to delete it
  delete link;
}

void
SculpinNIC::doSend(NetworkMessage* payload)
{
  nic_debug("sculpin: sending %s", payload->toString().c_str());

  uint64_t bytes_left = payload->byteLength();
  uint64_t byte_offset = 0;

  GlobalTimestamp now_ = now();
  if (now_ > inj_next_free_){
    inj_next_free_ = now_;
  }

  NodeId to = payload->toaddr();
  NodeId from = payload->fromaddr();
  uint64_t fid = payload->flowId();
  while (bytes_left > 0){
    uint32_t pkt_size = std::min(uint32_t(bytes_left), packet_size_);
    bytes_left -= pkt_size;
    bool is_tail = bytes_left == 0;
    byte_offset += pkt_size;
    SculpinPacket* pkt = new SculpinPacket(is_tail ? payload : nullptr, pkt_size, is_tail,
                                             fid, to, from);
    Timestamp extra_delay = inj_next_free_ - now_;
    Timestamp time_to_send = pkt_size * inj_byte_delay_;
    inj_next_free_ += time_to_send;
    pkt_debug("packet injecting at t=%8.4e: %s",
              inj_next_free_.sec(), pkt->toString().c_str());
    pkt->setTimeToSend(time_to_send);
    inj_link_->send(extra_delay, pkt);
  }

  if (payload->needsAck()){
    NetworkMessage* ack = payload->cloneInjectionAck();
    auto ev = newCallback(this, &NIC::sendToNode, ack);
    sendExecutionEvent(inj_next_free_, ev);
  }
}

void
SculpinNIC::cqHandle(SculpinPacket* pkt)
{
  Flow* msg = cq_.recv(pkt);
  if (msg){
    recvMessage(static_cast<NetworkMessage*>(msg));
  }
  delete pkt;
}

void
SculpinNIC::eject(SculpinPacket* pkt)
{
  GlobalTimestamp now_ = now();
  if (now_ > ej_next_free_){
    ej_next_free_ = now_;
  }
  pkt_debug("incoming packet - ejection next free at t=%8.4e: %s",
            ej_next_free_.sec(), pkt->toString().c_str());
  Timestamp time_to_send = pkt->byteLength() * inj_byte_delay_;
  ej_next_free_ = ej_next_free_ + time_to_send;
  auto qev = newCallback(this, &SculpinNIC::cqHandle, pkt);
  sendExecutionEvent(ej_next_free_, qev);
}

void
SculpinNIC::handlePayload(Event *ev)
{
  SculpinPacket* pkt = static_cast<SculpinPacket*>(ev);

  Timestamp time_to_send = pkt->byteLength() * inj_byte_delay_;
  if (time_to_send < pkt->timeToSend()){
    //tail flit cannot arrive here before it leaves the prev switch
    auto ev = newCallback(this, &SculpinNIC::eject, pkt);
    Timestamp delta_t = pkt->timeToSend() - time_to_send;
    sendDelayedExecutionEvent(delta_t, ev);
  } else {
    eject(pkt);
  }
}

void
SculpinNIC::handleCredit(Event *ev)
{
  spkt_abort_printf("SculpinNIC::handleCredit: should not handle credits in sculpin model");
}


}
} // end of namespace sstmac.
