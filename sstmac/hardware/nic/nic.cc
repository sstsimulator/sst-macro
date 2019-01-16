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

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/logp/logp_switch.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sstmac/common/stats/stat_histogram.h>
#include <sstmac/common/stats/stat_local_int.h>
#include <sstmac/common/stats/stat_global_int.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/hardware/memory/memory_model.h>
#include <sprockit/statics.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/util.h>

RegisterDebugSlot(nic);

RegisterNamespaces("nic", "message_sizes", "traffic_matrix",
                   "message_size_histogram", "injection", "bytes");

RegisterKeywords(
{ "nic_name", "DEPRECATED: the type of NIC to use on the node" },
{ "network_spyplot", "DEPRECATED: the file root of all stats showing traffic matrix" },
{ "post_latency", "the latency of the NIC posting messages" },
);

#define DEFAULT_NEGLIGIBLE_SIZE 256

namespace sstmac {
namespace hw {

static sprockit::need_deleteStatics<NIC> del_statics;

NIC::NIC(SST::Params& params, Node* parent) :
  spy_num_messages_(nullptr),
  spy_bytes_(nullptr),
  hist_msg_size_(nullptr),
  local_bytes_sent_(nullptr),
  global_bytes_sent_(nullptr),
  parent_(parent),
  my_addr_(parent->addr()),
  logp_link_(nullptr),
  os_(parent->os()),
  queue_(parent->os()),
  ConnectableSubcomponent(parent) //no self events with NIC
{
  negligibleSize_ = params->get_optional_int_param("negligibleSize", DEFAULT_NEGLIGIBLE_SIZE);

  /** TODO stats
  spy_num_messages_ = optionalStats<StatSpyplot>(parent,
        params, "traffic_matrix", "ascii", "num_messages");
  spy_bytes_ = optionalStats<StatSpyplot>(parent,
        params, "traffic_matrix", "ascii", "bytes");
  local_bytes_sent_ = optionalStats<StatLocalInt>(parent,
        params, "local_bytes_sent", "local_int");
  global_bytes_sent_ = optionalStats<StatGlobalInt>(parent,
        params, "global_bytes_sent", "global_int");
  //global_bytes_sent_->setLabel("NIC Total Bytes Sent");
  hist_msg_size_ = optionalStats<StatHistogram>(parent,
        params, "message_size_histogram", "histogram");
  */
}

NIC::~NIC()
{
  //if (node_handler_) delete node_handler_;
  //if (event_mtlHandler_) delete event_mtlHandler_;
  //if (spy_bytes_) delete spy_bytes_;
  //if (spy_num_messages_) delete spy_num_messages_;
  //if (local_bytes_sent_) delete local_bytes_sent_;
  //if (global_bytes_sent_) delete global_bytes_sent_;
  //if (hist_msg_size_) delete hist_msg_size_;
#if !SSTMAC_INTEGRATED_SST_CORE
  //delete link_mtlHandler_;
#endif
}

EventHandler*
NIC::mtlHandler() const
{
  return newHandler(const_cast<NIC*>(this), &NIC::mtlHandle);
}

void
NIC::mtlHandle(Event *ev)
{
  nic_debug("MTL handle");
  recvMessage(static_cast<NetworkMessage*>(ev));
}

void
NIC::deleteStatics()
{
}

std::function<void(NetworkMessage*)>
NIC::ctrlIoctl()
{
  void (EventLink::*sendfxn)(Event*) = &EventLink::send;
  return std::bind(sendfxn, logp_link_, std::placeholders::_1);
}

std::function<void(NetworkMessage*)>
NIC::dataIoctl()
{
  return std::bind(&NIC::injectSend, this, std::placeholders::_1);
}

void
NIC::injectSend(NetworkMessage* netmsg)
{
  if (netmsg->toaddr() == my_addr_){
    intranodeSend(netmsg);
  } else {
    netmsg->putOnWire();
    internodeSend(netmsg);
  }
}

void
NIC::recvMessage(NetworkMessage* netmsg)
{
  nic_debug("handling message %s:%lu of type %s from node %d while running",
    netmsg->toString().c_str(),
    netmsg->flowId(),
    NetworkMessage::tostr(netmsg->type()),
    int(netmsg->fromaddr()));

  switch (netmsg->type()) {
    case NetworkMessage::rdma_get_request: {
      netmsg->nicReverse(NetworkMessage::rdma_get_payload);
      netmsg->putOnWire();
      internodeSend(netmsg);
      break;
    }
    case NetworkMessage::nvram_get_request: {
      netmsg->nicReverse(NetworkMessage::nvram_get_payload);
      //internodeSend(netmsg);
      parent_->handle(netmsg);
      break;
    }
    case NetworkMessage::failure_notification:
    case NetworkMessage::rdma_get_sent_ack:
    case NetworkMessage::payload_sent_ack:
    case NetworkMessage::rdma_put_sent_ack: {
      //node_link_->send(netmsg);
      parent_->handle(netmsg);
      break;
    }
    case NetworkMessage::rdma_get_nack:
    case NetworkMessage::rdma_get_payload:
    case NetworkMessage::rdma_put_payload:
    case NetworkMessage::nvram_get_payload:
    case NetworkMessage::payload: {
      netmsg->takeOffWire();
      parent_->handle(netmsg);
      //node_link_->send(netmsg);
      break;
    }
    default: {
      spkt_throw_printf(sprockit::value_error,
        "nic::handle: invalid message type %s: %s",
        NetworkMessage::tostr(netmsg->type()), netmsg->toString().c_str());
    }
  }
}

void
NIC::ackSend(NetworkMessage* payload)
{
  if (payload->needsAck()){
    NetworkMessage* ack = payload->cloneInjectionAck();
    nic_debug("acking payload %s with ack %p",
      payload->toString().c_str(), ack);
    sendToNode(ack);
  }
}

void
NIC::intranodeSend(NetworkMessage* payload)
{
  nic_debug("intranode send payload %s", payload->toString().c_str());

  switch(payload->type())
  {
  case NetworkMessage::nvram_get_request:
    payload->nicReverse(NetworkMessage::nvram_get_payload);
    break;
  case NetworkMessage::rdma_get_request:
    payload->nicReverse(NetworkMessage::rdma_get_payload);
    break;
  default:
    break; //nothing to do
  }

  MemoryModel* mem = parent_->mem();
  //use 64 as a negligible number of compute bytes
  uint64_t byte_length = payload->byteLength();
  if (byte_length > 64){
    mem->access(payload->byteLength(),
                mem->maxSingleBw(),
                newCallback(this, &NIC::finishMemcpy, payload));
  } else {
    finishMemcpy(payload);
  }
}

void
NIC::finishMemcpy(NetworkMessage* payload)
{
  ackSend(payload);
  payload->intranode_memmove();
  sendToNode(payload);
}

void
NIC::recordMessage(NetworkMessage* netmsg)
{
  nic_debug("sending message %lu of size %ld of type %s to node %d: "
      "netid=%lu for %s",
      netmsg->flowId(),
      netmsg->byteLength(),
      NetworkMessage::tostr(netmsg->type()),
      int(netmsg->toaddr()),
      netmsg->flowId(), netmsg->toString().c_str());

  if (netmsg->type() == NetworkMessage::null_netmsg_type){
    //assume this is a simple payload
    netmsg->setType(NetworkMessage::payload);
  }

  if (spy_num_messages_) {
    spy_num_messages_->addData(netmsg->fromaddr(), netmsg->toaddr(), 1);
  }

  if (spy_bytes_) {
    spy_bytes_->addData(netmsg->fromaddr(), netmsg->toaddr(), netmsg->byteLength());
  }

  if (hist_msg_size_) {
    hist_msg_size_->addData(netmsg->byteLength());
  }

  if (local_bytes_sent_) {
    local_bytes_sent_->addData(netmsg->byteLength());
  }

  if (global_bytes_sent_) {
    global_bytes_sent_->addData(netmsg->byteLength());
  }
}

void
NIC::internodeSend(NetworkMessage* netmsg)
{
  recordMessage(netmsg);
  nic_debug("internode send payload of size %d %s",
    int(netmsg->byteLength()), netmsg->toString().c_str());
  //we might not have a logp overlay network
  if (negligibleSize(netmsg->byteLength())){
    ackSend(netmsg);
    logp_link_->send(netmsg);
  } else {
    doSend(netmsg);
  }
}

void
NIC::sendToNode(NetworkMessage* payload)
{
  auto forward_ev = newCallback(parent_, &Node::handle, payload);
  parent_->sendExecutionEventNow(forward_ev);
}

}
} // end of namespace sstmac.
