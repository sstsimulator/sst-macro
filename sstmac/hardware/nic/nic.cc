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

static sprockit::need_delete_statics<nic> del_statics;

nic::nic(sprockit::sim_parameters* params, node* parent) :
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
  connectable_subcomponent(parent) //no self events with NIC
{
  negligible_size_ = params->get_optional_int_param("negligible_size", DEFAULT_NEGLIGIBLE_SIZE);

  spy_num_messages_ = optional_stats<stat_spyplot>(parent,
        params, "traffic_matrix", "ascii", "num_messages");
  spy_bytes_ = optional_stats<stat_spyplot>(parent,
        params, "traffic_matrix", "ascii", "bytes");
  local_bytes_sent_ = optional_stats<stat_local_int>(parent,
        params, "local_bytes_sent", "local_int");
  global_bytes_sent_ = optional_stats<stat_global_int>(parent,
        params, "global_bytes_sent", "global_int");
  //global_bytes_sent_->set_label("NIC Total Bytes Sent");
  hist_msg_size_ = optional_stats<stat_histogram>(parent,
        params, "message_size_histogram", "histogram");
}

nic::~nic()
{
  //if (node_handler_) delete node_handler_;
  //if (event_mtl_handler_) delete event_mtl_handler_;
  //if (spy_bytes_) delete spy_bytes_;
  //if (spy_num_messages_) delete spy_num_messages_;
  //if (local_bytes_sent_) delete local_bytes_sent_;
  //if (global_bytes_sent_) delete global_bytes_sent_;
  //if (hist_msg_size_) delete hist_msg_size_;
#if !SSTMAC_INTEGRATED_SST_CORE
  //delete link_mtl_handler_;
#endif
}

event_handler*
nic::mtl_handler() const
{
  return new_handler(const_cast<nic*>(this), &nic::mtl_handle);
}

void
nic::mtl_handle(event *ev)
{
  recv_message(static_cast<network_message*>(ev));
}

void
nic::delete_statics()
{
}

std::function<void(network_message*)>
nic::ctrl_ioctl()
{
  return std::bind(&event_link::send, logp_link_, std::placeholders::_1);
}

std::function<void(network_message*)>
nic::data_ioctl()
{
  return std::bind(&nic::inject_send, this, std::placeholders::_1);
}

void
nic::inject_send(network_message* netmsg)
{
  if (netmsg->toaddr() == my_addr_){
    intranode_send(netmsg);
  } else {
    netmsg->put_on_wire();
    internode_send(netmsg);
  }
}

void
nic::recv_message(network_message* netmsg)
{
  nic_debug("handling message %s:%lu of type %s from node %d while running",
    netmsg->to_string().c_str(),
    netmsg->flow_id(),
    network_message::tostr(netmsg->type()),
    int(netmsg->fromaddr()));

  switch (netmsg->type()) {
    case network_message::rdma_get_request: {
      netmsg->nic_reverse(network_message::rdma_get_payload);
      netmsg->put_on_wire();
      internode_send(netmsg);
      break;
    }
    case network_message::nvram_get_request: {
      netmsg->nic_reverse(network_message::nvram_get_payload);
      //internode_send(netmsg);
      parent_->handle(netmsg);
      break;
    }
    case network_message::failure_notification:
    case network_message::rdma_get_sent_ack:
    case network_message::payload_sent_ack:
    case network_message::rdma_put_sent_ack: {
      //node_link_->send(netmsg);
      parent_->handle(netmsg);
      break;
    }
    case network_message::rdma_get_nack:
    case network_message::rdma_get_payload:
    case network_message::rdma_put_payload:
    case network_message::nvram_get_payload:
    case network_message::payload: {
      netmsg->take_off_wire();
      parent_->handle(netmsg);
      //node_link_->send(netmsg);
      break;
    }
    default: {
      spkt_throw_printf(sprockit::value_error,
        "nic::handle: invalid message type %s: %s",
        network_message::tostr(netmsg->type()), netmsg->to_string().c_str());
    }
  }
}

void
nic::ack_send(network_message* payload)
{
  if (payload->needs_ack()){
    network_message* ack = payload->clone_injection_ack();
    nic_debug("acking payload %s with ack %p",
      payload->to_string().c_str(), ack);
    send_to_node(ack);
  }
}

void
nic::intranode_send(network_message* payload)
{
  nic_debug("intranode send payload %s", payload->to_string().c_str());

  switch(payload->type())
  {
  case network_message::nvram_get_request:
    payload->nic_reverse(network_message::nvram_get_payload);
    break;
  case network_message::rdma_get_request:
    payload->nic_reverse(network_message::rdma_get_payload);
    break;
  default:
    break; //nothing to do
  }

  memory_model* mem = parent_->mem();
  //use 64 as a negligible number of compute bytes
  uint64_t byte_length = payload->byte_length();
  if (byte_length > 64){
    mem->access(payload->byte_length(),
                mem->max_single_bw(),
                new_callback(this, &nic::finish_memcpy, payload));
  } else {
    finish_memcpy(payload);
  }
}

void
nic::finish_memcpy(network_message* payload)
{
  ack_send(payload);
  payload->intranode_memmove();
  send_to_node(payload);
}

void
nic::record_message(network_message* netmsg)
{
  nic_debug("sending message %lu of size %ld of type %s to node %d: "
      "netid=%lu for %s",
      netmsg->flow_id(),
      netmsg->byte_length(),
      network_message::tostr(netmsg->type()),
      int(netmsg->toaddr()),
      netmsg->flow_id(), netmsg->to_string().c_str());

  if (netmsg->type() == network_message::null_netmsg_type){
    //assume this is a simple payload
    netmsg->set_type(network_message::payload);
  }

  if (spy_num_messages_) {
    spy_num_messages_->add_one(netmsg->fromaddr(),
                  netmsg->toaddr());
  }

  if (spy_bytes_) {
    spy_bytes_->add(netmsg->fromaddr(),
                    netmsg->toaddr(), netmsg->byte_length());
  }

  if (hist_msg_size_) {
    hist_msg_size_->collect(netmsg->byte_length());
  }

  if (local_bytes_sent_) {
    local_bytes_sent_->collect(netmsg->byte_length());
  }

  if (global_bytes_sent_) {
    global_bytes_sent_->collect(netmsg->byte_length());
  }
}

void
nic::internode_send(network_message* netmsg)
{
  record_message(netmsg);
  nic_debug("internode send payload of size %d %s",
    int(netmsg->byte_length()), netmsg->to_string().c_str());
  //we might not have a logp overlay network
  if (negligible_size(netmsg->byte_length())){
    ack_send(netmsg);
    logp_link_->send(netmsg);
  } else {
    do_send(netmsg);
  }
}

void
nic::send_to_node(network_message* payload)
{
  auto forward_ev = new_callback(parent_, &node::handle, payload);
  parent_->send_now_self_event_queue(forward_ev);
}

}
} // end of namespace sstmac.
