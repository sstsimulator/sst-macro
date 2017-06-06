/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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
"nic_name",
"network_spyplot",
"post_bandwidth",
"post_latency",
"pipeline_fraction",
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
  logp_switch_(nullptr),
  event_mtl_handler_(nullptr),
  my_addr_(parent->addr()),
  nic_pipeline_multiplier_(0.),
  post_inv_bw_(0),
  post_latency_(0),
  next_free_(0),
  connectable_subcomponent(parent) //no self events with NIC
{
  event_mtl_handler_ = new_handler(this, &nic::mtl_handle);
  node_handler_ = new_handler(parent, &node::handle);

  if (params->has_param("post_latency")){
    post_latency_ = params->get_time_param("post_latency");
    sprockit::sim_parameters* inj_params = params->get_namespace("injection");
    double bw = inj_params->get_bandwidth_param("bandwidth");
    post_inv_bw_ = 1.0 / bw;
    //by default, assume very little contention on the nic
    double nic_pipeline_fraction = params->get_double_param("pipeline_fraction");
    //we multiply by the percent that is NOT pipelineable
    nic_pipeline_multiplier_ = 1.0 - nic_pipeline_fraction;
  }

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

#if !SSTMAC_INTEGRATED_SST_CORE
  link_mtl_handler_ = new_handler(this, &nic::mtl_handle);
#endif
}

nic::~nic()
{
  if (node_handler_) delete node_handler_;
  if (event_mtl_handler_) delete event_mtl_handler_;
  if (spy_bytes_) delete spy_bytes_;
  if (spy_num_messages_) delete spy_num_messages_;
  if (local_bytes_sent_) delete local_bytes_sent_;
  if (global_bytes_sent_) delete global_bytes_sent_;
  if (hist_msg_size_) delete hist_msg_size_;
#if !SSTMAC_INTEGRATED_SST_CORE
  delete link_mtl_handler_;
#endif
}

void
nic::mtl_handle(event *ev)
{
  recv_message(static_cast<message*>(ev));
}

void
nic::delete_statics()
{
}

void
nic::inject_send(network_message* netmsg, sw::operating_system* os)
{
  long bytes = netmsg->byte_length();
  timestamp delay = post_latency_ + timestamp(post_inv_bw_ * bytes);
  timestamp nic_ready = next_free_ + delay;
  next_free_ = next_free_ + delay * nic_pipeline_multiplier_;
  os->sleep_until(nic_ready);

  if (netmsg->toaddr() == my_addr_){
    intranode_send(netmsg);
  } else {
    internode_send(netmsg);
  }
}

void
nic::recv_message(message* msg)
{
  if (parent_->failed()){
    return;
  }

  nic_debug("receiving message %s",
    msg->to_string().c_str());

  network_message* netmsg = safe_cast(network_message, msg);

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
      internode_send(netmsg);
      break;
    }
    case network_message::rdma_get_sent_ack:
    case network_message::payload_sent_ack:
    case network_message::rdma_put_sent_ack: {
      parent_->handle(netmsg);
      break;
    }
    case network_message::failure_notification: {
      parent_->handle(netmsg);
      break;
    }
    case network_message::rdma_get_nack:
    case network_message::rdma_get_payload:
    case network_message::rdma_put_payload:
    case network_message::nvram_get_payload:
    case network_message::payload: {
      send_to_node(netmsg);
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
  nic_debug("intranode send payload %s",
    payload->to_string().c_str());

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
  long byte_length = payload->byte_length();
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
  nic_debug("internode send payload %s",
    netmsg->to_string().c_str());
  //we might not have a logp overlay network
  if (logp_switch_ && negligible_size(netmsg->byte_length())){
    send_to_link(logp_switch_, netmsg);
    ack_send(netmsg);
  } else {
    do_send(netmsg);
  }
}

void
nic::send_to_logp_switch(network_message* netmsg)
{
  nic_debug("send to logP switch %s",
    netmsg->to_string().c_str());
  //we might not have a logp overlay network
  if (logp_switch_) send_to_link(logp_switch_, netmsg);
  else do_send(netmsg);
}

void
nic::send_to_node(network_message* payload)
{
  schedule_now(node_handler_, payload);
}

}
} // end of namespace sstmac.