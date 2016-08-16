/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
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
#include <sprockit/statics.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/util.h>

ImplementFactory(sstmac::hw::nic);
RegisterDebugSlot(nic);

RegisterNamespaces("nic", "message_sizes", "traffic_matrix",
                   "message_size_histogram");


#if SSTMAC_INTEGRATED_SST_CORE
#define DEFAULT_NEGLIGIBLE_SIZE 0
#else
#define DEFAULT_NEGLIGIBLE_SIZE 256
#endif

namespace sstmac {
namespace hw {

static sprockit::need_delete_statics<nic> del_statics;

nic::nic(sprockit::factory_type *interconn) :
  spy_num_messages_(nullptr),
  spy_bytes_(nullptr),
  hist_msg_size_(nullptr),
  local_bytes_sent_(nullptr),
  global_bytes_sent_(nullptr),
  interconn_(nullptr),
  parent_(nullptr),
  mtl_handler_(nullptr)
{
  if (interconn) interconn_ = safe_cast(interconnect, interconn);
}

nic::~nic()
{
  if (mtl_handler_) delete mtl_handler_;
  if (spy_bytes_) delete spy_bytes_;
  if (spy_num_messages_) delete spy_num_messages_;
  if (local_bytes_sent_) delete local_bytes_sent_;
  if (global_bytes_sent_) delete global_bytes_sent_;
  if (hist_msg_size_) delete hist_msg_size_;
}

void
nic::mtl_handle(event *ev)
{
  recv_message(static_cast<message*>(ev));
}

void
nic::init_factory_params(sprockit::sim_parameters *params)
{
  my_addr_ = node_id(params->get_int_param("id"));
  init_loc_id(event_loc_id(my_addr_));

  mtl_handler_ = new_handler(this, &nic::mtl_handle);

  negligible_size_ = params->get_optional_int_param("negligible_size", DEFAULT_NEGLIGIBLE_SIZE);

  spy_num_messages_ = optional_stats<stat_spyplot>(
        params, "traffic_matrix", "spyplot", "num_messages");
  spy_bytes_ = optional_stats<stat_spyplot>(
        params, "traffic_matrix", "spyplot", "bytes");

  if (params->has_namespace("local_bytes_sent")) {
    sprockit::sim_parameters* traffic_params = params->get_namespace("local_bytes_sent");
    local_bytes_sent_ = test_cast(stat_local_int, stat_collector_factory::get_optional_param("type", "local_int", traffic_params));
    local_bytes_sent_->set_id(my_addr_);
  }

  if (params->has_namespace("global_bytes_sent")) {
    sprockit::sim_parameters* traffic_params = params->get_namespace("global_bytes_sent");
    global_bytes_sent_ = test_cast(stat_global_int, stat_collector_factory::get_optional_param("type", "global_int", traffic_params));
    global_bytes_sent_->set_label("NIC Total Bytes Sent");
  }

  if (params->has_namespace("message_size_histogram")){
    sprockit::sim_parameters* size_params = params->get_namespace("message_size_histogram");
    hist_msg_size_ = test_cast(stat_histogram, stat_collector_factory::get_optional_param("type", "histogram", size_params));

    if (!hist_msg_size_){
      spkt_throw_printf(sprockit::value_error,
        "NIC message size tracker must be histogram, %s given",
        size_params->get_param("type").c_str());
    }
  }
}

void
nic::delete_statics()
{
}

void
nic::recv_message(message* msg)
{
  if (parent_->failed()){
    return;
  }

  nic_debug("receiving message %p:%s",
    msg, msg->to_string().c_str());

  network_message* netmsg = safe_cast(network_message, msg);

  nic_debug("handling message %s:%lu of type %s from node %d while running",
    netmsg->to_string().c_str(),
    uint64_t(netmsg->net_id()),
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
        "nic::handle: invalid message type %s",
        network_message::tostr(netmsg->type()));
    }
  }
}

void
nic::ack_send(network_message* payload)
{
  if (payload->needs_ack()){
    network_message* ack = payload->clone_injection_ack();
    nic_debug("acking payload %p:%s with ack %p",
      payload, payload->to_string().c_str(), ack);
    send_to_node(ack);
  }
}

void
nic::intranode_send(network_message* payload)
{
  //Stop recording for now
  //record_message(payload);

  nic_debug("intranode send payload %p:%s",
    payload, payload->to_string().c_str());

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

  send_to_node(payload);
  ack_send(payload);
}

#if SSTMAC_INTEGRATED_SST_CORE
void
nic::handle_event(SST::Event *ev)
{
  handle(static_cast<event*>(ev));
}
#endif

void
nic::record_message(network_message* netmsg)
{
  nic_debug("sending message %lu of size %ld of type %s to node %d: "
      "netid=%lu for %s",
      uint64_t(netmsg->net_id()),
      netmsg->byte_length(),
      network_message::tostr(netmsg->type()),
      int(netmsg->toaddr()),
      netmsg->unique_id(), netmsg->to_string().c_str());

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
  nic_debug("internode send payload %p:%s",
    netmsg, netmsg->to_string().c_str());
  if (negligible_size(netmsg->byte_length())){
    send_to_interconn(netmsg);
    ack_send(netmsg);
  } else {
    do_send(netmsg);
  }
}

void
nic::finalize_init()
{
}

void
nic::send_to_node(network_message* payload)
{
  schedule_now(parent_, payload);
}

void
nic::send_to_interconn(network_message* netmsg)
{
#if SSTMAC_INTEGRATED_SST_CORE
  spkt_throw(sprockit::unimplemented_error,
       "nic::send_to_interconn: integrated core");
#else
  safe_cast(interconnect, interconn_)->immediate_send(parent(), netmsg, now());
#endif
}

void
nic::set_event_parent(event_scheduler* m)
{
  connectable_subcomponent::set_event_parent(m);
#if !SSTMAC_INTEGRATED_SST_CORE
  if (spy_num_messages_) m->register_stat(spy_num_messages_);
  if (spy_bytes_) m->register_stat(spy_bytes_);
  if (hist_msg_size_) m->register_stat(hist_msg_size_);
  if (local_bytes_sent_) m->register_stat(local_bytes_sent_);
  if (global_bytes_sent_) m->register_stat(global_bytes_sent_);
#endif

}

}
} // end of namespace sstmac.

