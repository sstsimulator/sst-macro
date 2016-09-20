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

#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/router/routable.h>
#include <sstmac/hardware/packet_flow/packet_flow_nic.h>
#include <sstmac/hardware/node/node.h>
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

SpktRegister("packet_flow", nic, packet_flow_nic,
            "implements a nic that models messages as a packet flow");

SpktRegister("packet_flow", netlink, packet_flow_netlink,
            "implements a netlink that models messages as a packet flow");

const int packet_flow_netlink::really_big_buffer = 1<<30;

packet_flow_nic::packet_flow_nic(sprockit::sim_parameters* params, node* parent) :
  nic(params, parent),
  packetizer_(nullptr)
{
  sprockit::sim_parameters* inj_params = params->get_namespace("injection");

  packetizer_ = safe_cast(packet_flow_nic_packetizer,
       packetizer_factory::get_optional_param("packetizer", "cut_through",
                                              inj_params, parent, this));
  packetizer_->setNotify(this);
  packetizer_->set_acker(mtl_handler());

  //make port 0 a copy of the injection params
  sprockit::sim_parameters* port0_params = params->get_optional_namespace("port0");
  inj_params->combine_into(port0_params);

#if !SSTMAC_INTEGRATED_SST_CORE
  ack_handler_ = new_link_handler(packetizer_,
                             &packet_flow_nic_packetizer::recv_credit);

  payload_handler_ = new_link_handler(packetizer_,
                             &packet_flow_nic_packetizer::recv_packet);
#endif
}

//
// Goodbye.
//
packet_flow_nic::~packet_flow_nic() throw ()
{
  if (packetizer_) delete packetizer_;
#if !SSTMAC_INTEGRATED_SST_CORE
  delete ack_handler_;
  delete payload_handler_;
#endif
}

link_handler*
packet_flow_nic::payload_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  if (port == topology::speedy_port){
    return new_link_handler(const_cast<packet_flow_nic*>(this),
                            &nic::mtl_handle);
  } else {
    return new_link_handler(packetizer_,
                &packet_flow_nic_packetizer::recv_packet);
  }
#else
  if (port == topology::speedy_port){
    return link_mtl_handler_;
  } else {
    return payload_handler_;
  }
#endif
}

link_handler*
packet_flow_nic::ack_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new_link_handler(packetizer_,
                 &packet_flow_nic_packetizer::recv_credit);
#else
  return ack_handler_;
#endif
}

void
packet_flow_nic::connect_output(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_handler* mod)
{
  packet_flow_nic_packetizer* packer = safe_cast(packet_flow_nic_packetizer, packetizer_);
  packer->set_output(params, dst_inport, mod);
}

void
packet_flow_nic::connect_input(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_handler* mod)
{
  if (!mod) abort();
  packet_flow_nic_packetizer* packer = safe_cast(packet_flow_nic_packetizer, packetizer_);
  packer->set_input(params, src_outport, mod);
}

void
packet_flow_nic::do_send(network_message* payload)
{
  nic_debug("packet flow: sending %s", payload->to_string().c_str());
  int vn = 0; //we only ever use one virtual network
  packetizer_->start(vn, payload);
  //schedule_delay(inj_lat_, new_callback(packetizer_, &packetizer::start, vn, payload));
}

void
packet_flow_netlink::connect_output(
  sprockit::sim_parameters* params,
  int src_outport, int dst_inport,
  event_handler *mod)
{
  block_->set_output(params, src_outport, dst_inport, mod);
}

void
packet_flow_netlink::connect_input(
  sprockit::sim_parameters* params,
  int src_outport, int dst_inport,
  event_handler* mod)
{
  block_->set_input(params, dst_inport, src_outport, mod);
}

packet_flow_netlink::packet_flow_netlink(sprockit::sim_parameters *params, node *parent)
  : netlink(params, parent),
  block_(nullptr),
  tile_rotater_(0)
{
  block_ = new packet_flow_crossbar(params, parent);
  block_->configure_basic_ports(num_inject_ + num_eject_);
#if !SSTMAC_INTEGRATED_SST_CORE
  ack_handler_ = new_link_handler(this,
                             &packet_flow_netlink::handle_credit);

  payload_handler_ = new_link_handler(this,
                             &packet_flow_netlink::handle_payload);
#endif
}

void
packet_flow_netlink::deadlock_check()
{
  block_->deadlock_check();
}

link_handler*
packet_flow_netlink::payload_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new_link_handler(const_cast<packet_flow_netlink*>(this),
             &packet_flow_netlink::handle_payload);
#else
  return payload_handler_;
#endif
}

link_handler*
packet_flow_netlink::ack_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new_link_handler(const_cast<packet_flow_netlink*>(this),
             &packet_flow_netlink::handle_credit);
#else
  return payload_handler_;
#endif
}

void
packet_flow_netlink::handle_credit(event* ev)
{
  packet_flow_credit* credit = static_cast<packet_flow_credit*>(ev);
  debug_printf(sprockit::dbg::packet_flow,
     "netlink %s:%p handling credit %s",
     topology::global()->label(event_location()).c_str(),
     this,
     credit->to_string().c_str());
  block_->handle_credit(credit);
}

void
packet_flow_netlink::handle_payload(event* ev)
{
  packet_flow_payload* payload = static_cast<packet_flow_payload*>(ev);
  routable* rtbl = payload->interface<routable>();
  debug_printf(sprockit::dbg::packet_flow,
       "netlink %d:%p handling payload %s",
        //topology::global()->label(event_location()).c_str(),
        int(id_), this, payload->to_string().c_str());
  node_id toaddr = payload->toaddr();
  netlink_id dst_netid(toaddr / num_eject_);
  routable::path& p = rtbl->current_path();
  if (dst_netid == id_){
    //stays local - goes to a node
    int node_offset = toaddr % num_eject_;
    p.outport = netlink::node_port(node_offset);
    p.vc = 0;
    p.geometric_id = 0;
    debug_printf(sprockit::dbg::packet_flow,
     "netlink %d ejecting %s to node %d at offset %d to port %d\n",
        int(id_), sprockit::to_string(ev).c_str(), int(toaddr), node_offset, p.outport);
  } else {
    //goes to switch
    p.outport = netlink::switch_port(tile_rotater_);
    p.vc = 0;
    debug_printf(sprockit::dbg::packet_flow,
     "netlink %d injecting msg %s to switch %d on redundant path %d of %d to port %d\n",
        int(id_), sprockit::to_string(ev).c_str(),
        int(toaddr), tile_rotater_, num_inject_, p.outport);
    tile_rotater_ = (tile_rotater_ + 1) % num_inject_;
  }
  block_->handle_payload(payload);
}

packet_flow_netlink::~packet_flow_netlink()
{
  if (block_) delete block_;
#if !SSTMAC_INTEGRATED_SST_CORE
  delete ack_handler_;
  delete payload_handler_;
#endif
}

}
} // end of namespace sstmac.


