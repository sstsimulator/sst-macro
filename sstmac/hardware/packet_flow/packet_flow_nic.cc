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

void
packet_flow_nic::init_factory_params(sprockit::sim_parameters *params)
{
  nic::init_factory_params(params);
  inj_lat_ = params->get_time_param("injection_latency");
  std::string default_arb = params->get_optional_param("arbitrator", "cut_through");
  packetizer_ = packetizer_factory::get_optional_param("packetizer", default_arb, params);
  packetizer_->setNotify(this);

  packet_flow_nic_packetizer* packer = test_cast(packet_flow_nic_packetizer, packetizer_);
  if (packer) packer->set_acker(mtl_handler());

#if SSTMAC_INTEGRATED_SST_CORE
  injection_credits_ = params->get_byte_length_param("injection_credits");
#endif
}

#if SSTMAC_INTEGRATED_SST_CORE
void
packet_flow_nic::init_sst_params(SST::Params &params, SST::Component* parent)
{
  packetizer_->init_sst_params(params, parent);
}

#endif

//
// Goodbye.
//
packet_flow_nic::~packet_flow_nic() throw ()
{
  if (packetizer_) delete packetizer_;
}

double
packet_flow_nic::injection_bandwidth() const
{
  packet_flow_nic_packetizer* packer = safe_cast(packet_flow_nic_packetizer, packetizer_);
  return packer->injection_bandwidth();
}

void
packet_flow_nic::connect(
  int src_outport,
  int dst_inport,
  connection_type_t ty,
  connectable* mod,
  config* cfg)
{
  packet_flow_nic_packetizer* packer = safe_cast(packet_flow_nic_packetizer, packetizer_);
  switch(ty) {
    case connectable::output: {
      if (src_outport != 0){
        spkt_throw(sprockit::illformed_error,
            "packet_flow_nic::connect: injection outport not zero");
      }
    #if !SSTMAC_INTEGRATED_SST_CORE
      packet_flow_component* comp = safe_cast(packet_flow_component, mod);
      injection_credits_ = comp->initial_credits();
    #endif
      packer->set_output(dst_inport, mod, injection_credits_);
      break;
    }
    case connectable::input: {
      if (dst_inport != 0){
        spkt_throw(sprockit::illformed_error,
            "packet_flow_nic::connect: ejection outport not zero");
      }
      packer->set_input(src_outport, mod);
      break;
    }
  }
}

void
packet_flow_nic::handle(event *ev)
{
  packetizer_->handle(ev);
}

void
packet_flow_nic::do_send(network_message* payload)
{
  nic_debug("packet flow: sending %s", payload->to_string().c_str());
  int vn = 0; //we only ever use one virtual network
  schedule_delay(inj_lat_, new_event(packetizer_, &packetizer::start, vn, payload));
}

void
packet_flow_nic::set_event_parent(event_scheduler* m)
{
  nic::set_event_parent(m);
  packetizer_->set_event_parent(m);
}

void
packet_flow_netlink::connect(int src_outport, int dst_inport,
  connection_type_t ty, connectable *mod,
  connectable::config* cfg)
{
  init();

  event_handler* conn = safe_cast(event_handler, mod);

  switch(ty)
  {
  case connectable::input:
  {
    block_->set_input(dst_inport, src_outport, conn);
    break;
  }
  case connectable::output:
  {
    block_->set_output(src_outport, dst_inport, conn);
    packet_flow_component* comp = safe_cast(packet_flow_component, mod);
    debug_printf(sprockit::dbg::packet_flow_config | sprockit::dbg::packet_flow,
      "%s initializing with %d credits on port %d",
       block_->to_string().c_str(), comp->initial_credits(), src_outport);
    block_->init_credits(src_outport, comp->initial_credits());
    break;
  }
  }

}

void
packet_flow_netlink::init_factory_params(sprockit::sim_parameters *params)
{
  netlink::init_factory_params(params);
}

void
packet_flow_netlink::deadlock_check()
{
  block_->deadlock_check();
}

void
packet_flow_netlink::set_event_parent(event_scheduler* m)
{
  block_->set_event_parent(m);
  netlink::set_event_parent(m);
}

void
packet_flow_netlink::handle(event* ev)
{
  packet_flow_interface* fmsg = interface_cast(packet_flow_interface, ev);
  switch (fmsg->type()) {
    case packet_flow_interface::credit: {
      packet_flow_credit* credit = static_cast<packet_flow_credit*>(ev);
      debug_printf(sprockit::dbg::packet_flow,
         "netlink %s:%p handling credit %s",
         topology::global()->label(event_location()).c_str(),
         this,
         credit->to_string().c_str());
      block_->handle_credit(credit);
      break;
    }
    case packet_flow_interface::payload: {
      packet_flow_payload* payload = static_cast<packet_flow_payload*>(ev);
      geometry_routable* rtbl = payload->interface<geometry_routable>();
      debug_printf(sprockit::dbg::packet_flow,
           "netlink %d:%p handling payload %s",
            //topology::global()->label(event_location()).c_str(),
            int(id_), this, payload->to_string().c_str());
      node_id toaddr = payload->toaddr();
      netlink_id dst_netid(toaddr / num_eject_);
      geometry_routable::path& p = rtbl->current_path();
      if (dst_netid == id_){
        //stays local - goes to a node
        int node_offset = toaddr % num_eject_;
        p.outport = netlink::node_port(node_offset);
        p.vc = 0;
        p.geometric_id = 0;
        debug_printf(sprockit::dbg::packet_flow,
         "netlink %d ejecting %s to node %d at offset %d to port %d\n",
            int(id_), ev->to_string().c_str(), int(toaddr), node_offset, p.outport);
      } else {
        //goes to switch
        p.outport = netlink::switch_port(tile_rotater_);
        p.vc = 0;
        debug_printf(sprockit::dbg::packet_flow,
         "netlink %d injecting msg %s to switch %d on redundant path %d of %d to port %d\n",
            int(id_), ev->to_string().c_str(), int(toaddr), tile_rotater_, num_inject_, p.outport);
        tile_rotater_ = (tile_rotater_ + 1) % num_inject_;
      }
      block_->handle_payload(payload);
      break;
    }
  }
}

void
packet_flow_netlink::init()
{
  if (inited_) return;

  inited_ = true;
  int really_big_buffer = 1<<30;
  int num_vc = 1;
  block_ = new packet_flow_crossbar(timestamp(0), timestamp(0), num_vc, really_big_buffer, "netlink");
  block_->set_event_location(id_);
  block_->configure_basic_ports(num_inject_ + num_eject_);
}

}
} // end of namespace sstmac.


