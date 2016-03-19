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
#include <sstmac/hardware/packet_flow/packet_flow_nic.h>
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

packet_flow_nic::packet_flow_nic() :
 endpoint_(0),
 inj_buffer_(0),
 ej_buffer_(0),
 congestion_spyplot_(0),
 congestion_hist_(0),
 inj_handler_(0),
 injection_credits_(0)
{
}

void
packet_flow_nic::init_factory_params(sprockit::sim_parameters *params)
{
  nic::init_factory_params(params);

  acc_delay_ = params->get_optional_bool_param("accumulate_congestion_delay",false);

  if (params->has_namespace("congestion_delay_histogram")){
    sprockit::sim_parameters* congestion_params = params->get_namespace("congestion_delay_histogram");
    congestion_hist_ = test_cast(stat_histogram, stat_collector_factory::get_optional_param("type", "histogram", congestion_params));
    if (!congestion_hist_){
      spkt_throw_printf(sprockit::value_error,
        "congestion delay stats must be histogram, %s given",
        congestion_params->get_param("type").c_str());
    }
  }

  if (params->has_namespace("congestion_delay_matrix")){
    sprockit::sim_parameters* congestion_params = params->get_namespace("congestion_delay_matrix");
    congestion_spyplot_ = test_cast(stat_spyplot, stat_collector_factory::get_optional_param("type", "spyplot_png", congestion_params));
    if (!congestion_spyplot_){
      spkt_throw_printf(sprockit::value_error,
        "congestion matrix stats must be spyplot or spyplot_png, %s given",
        congestion_params->get_param("type").c_str());
    }
  }
  inj_bw_ = params->get_bandwidth_param("injection_bandwidth");
  inj_lat_ = params->get_time_param("injection_latency");

  buffer_size_ = params->get_optional_byte_length_param("eject_buffer_size", 1<<30);

  int one_vc = 1;
  //total hack for now, assume that the buffer itself has a low latency link to the switch
  timestamp small_latency(10e-9);
  packet_flow_bandwidth_arbitrator* inj_arb = packet_flow_bandwidth_arbitrator_factory::get_optional_param(
        "arbitrator", "cut_through", params);
  inj_arb->set_outgoing_bw(inj_bw_);
  inj_buffer_ = new packet_flow_injection_buffer(small_latency, inj_arb);

  //total hack for now, assume that the buffer has a delayed send, but ultra-fast credit latency
  packet_flow_bandwidth_arbitrator* ej_arb = packet_flow_bandwidth_arbitrator_factory::get_optional_param(
        "arbitrator", "cut_through", params);
  ej_arb->set_outgoing_bw(inj_bw_);
  ej_buffer_ = new packet_flow_eject_buffer(inj_lat_, small_latency, buffer_size_, ej_arb);

  endpoint_ =
    packet_flow_endpoint_factory::get_optional_param("arbitrator",
        "cut_through", params);
#if SSTMAC_INTEGRATED_SST_CORE
  injection_credits_ = params->get_byte_length_param("injection_credits");
#endif
}

//
// Goodbye.
//
packet_flow_nic::~packet_flow_nic() throw ()
{
  if (inj_buffer_) delete inj_buffer_;
  if (ej_buffer_) delete ej_buffer_;
  if (endpoint_) delete endpoint_;
  if (inj_handler_) delete inj_handler_;
}

void
packet_flow_nic::set_node(node* parent)
{
  nic::set_node(parent);
  if (inj_buffer_) inj_buffer_->set_acker(parent);
}

void
packet_flow_nic::finalize_init()
{
  if (my_addr_ == node_id()) {
    return; //just a template nic
  }

  endpoint_->set_exit(this);
  endpoint_->init_param1(addr());

  inj_buffer_->set_event_location(addr());
  ej_buffer_->set_event_location(addr());

  inj_buffer_->set_accumulate_delay(acc_delay_);
  ej_buffer_->set_accumulate_delay(acc_delay_);

  if (parent_) inj_buffer_->set_acker(parent_);

  nic::finalize_init();
}

void
packet_flow_nic::connect(
  int src_outport,
  int dst_inport,
  connection_type_t ty,
  connectable* mod)
{
  switch(ty) {
    case connectable::output: {
      if (src_outport != 0){
        spkt_throw(sprockit::illformed_error,
            "packet_flow_nic::connect: injection outport not zero");
      }
      set_injection_output(dst_inport, mod);
      break;
    }
    case connectable::input: {
      if (dst_inport != 0){
        spkt_throw(sprockit::illformed_error,
            "packet_flow_nic::connect: ejection outport not zero");
      }
      set_ejection_input(src_outport, mod);
      break;
    }
    default:
      nic::connect(src_outport, dst_inport, ty, mod);
      break;
  }
}

void
packet_flow_nic::recv_credit(event* credit)
{
  inj_buffer_->handle_credit(safe_cast(packet_flow_credit, credit));
}

void
packet_flow_nic::recv_packet(event* ev)
{
  packet* chunk = safe_cast(packet, ev);

  ej_buffer_->return_credit(chunk);

  if (congestion_hist_){
    congestion_hist_->collect(chunk->delay_us()*1e-6); //convert to seconds
  }
  if (congestion_spyplot_){
    long delay_ns = chunk->delay_us() * 1e3; //go to ns
    congestion_spyplot_->add(chunk->fromaddr(), chunk->toaddr(), delay_ns);
  }
  endpoint_->handle(chunk);
}

void
packet_flow_nic::set_injection_output(int inj_port, connectable* sw)
{
  event_handler* handler = safe_cast(event_handler, sw);
  inj_buffer_->set_output(0, inj_port, handler);
#if !SSTMAC_INTEGRATED_SST_CORE
  packet_flow_component* comp = safe_cast(packet_flow_component, sw);
  injection_credits_ = comp->initial_credits();
#endif
  inj_buffer_->init_credits(0, injection_credits_);
  if (!inj_handler_)
    inj_handler_ = ev_callback(event_location(), inj_buffer_, &packet_flow_buffer::start);
}

void
packet_flow_nic::set_ejection_input(int ej_port, connectable* sw)
{
  event_handler* handler = safe_cast(event_handler, sw);
  int only_port = 0;
  ej_buffer_->set_output(only_port, only_port, this);
  ej_buffer_->set_input(only_port, ej_port, handler);
}

void
packet_flow_nic::do_send(network_message* payload)
{
  nic_debug("packet flow: sending %s", payload->to_string().c_str());
  SCHEDULE_DELAY(inj_lat_, inj_handler_, payload);
}

void
packet_flow_nic::set_event_parent(event_scheduler* m)
{
  nic::set_event_parent(m);
  inj_buffer_->set_event_parent(m);
  ej_buffer_->set_event_parent(m);
  endpoint_->set_event_parent(m);
#if !SSTMAC_INTEGRATED_SST_CORE
  if (congestion_hist_) m->register_stat(congestion_hist_);
  if (congestion_spyplot_) m->register_stat(congestion_spyplot_);
#endif
}

void
packet_flow_netlink::connect(int src_outport, int dst_inport, connection_type_t ty, connectable *mod)
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
      debug_printf(sprockit::dbg::packet_flow,
           "netlink %d:%p handling payload %s",
            //topology::global()->label(event_location()).c_str(),
            int(id_), this, payload->to_string().c_str());
      node_id toaddr = payload->toaddr();
      netlink_id dst_netid(toaddr / num_eject_);
      routing_info::path& p = payload->rinfo().current_path();
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


