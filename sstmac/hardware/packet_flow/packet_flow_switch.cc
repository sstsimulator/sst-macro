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

#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/packet_flow/packet_flow_switch.h>
#include <sstmac/hardware/packet_flow/packet_flow_stats.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/switch/dist_dummyswitch.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sstmac/common/stats/stat_global_int.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/hardware/topology/topology.h>

RegisterNamespaces("congestion_stats");

namespace sstmac {
namespace hw {

ImplementSSTComponent("packet_flow", network_switch, packet_flow_switch,
  "A network switch implementing the packet flow congestion model");


packet_flow_abstract_switch::packet_flow_abstract_switch(
  sprockit::sim_parameters *params, uint64_t id, event_manager *mgr) :
  buf_stats_(nullptr),
  xbar_stats_(nullptr),
  router_(nullptr),
  network_switch(params, id, mgr)
{
  sprockit::sim_parameters* xbar_params = params->get_optional_namespace("xbar");

  xbar_stats_ = packet_stats_callback_factory::get_optional_param("stats", "null",
                                             xbar_params, this);

  sprockit::sim_parameters* buf_params = params->get_optional_namespace("output_buffer");
  buf_stats_ = packet_stats_callback_factory::get_optional_param("stats", "null",
                                             buf_params, this);

  router_ = router_factory::get_param("router", params, top_, this);

  sprockit::sim_parameters* ej_params = params->get_optional_namespace("ejection");
  std::vector<topology::injection_port> conns;
  top_->nodes_connected_to_ejection_switch(my_addr_, conns);
  for (topology::injection_port& conn : conns){
    sprockit::sim_parameters* port_params = topology::get_port_params(params, conn.port);
    ej_params->combine_into(port_params);
  }
}



packet_flow_abstract_switch::~packet_flow_abstract_switch()
{
  if (buf_stats_) delete buf_stats_;
  if (xbar_stats_) delete xbar_stats_;
  if (router_) delete router_;
}

packet_flow_switch::packet_flow_switch(
  sprockit::sim_parameters* params,
  uint64_t id,
  event_manager* mgr)
: packet_flow_abstract_switch(params, id, mgr),
  xbar_(nullptr)
{
  sprockit::sim_parameters* xbar_params = params->get_namespace("xbar");
  xbar_params->add_param_override("num_vc", router_->max_num_vc());
  xbar_ = new packet_flow_crossbar(xbar_params, this);
  xbar_->set_stat_collector(xbar_stats_);
  xbar_->configure_basic_ports(top_->max_num_ports());
#if !SSTMAC_INTEGRATED_SST_CORE
  payload_handler_ = new_link_handler(this, &packet_flow_switch::handle_payload);
  ack_handler_ = new_link_handler(this, &packet_flow_switch::handle_credit);
#endif
}

packet_flow_switch::~packet_flow_switch()
{
  if (xbar_) delete xbar_;
#if !SSTMAC_INTEGRATED_SST_CORE
  if (ack_handler_) delete ack_handler_;
  if (payload_handler_) delete payload_handler_;
#endif
  int nbuffers = out_buffers_.size();
  for (int i=0; i < nbuffers; ++i){
    packet_flow_sender* buf = out_buffers_[i];
    if (buf) delete buf;
  }
}

packet_flow_sender*
packet_flow_switch::output_buffer(sprockit::sim_parameters* params,
                                  int src_outport)
{
  if (out_buffers_.empty()) out_buffers_.resize(top_->max_num_ports());
  if (!out_buffers_[src_outport]){
    //debug_printf(sprockit::dbg::packet_flow | sprockit::dbg::packet_flow_config,
    //  "Switch %d: making buffer with bw=%10.6e on port=%d with buffer size %d going into buffer size %d",
    //  int(my_addr_), total_link_bw, port, src_buffer_size, dst_buffer_size);

    params->add_param_override("num_vc", router_->max_num_vc());
    packet_flow_network_buffer* out_buffer = new packet_flow_network_buffer(params, this);

    out_buffer->set_stat_collector(buf_stats_);
    out_buffers_[src_outport] = out_buffer;

    int buffer_inport = 0;
    xbar_->set_output(params, src_outport, buffer_inport,
                      out_buffer->payload_handler());
    out_buffer->set_input(params, buffer_inport,
                          src_outport, xbar_->credit_handler());
  }
  return out_buffers_[src_outport];
}

void
packet_flow_switch::connect_output(
  sprockit::sim_parameters* port_params,
  int src_outport,
  int dst_inport,
  event_handler* mod)
{
  //create an output buffer for the port
  packet_flow_sender* out_buffer = output_buffer(port_params, src_outport);
  out_buffer->set_output(port_params, src_outport, dst_inport, mod);
}

void
packet_flow_switch::connect_input(
  sprockit::sim_parameters* port_params,
  int src_outport,
  int dst_inport,
  event_handler* mod)
{
  xbar_->set_input(port_params, dst_inport, src_outport, mod);
}

void
packet_flow_switch::compatibility_check() const
{
  router_->compatibility_check();
}

void
packet_flow_switch::deadlock_check()
{
  xbar_->deadlock_check();
}

void
packet_flow_switch::deadlock_check(event *ev)
{
  xbar_->deadlock_check(ev);
}

int
packet_flow_switch::queue_length(int port) const
{
  packet_flow_buffer* buf = static_cast<packet_flow_buffer*>(out_buffers_[port]);
  return buf->queue_length();
}

void
packet_flow_switch::handle_credit(event *ev)
{
  packet_flow_credit* credit = static_cast<packet_flow_credit*>(ev);
  out_buffers_[credit->port()]->handle_credit(credit);
}

void
packet_flow_switch::handle_payload(event *ev)
{
  packet_flow_payload* payload = static_cast<packet_flow_payload*>(ev);
  router_->route(payload);
  xbar_->handle_payload(payload);
}

std::string
packet_flow_switch::to_string() const
{
  return sprockit::printf("packet flow switch %d", int(my_addr_));
}

link_handler*
packet_flow_switch::ack_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new SST::Event::Handler<packet_flow_switch>(const_cast<packet_flow_switch*>(this),
                          &packet_flow_switch::handle_credit);
#else
  return ack_handler_;
#endif
}

link_handler*
packet_flow_switch::payload_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new SST::Event::Handler<packet_flow_switch>(const_cast<packet_flow_switch*>(this),
                          &packet_flow_switch::handle_payload);
#else
  return payload_handler_;
#endif
}

}
}



