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
#include <sstmac/hardware/pisces/pisces_switch.h>
#include <sstmac/hardware/pisces/pisces_stats.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/switch/dist_dummyswitch.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sstmac/common/stats/stat_global_int.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/hardware/topology/topology.h>

RegisterNamespaces("switch", "router", "congestion_stats", "xbar", "link",
                   "output_buffer");

RegisterKeywords(
"stats",
"send_latency",
"credit_latency",
"credits",
"num_vc",
"pisces_arbitrator",
"pisces_network_hop_latency",
"pisces_network_link_bandwidth",
"pisces_switch_crossbar_bandwidth",
"network_switch_type",
"network_link_bandwidth",
"eject_buffer_size",
"output_buffer_size",
"input_buffer_size",
);

namespace sstmac {
namespace hw {

SpktRegister("pisces", network_switch, pisces_switch,
  "A network switch implementing the packet flow congestion model");

pisces_abstract_switch::pisces_abstract_switch(
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

  sprockit::sim_parameters* rtr_params = params->get_optional_namespace("router");
  rtr_params->add_param_override("id", int(my_addr_));
  router_ = router_factory::get_param("name", rtr_params, top_, this);

  sprockit::sim_parameters* ej_params = params->get_optional_namespace("ejection");
  std::vector<topology::injection_port> conns;
  top_->nodes_connected_to_ejection_switch(my_addr_, conns);
  if (!ej_params->has_param("credits")){
    //never be limited by credits
    ej_params->add_param_override("credits", "1GB");
  }

  pisces_sender::configure_payload_port_latency(ej_params);

  for (topology::injection_port& conn : conns){
    sprockit::sim_parameters* port_params = topology::get_port_params(params, conn.port);
    ej_params->combine_into(port_params);
  }
}



pisces_abstract_switch::~pisces_abstract_switch()
{
  if (buf_stats_) delete buf_stats_;
  if (xbar_stats_) delete xbar_stats_;
  if (router_) delete router_;
}

pisces_switch::pisces_switch(
  sprockit::sim_parameters* params,
  uint64_t id,
  event_manager* mgr)
: pisces_abstract_switch(params, id, mgr),
  xbar_(nullptr)
{
  sprockit::sim_parameters* xbar_params = params->get_namespace("xbar");
  xbar_params->add_param_override("num_vc", router_->max_num_vc());
  xbar_ = new pisces_crossbar(xbar_params, this);
  xbar_->set_stat_collector(xbar_stats_);
  xbar_->configure_basic_ports(top_->max_num_ports());
#if !SSTMAC_INTEGRATED_SST_CORE
  payload_handler_ = new_handler(this, &pisces_switch::handle_payload);
  ack_handler_ = new_handler(this, &pisces_switch::handle_credit);
#endif
}

pisces_switch::~pisces_switch()
{
  if (xbar_) delete xbar_;
#if !SSTMAC_INTEGRATED_SST_CORE
  if (ack_handler_) delete ack_handler_;
  if (payload_handler_) delete payload_handler_;
#endif
  int nbuffers = out_buffers_.size();
  for (int i=0; i < nbuffers; ++i){
    pisces_sender* buf = out_buffers_[i];
    if (buf) delete buf;
  }
}

pisces_sender*
pisces_switch::output_buffer(sprockit::sim_parameters* params,
                                  int src_outport)
{
  if (out_buffers_.empty()){
    out_buffers_.resize(top_->max_num_ports());
  }
  if (!out_buffers_[src_outport]){
    //debug_printf(sprockit::dbg::pisces | sprockit::dbg::pisces_config,
    //  "Switch %d: making buffer with bw=%10.6e on port=%d with buffer size %d going into buffer size %d",
    //  int(my_addr_), total_link_bw, port, src_buffer_size, dst_buffer_size);

    params->add_param_override("num_vc", router_->max_num_vc());
    pisces_network_buffer* out_buffer = new pisces_network_buffer(params, this);

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
pisces_switch::connect_output(
  sprockit::sim_parameters* port_params,
  int src_outport,
  int dst_inport,
  event_handler* mod)
{
  //create an output buffer for the port
  pisces_sender* out_buffer = output_buffer(port_params, src_outport);
  out_buffer->set_output(port_params, src_outport, dst_inport, mod);
}

void
pisces_switch::connect_input(
  sprockit::sim_parameters* port_params,
  int src_outport,
  int dst_inport,
  event_handler* mod)
{
  xbar_->set_input(port_params, dst_inport, src_outport, mod);
}

void
pisces_switch::compatibility_check() const
{
  router_->compatibility_check();
}

void
pisces_switch::deadlock_check()
{
  xbar_->deadlock_check();
}

void
pisces_switch::deadlock_check(event *ev)
{
  xbar_->deadlock_check(ev);
}

int
pisces_switch::queue_length(int port) const
{
  pisces_buffer* buf = static_cast<pisces_buffer*>(out_buffers_[port]);
  return buf->queue_length();
}

void
pisces_switch::handle_credit(event *ev)
{
  pisces_credit* credit = static_cast<pisces_credit*>(ev);
  switch_debug("handling credit %s", credit->to_string().c_str());
  out_buffers_[credit->port()]->handle_credit(credit);
}

void
pisces_switch::handle_payload(event *ev)
{
  pisces_payload* payload = static_cast<pisces_payload*>(ev);
  switch_debug("handling payload %s", payload->to_string().c_str());
  router_->route(payload);
  xbar_->handle_payload(payload);
}

std::string
pisces_switch::to_string() const
{
  return sprockit::printf("packet flow switch %d", int(my_addr_));
}

link_handler*
pisces_switch::credit_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new SST::Event::Handler<pisces_switch>(const_cast<pisces_switch*>(this),
                          &pisces_switch::handle_credit);
#else
  return ack_handler_;
#endif
}

link_handler*
pisces_switch::payload_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new SST::Event::Handler<pisces_switch>(const_cast<pisces_switch*>(this),
                          &pisces_switch::handle_payload);
#else
  return payload_handler_;
#endif
}

}
}



