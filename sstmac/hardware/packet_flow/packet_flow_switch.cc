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

RegisterNamespaces("congestion_stats");

namespace sstmac {
namespace hw {

#if !SSTMAC_INTEGRATED_SST_CORE
SpktRegister("packet_flow", network_switch, packet_flow_switch);
#endif

ImplementIntegratedComponent(packet_flow_switch);

template <class T>
void
set_ev_parent(T& themap, event_scheduler* m)
{
  typename T::iterator it, end = themap.end();
  for (it=themap.begin(); it != end; ++it) {
    it->second->set_event_parent(m);
  }
}

template <class T>
void
vec_set_ev_parent(std::vector<T*>& themap, event_scheduler* m)
{
  for (int i=0; i < themap.size(); ++i){
    T* t = themap[i];
    if (t) t->set_event_parent(m);
  }
}

void
packet_flow_abstract_switch::init_factory_params(sprockit::sim_parameters *params)
{
  network_switch::init_factory_params(params);
  params_ = new packet_flow_params;
  params_->link_bw =
    params->get_bandwidth_param("link_bandwidth");
  params_->hop_lat =
    params->get_time_param("hop_latency");
  params_->xbar_output_buffer_num_bytes
    = params->get_byte_length_param("output_buffer_size");
  params_->crossbar_bw =
    params->get_bandwidth_param("crossbar_bandwidth");
  params_->xbar_input_buffer_num_bytes
    = params->get_byte_length_param("input_buffer_size");

  params_->link_arbitrator_template
    = packet_flow_bandwidth_arbitrator_factory::get_optional_param(
        "arbitrator", "cut_through", params);

  /**
   sstkeyword {
   docstring=The minimum number of bytes a single packet train can contain.ENDL
   Raising this value increases the coarseness and lowers the accuracy.;
   };
   */
  int min_bytes =
    params->get_optional_int_param("mtu", 4096);

  /**
    sstkeyword {
      docstring=Enables output queue depth reporting.ENDL
      If set to true, warnings will be provided each time an output queue increases by a given number.
      This can only be enabled if sanity check is enabled by configure.;
    }
  */
  params_->queue_depth_reporting =
      params->get_optional_bool_param("sanity_check_queue_depth_reporting",false);

  /**
    sstkeyword {
      docstring=Sets the count delta for output queue depth reporting.ENDL
      The default is 100.;
    }
  */
  params_->queue_depth_delta =
      params->get_optional_int_param("sanity_check_queue_depth_delta", 100);

  packet_flow_payload::init_statics(min_bytes);
}

#if SSTMAC_INTEGRATED_SST_CORE
packet_flow_switch::packet_flow_switch(
  SST::ComponentId_t id,
  SST::Params& params
) : packet_flow_abstract_switch(id, params),
  congestion_spyplot_(0),
  bytes_sent_(0),
  byte_hops_(0),
  xbar_(0)
{
}
#else
packet_flow_switch::packet_flow_switch() :
 xbar_(0),
 congestion_spyplot_(0),
 bytes_sent_(0),
 byte_hops_(0)
{
}
#endif

void
packet_flow_switch::init_factory_params(sprockit::sim_parameters *params)
{
  packet_flow_abstract_switch::init_factory_params(params);
  if (params->has_namespace("congestion_stats")){
    sprockit::sim_parameters* congestion_params = params->get_namespace("congestion_stats");
    congestion_spyplot_ = test_cast(stat_spyplot, stat_collector_factory::get_optional_param("type", "spyplot_png", congestion_params));
    if (!congestion_spyplot_){
      spkt_throw_printf(sprockit::value_error,
        "packet flow congestion stats must be spyplot or spyplot_png, %s given",
        congestion_params->get_param("type").c_str());
    }
  }

  if (params->has_namespace("bytes_sent")){
    sprockit::sim_parameters* byte_params = params->get_namespace("bytes_sent");
    bytes_sent_ = test_cast(stat_bytes_sent, stat_collector_factory::get_optional_param("type", "bytes_sent", byte_params));
    if (!bytes_sent_){
      spkt_throw_printf(sprockit::value_error,
        "packet flow bytes sent stats must be bytes_sent, %s given",
        byte_params->get_param("type").c_str());
    }
    bytes_sent_->set_id(my_addr_);
  }

  if (params->has_namespace("byte_hops")) {
    sprockit::sim_parameters* traffic_params = params->get_namespace("byte_hops");
    byte_hops_ = test_cast(stat_global_int, stat_collector_factory::get_optional_param("type", "global_int", traffic_params));
    byte_hops_->set_label("Byte Hops");
  }
}

void
packet_flow_switch::set_topology(topology *top)
{
  if (bytes_sent_) bytes_sent_->set_topology(top);
  network_switch::set_topology(top);
}

packet_flow_switch::~packet_flow_switch()
{
}

void
packet_flow_switch::initialize()
{
  crossbar();
  int nbuffers = out_buffers_.size();
  int buffer_inport = 0;
  for (int i=0; i < nbuffers; ++i){
    int outport = i;
    packet_flow_sender* buf = out_buffers_[outport];
    if (buf){ //might be sparse
      xbar_->set_output(outport, buffer_inport, buf);
      xbar_->init_credits(outport, buf->num_initial_credits());
      buf->set_input(buffer_inport, outport, xbar_);
      buf->set_event_location(my_addr_);
    }
  }
}

packet_flow_crossbar*
packet_flow_switch::crossbar()
{
  if (!xbar_) {
    xbar_ = new packet_flow_crossbar(
              timestamp(0), //assume zero-time send
              params_->hop_lat, //delayed credits
              params_->crossbar_bw,
              router_->max_num_vc(),
              params_->xbar_input_buffer_num_bytes,
              params_->link_arbitrator_template->clone());
    xbar_->configure_basic_ports(topol()->max_num_ports());
    xbar_->set_event_location(my_addr_);
  }
  return xbar_;
}

void
packet_flow_switch::resize_buffers()
{
  if (out_buffers_.empty()) out_buffers_.resize(top_->max_num_ports());
}

packet_flow_sender*
packet_flow_switch::output_buffer(int port, double out_bw, int red)
{
  if (!out_buffers_[port]){
    packet_flow_network_buffer* out_buffer
      = new packet_flow_network_buffer(out_bw,
                  params_->hop_lat,
                  timestamp(0), //assume credit latency to xbar is free
                  params_->xbar_output_buffer_num_bytes * red,
                  router_->max_num_vc(),
                  params_->link_arbitrator_template->clone());
    out_buffer->set_event_location(my_addr_);
    int buffer_outport = 0;
    out_buffer->init_credits(buffer_outport, params_->xbar_input_buffer_num_bytes);
    out_buffer->set_sanity_params(params_->queue_depth_reporting,
                                params_->queue_depth_delta);
    out_buffers_[port] = out_buffer;
  }
  return out_buffers_[port];
}

void
packet_flow_switch::connect_output(
  int src_outport,
  int dst_inport,
  connectable* mod,
  double weight, int red
)
{
  resize_buffers();

  //create an output buffer for the port
  double total_link_bw = params_->link_bw * weight * red;
  packet_flow_sender* out_buffer = output_buffer(src_outport, total_link_bw, red);
  out_buffer->set_output(src_outport, dst_inport, safe_cast(event_handler, mod));
}

void
packet_flow_switch::connect_input(
  int src_outport,
  int dst_inport,
  connectable* mod,
  double weight, int red
)
{
  crossbar()->set_input(dst_inport, src_outport, safe_cast(event_handler, mod));
}

void
packet_flow_switch::connect_weighted(
  int src_outport,
  int dst_inport,
  connection_type_t ty,
  connectable* mod,
  double weight, int red)
{
  switch(ty) {
    case output:
      connect_output(src_outport, dst_inport, mod, weight, red);
      break;
    case input:
      connect_input(src_outport, dst_inport, mod, weight, red);
      break;
    default:
      network_switch::connect_weighted(src_outport, dst_inport, ty, mod, weight, red);
      break;
  }
}

void
packet_flow_switch::connect_injector(int src_outport, int dst_inport, event_handler* nic)
{
  connectable* inp = safe_cast(connectable, nic);
  connect_input(src_outport, dst_inport, inp, 1.0, 1);
}

void
packet_flow_switch::connect_ejector(int src_outport, int dst_inport, event_handler* nic)
{
  connectable* inp = safe_cast(connectable, nic);
  connect_output(src_outport, dst_inport, inp, 1.0, 1);
}

std::vector<switch_id>
packet_flow_switch::connected_switches() const
{
  std::vector<switch_id> ret;
  ret.reserve(out_buffers_.size());
  int idx = 0;
  for (int b=0; b < out_buffers_.size(); ++b){
    packet_flow_buffer* buf = test_cast(packet_flow_buffer, out_buffers_[b]);
    if (buf) ret[idx++] = buf->output_location().convert_to_switch_id();
  }
  return ret;
}

void
packet_flow_switch::deadlock_check()
{
  xbar_->deadlock_check();
}

void
packet_flow_switch::set_event_manager(event_manager* m)
{
  network_switch::set_event_manager(m);
  crossbar();
#if !SSTMAC_INTEGRATED_SST_CORE
  if (congestion_spyplot_){
    xbar_->set_congestion_spyplot(congestion_spyplot_);
    for (int i=0; i < out_buffers_.size(); ++i){
      packet_flow_buffer* buf = test_cast(packet_flow_buffer, out_buffers_[i]);
      if (buf) buf->set_congestion_spyplot(congestion_spyplot_);
    }
    m->register_stat(congestion_spyplot_);
  }

  if (bytes_sent_){
    xbar_->set_bytes_sent_collector(bytes_sent_);
    m->register_stat(bytes_sent_);
  }

  if (byte_hops_) {
    xbar_->set_byte_hops_collector(byte_hops_);
    m->register_stat(byte_hops_);
  }
#endif
  vec_set_ev_parent(out_buffers_, this);
  xbar_->set_event_parent(this);
}

int
packet_flow_switch::queue_length(int port) const
{
  packet_flow_buffer* buf = static_cast<packet_flow_buffer*>(out_buffers_[port]);
  return buf->get_queue_length();
}

void
packet_flow_switch::handle(const sst_message::ptr& msg)
{
  //this should only happen in parallel mode...
  //this means we are getting a message that has crossed the parallel boundary
  packet_flow_interface* fmsg = ptr_interface_cast(packet_flow_interface, msg);
  switch (fmsg->type()) {
    case packet_flow_interface::credit: {
      packet_flow_credit::ptr credit = ptr_static_cast(packet_flow_credit, msg);
      out_buffers_[credit->port()]->handle_credit(credit);
      break;
    }
    case packet_flow_interface::payload: {
      packet_flow_payload::ptr payload = ptr_static_cast(packet_flow_payload, msg);
      router_->route(payload.get());
      xbar_->handle_payload(payload);
      break;
    }
  }
}

std::string
packet_flow_switch::to_string() const
{
  return sprockit::printf("packet flow switch %d", int(my_addr_));
}

}
}



