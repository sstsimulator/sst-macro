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
  packet_size_ = params->get_optional_byte_length_param("mtu", 4096);
  link_bw = params->get_bandwidth_param("link_bandwidth");
  hop_lat = params->get_time_param("hop_latency");
  xbar_output_buffer_num_bytes = params->get_byte_length_param("output_buffer_size");
  xbar_bw = params->get_bandwidth_param("crossbar_bandwidth");
  xbar_input_buffer_num_bytes  = params->get_byte_length_param("input_buffer_size");

  if (params->has_param("ejection_bandwidth")){
    ej_bw = params->get_bandwidth_param("ejection_bandwidth");
  } else {
    ej_bw = params->get_bandwidth_param("injection_bandwidth");
  }

  link_arbitrator_template
    = packet_flow_bandwidth_arbitrator_factory::get_optional_param(
        "arbitrator", "cut_through", params);

  sprockit::sim_parameters* xbar_params = params->get_optional_namespace("xbar");

  xbar_stats_ = packet_sent_stats_factory::get_optional_param("stats", "null", xbar_params);

  sprockit::sim_parameters* buf_params = params->get_optional_namespace("output_buffer");
  buf_stats_ = packet_sent_stats_factory::get_optional_param("stats", "null", buf_params);
}

#if SSTMAC_INTEGRATED_SST_CORE
packet_flow_switch::packet_flow_switch(
  SST::ComponentId_t id,
  SST::Params& params
) : packet_flow_abstract_switch(id, params),
  xbar_(0)
{
  init_factory_params(SSTIntegratedComponent::params_);
  init_sst_params(params);
}
#else
packet_flow_switch::packet_flow_switch() :
 xbar_(0)
{
}
#endif

packet_flow_switch::~packet_flow_switch()
{
  if (xbar_) delete xbar_;
 
  int nbuffers = out_buffers_.size();
  for (int i=0; i < nbuffers; ++i){
    packet_flow_sender* buf = out_buffers_[i];
    if (buf) delete buf;
  }
}


void
packet_flow_switch::init_factory_params(sprockit::sim_parameters *params)
{
  params_ = params;
  packet_flow_abstract_switch::init_factory_params(params);
}

void
packet_flow_switch::set_topology(topology *top)
{
  network_switch::set_topology(top);
}

void
packet_flow_switch::initialize()
{
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
packet_flow_switch::crossbar(config* cfg)
{
  if (!xbar_) {
    double bw = xbar_bw;
    if (cfg->ty == WeightedConnection){
      bw *= cfg->xbar_weight;
    }
    debug_printf(sprockit::dbg::packet_flow | sprockit::dbg::packet_flow_config,
      "Switch %d: creating crossbar with bandwidth %12.8e",
      int(my_addr_), xbar_bw);
    xbar_ = new packet_flow_crossbar(
              timestamp(0), //assume zero-time send
              hop_lat, //delayed credits
              bw,
              router_->max_num_vc(),
              xbar_input_buffer_num_bytes,
              link_arbitrator_template->clone(-1/*fake bw*/));
    xbar_->configure_basic_ports(topol()->max_num_ports());
    xbar_->set_event_location(my_addr_);
    xbar_->set_stat_collector(xbar_stats_);
  }
  return xbar_;
}

void
packet_flow_switch::resize_buffers()
{
  if (out_buffers_.empty()) out_buffers_.resize(top_->max_num_ports());
}

packet_flow_sender*
packet_flow_switch::output_buffer(int port, config* cfg)
{
  if (!out_buffers_[port]){
    bool inj_port = top_->is_injection_port(port);
    double total_link_bw = inj_port ? ej_bw : link_bw;
    int dst_buffer_size = xbar_input_buffer_num_bytes;
    int src_buffer_size = xbar_output_buffer_num_bytes;
    timestamp lat = hop_lat;
    switch(cfg->ty){
      case BasicConnection:
        break;
      case RedundantConnection:
        total_link_bw *= cfg->red;
        src_buffer_size *= cfg->red;
        break;
       case WeightedConnection:
        total_link_bw *= cfg->link_weight;
        src_buffer_size *= cfg->src_buffer_weight;
        dst_buffer_size *= cfg->dst_buffer_weight;
        break;
      case FixedBandwidthConnection:
        total_link_bw = cfg->bw;
        break;
      case FixedConnection:
        total_link_bw = cfg->bw;
        lat = cfg->latency;
        break;
      default:
        spkt_throw_printf(sprockit::value_error,
          "bad connection::config enum %d", cfg->ty);
    }

    debug_printf(sprockit::dbg::packet_flow | sprockit::dbg::packet_flow_config,
      "Switch %d: making buffer with bw=%10.6e on port=%d with buffer size %d going into buffer size %d",
      int(my_addr_), total_link_bw, port, src_buffer_size, dst_buffer_size);

    packet_flow_network_buffer* out_buffer
      = new packet_flow_network_buffer(
                  hop_lat,
                  timestamp(0), //assume credit latency to xbar is free
                  src_buffer_size,
                  router_->max_num_vc(),
                  packet_size_,
                  link_arbitrator_template->clone(total_link_bw));

    out_buffer->set_stat_collector(buf_stats_);
    out_buffer->set_event_location(my_addr_);
    int buffer_outport = 0;
    out_buffer->init_credits(buffer_outport, dst_buffer_size);
    out_buffers_[port] = out_buffer;
  }
  return out_buffers_[port];
}

void
packet_flow_switch::connect_output(
  int src_outport,
  int dst_inport,
  connectable* mod,
  config* cfg)
{
  resize_buffers();

  //create an output buffer for the port
  packet_flow_sender* out_buffer = output_buffer(src_outport, cfg);
  out_buffer->set_output(src_outport, dst_inport, safe_cast(event_handler, mod));
}

void
packet_flow_switch::connect_input(
  int src_outport,
  int dst_inport,
  connectable* mod,
  config* cfg)
{
  crossbar(cfg)->set_input(dst_inport, src_outport, safe_cast(event_handler, mod));
}

void
packet_flow_switch::connect(
  int src_outport,
  int dst_inport,
  connection_type_t ty,
  connectable* mod,
  config* cfg)
{
  switch(ty) {
    case output:
      connect_output(src_outport, dst_inport, mod, cfg);
      break;
    case input:
      connect_input(src_outport, dst_inport, mod, cfg);
      break;
  }
}

void
packet_flow_switch::connect_injector(int src_outport, int dst_inport, event_handler* nic)
{
  connectable* inp = safe_cast(connectable, nic);
  connect_input(src_outport, dst_inport, inp, NULL); //no cfg
}

void
packet_flow_switch::connect_ejector(int src_outport, int dst_inport, event_handler* nic)
{
  connectable* inp = safe_cast(connectable, nic);
  connect_output(src_outport, dst_inport, inp, NULL); //no cfg
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
  if (!xbar_){
    spkt_throw(sprockit::value_error,
       "crossbar uninitialized on switch");
  }

  xbar_stats_->set_event_manager(m);
  buf_stats_->set_event_manager(m);

  vec_set_ev_parent(out_buffers_, this);
  xbar_->set_event_parent(this);
}

int
packet_flow_switch::queue_length(int port) const
{
  packet_flow_buffer* buf = static_cast<packet_flow_buffer*>(out_buffers_[port]);
  return buf->queue_length();
}

void
packet_flow_switch::handle(event* ev)
{
  //this should only happen in parallel mode...
  //this means we are getting a message that has crossed the parallel boundary
  packet_flow_interface* fpack = interface_cast(packet_flow_interface, ev);
  switch (fpack->type()) {
    case packet_flow_interface::credit: {
      packet_flow_credit* credit = static_cast<packet_flow_credit*>(fpack);
      out_buffers_[credit->port()]->handle_credit(credit);
      break;
    }
    case packet_flow_interface::payload: {
      packet_flow_payload* payload = static_cast<packet_flow_payload*>(fpack);
      router_->route(payload);
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



