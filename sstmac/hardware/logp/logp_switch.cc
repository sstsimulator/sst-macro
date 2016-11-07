#ifndef simple_switch_CC
#define simple_switch_CC

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
#include <sstmac/hardware/logp/logp_switch.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/switch/dist_dummyswitch.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>


namespace sstmac {
namespace hw {

SpktRegister("logP | simple | LogP | logp", network_switch, logp_switch,
  "A switch that implements no congestion modeling");

logp_switch::logp_switch(sprockit::sim_parameters *params, uint64_t id, event_manager *mgr) :
  network_switch(params, id, mgr, device_id::logp_overlay)
{
  sprockit::sim_parameters* link_params = params->get_namespace("link");
  sprockit::sim_parameters* ej_params = params->get_namespace("ejection");

  double net_bw = link_params->get_bandwidth_param("bandwidth");
  inverse_bw_ = 1.0/net_bw;
  if (link_params->has_param("send_latency")){
    hop_latency_ = link_params->get_time_param("send_latency");
  } else {
    hop_latency_ = link_params->get_time_param("latency");
  }

  double inj_bw = ej_params->get_optional_bandwidth_param("bandwidth", net_bw);
  inj_bw_inverse_ = 1.0/inj_bw;
  if (ej_params->has_param("send_latency")){
    inj_lat_ = ej_params->get_time_param("send_latency");
  } else {
    inj_lat_ = ej_params->get_time_param("latency");
  }
  dbl_inj_lat_ = 2*inj_lat_;

  inv_min_bw_ = std::max(inverse_bw_, inj_bw_inverse_);

  interconn_ = interconnect::static_interconnect(params, mgr);

  nics_.resize(top_->num_nodes());
  neighbors_.reserve(1000); //nproc - just reserve a large block for now
#if !SSTMAC_INTEGRATED_SST_CORE
  mtl_handler_ = new_handler(this, &logp_switch::handle);
#endif
}

logp_switch::~logp_switch()
{
#if !SSTMAC_INTEGRATED_SST_CORE
  delete mtl_handler_;
#endif
}

link_handler*
logp_switch::payload_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new SST::Event::Handler<logp_switch>(
        const_cast<logp_switch*>(this), &logp_switch::handle);
#else
  return mtl_handler_;
#endif
}

void
logp_switch::connect_output(sprockit::sim_parameters *params,
                              int src_outport, int dst_inport,
                              event_handler *mod)
{
  if (dst_inport == Node){
    node_id nid = src_outport;
    switch_debug("Connecting LogP to NIC %d", nid);
    nics_[nid] = mod;
  } else if (dst_inport == Switch){
    switch_id sid = src_outport;
    switch_debug("Connecting to LogP switch %d", sid);
    if (sid >= neighbors_.size()){
      neighbors_.resize(sid+1);
    }
    neighbors_[sid] = mod;
  } else {
    spkt_abort_printf("Invalid inport %d in logp_switch::connect_output", dst_inport);
  }
}

void
logp_switch::connect_input(sprockit::sim_parameters *params,
                              int src_outport, int dst_inport,
                              event_handler *mod)
{
  //no-op
}

void
logp_switch::handle(event* ev)
{
  //this should only handle messages
  message* msg = safe_cast(message, ev);
  node_id dst = msg->toaddr();
  node_id src = msg->fromaddr();

  bool local_dst = nics_[dst];
  bool local_src = nics_[src];

  switch_debug("handling message %d(%d)->%d(%d): %s",
               src, local_src, dst, local_dst, msg->to_string().c_str());

  if (local_dst){
    timestamp extra_delay(inv_min_bw_ * msg->byte_length()); //bw term
    if (local_src){ //need to accumulate all the delay here
      //local staying local
      int num_hops = top_->num_hops_to_node(src, dst);
      extra_delay += num_hops * hop_latency_ + dbl_inj_lat_; //factor of 2 for in-out
    } else; //remote coming local
    send_delayed_to_link(extra_delay, nics_[dst], msg);
  } else {
    //local going remote - just accumulate latency delay
    int num_hops = top_->num_hops_to_node(src, dst);
    timestamp extra_delay = num_hops * hop_latency_; //factor of 2 for in-out

    int dst_switch = interconn_->node_to_logp_switch(dst);
    send_delayed_to_link(extra_delay, dbl_inj_lat_, neighbors_[dst_switch], msg);
  }
}



}
}

#endif // simple_switch_CC


