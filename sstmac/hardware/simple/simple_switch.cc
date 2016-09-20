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
#include <sstmac/hardware/simple/simple_switch.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/switch/dist_dummyswitch.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>


namespace sstmac {
namespace hw {

ImplementSSTComponent("simple", network_switch, simple_switch,
  "A switch that implements no congestion modeling");

simple_switch::simple_switch(sprockit::sim_parameters *params, uint64_t id, event_manager *mgr) :
  network_switch(params, id, mgr)
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


  inv_min_bw_ = std::max(inverse_bw_, inj_bw_inverse_);

  interconn_ = interconnect::static_interconnect(params, mgr);

  nics_.resize(top_->num_nodes());
  neighbors_.resize(top_->num_nodes());
#if !SSTMAC_INTEGRATED_SST_CORE
  mtl_handler_ = new_link_handler(this, &simple_switch::handle);
#endif
}

simple_switch::~simple_switch()
{
#if !SSTMAC_INTEGRATED_SST_CORE
  delete mtl_handler_;
#endif
}

link_handler*
simple_switch::payload_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new_link_handler(const_cast<simple_switch*>(this),
                          &simple_switch::handle);
#else
  return mtl_handler_;
#endif
}

void
simple_switch::add_switch(event_handler* netsw, node_id nid)
{
  neighbors_[nid] = netsw;
}

void
simple_switch::add_nic(event_handler* theNic, node_id nid)
{
  nics_[nid] = theNic;
}

void
simple_switch::connect_output(sprockit::sim_parameters *params,
                              int src_outport, int dst_inport,
                              event_handler *mod)
{
  spkt_throw_printf(sprockit::unimplemented_error,
                    "simple_switch::connect: should not be called");
}

void
simple_switch::connect_input(sprockit::sim_parameters *params,
                              int src_outport, int dst_inport,
                              event_handler *mod)
{
  spkt_throw_printf(sprockit::unimplemented_error,
                    "simple_switch::connect: should not be called");
}

void
simple_switch::handle(event* ev)
{
  //this should only handle messages
  message* msg = safe_cast(message, ev);
  node_id dst = msg->toaddr();
  node_id src = msg->fromaddr();

  bool local_dst = interconn_->local_speedy_node(dst);
  bool local_src = interconn_->local_speedy_node(src);

  timestamp delay;
  if (local_dst){
    delay = timestamp(inv_min_bw_ * msg->byte_length()); //bw term
    if (local_src){ //need to accumulate all the delay here
      //local staying local
      int num_hops = top_->num_hops_to_node(src, dst);
      delay += num_hops * hop_latency_ + 2*inj_lat_; //factor of 2 for in-out
    } else; //remote coming local
    schedule_delay(delay, nics_[dst], msg);
  } else {
    //local going remote - just accumulate latency delay
    int num_hops = top_->num_hops_to_node(src, dst);
    delay = num_hops * hop_latency_ + 2*inj_lat_; //factor of 2 for in-out
    schedule_delay(delay, neighbors_[dst], msg);
  }

  debug_printf(sprockit::dbg::network_switch,
    "switch %d handling message from %d to %d - delay=%10.6e ms\n%s\n",
    int(my_addr_), int(src), int(dst), delay.msec(), msg->to_string().c_str());
}



}
}

#endif // simple_switch_CC


