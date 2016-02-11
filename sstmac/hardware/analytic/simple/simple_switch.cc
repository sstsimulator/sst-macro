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
#include <sstmac/hardware/analytic/simple/simple_switch.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/switch/dist_dummyswitch.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

#if !SSTMAC_INTEGRATED_SST_CORE
SpktRegister("simple", network_switch, simple_switch);
#endif

void
simple_switch::init_factory_params(sprockit::sim_parameters* params)
{
  network_switch::init_factory_params(params);
  double net_bw = params->get_bandwidth_param("bandwidth");
  inverse_bw_ = 1.0/net_bw;
  hop_latency_ = params->get_time_param("hop_latency");

  double inj_bw = params->get_optional_bandwidth_param("injection_bandwidth", net_bw);
  inj_bw_inverse_ = 1.0/inj_bw;
  inj_lat_ = params->get_optional_time_param("injection_latency", 0);

  inv_min_bw_ = std::max(inverse_bw_, inj_bw_inverse_);
}

void
simple_switch::set_topology(topology* top)
{
  network_switch::set_topology(top);
  //validate this...
  std::vector<node_id> nodes = top_->nodes_connected_to_injection_switch(my_addr_);
  if (nodes.size() == 0){
    my_start_ = my_end_ = node_id(-1);
    return;
  }
  my_start_ = nodes[0];
  int size = nodes.size();
  int nid = my_start_ + 1;
  for (int i=1; i < size; ++i, ++nid){
    if (nodes[i] != nid){
      spkt_throw(sprockit::illformed_error,
        "simple switch has invalid node connections - parallel runs here should use block indexing only");
    }
  }
  my_end_ = node_id(nid);
}

void
simple_switch::finalize_init()
{
  network_switch::finalize_init();
}

simple_switch::~simple_switch()
{
}

void
simple_switch::initialize()
{
}

void
simple_switch::add_switch(connectable* mod)
{
  network_switch* netsw = safe_cast(network_switch, mod);
  std::vector<node_id> allnodes = top_->nodes_connected_to_injection_switch(netsw->addr());
  for (int i=0; i < allnodes.size(); ++i){
    node_id nid = allnodes[i];
    neighbors_[nid] = netsw;
  }
}

void
simple_switch::connect_weighted(
  int src_outport,
  int dst_inport,
  connection_type_t ty,
  connectable* mod,
  double weight, int red)
{
  add_switch(mod);
}

void
simple_switch::connect_injector(int src_outport, int dst_inport, event_handler* inj)
{
  nic* theNic = safe_cast(nic, inj);
  nics_[theNic->addr()] = theNic;
}

void
simple_switch::connect_ejector(int src_outport, int dst_inport, event_handler* ej)
{
  nic* theNic = safe_cast(nic, ej);
  nics_[theNic->addr()] = theNic;
}

std::vector<switch_id>
simple_switch::connected_switches() const
{
  std::vector<switch_id> ret;
  ret.resize(neighbors_.size());
  spkt_unordered_map<node_id, network_switch*>::const_iterator it, end = neighbors_.end();
  int idx = 0;
  for (it=neighbors_.begin(); it != end; ++it, ++idx){
    network_switch* netsw = it->second;
    ret[idx] = netsw->addr();
  }
  return ret;
}

timestamp
simple_switch::lookahead() const
{
  //there could be zero hops - so we can't add anything here
  return 2*inj_lat_;
}

void
simple_switch::handle(const sst_message::ptr& msg)
{
  node_id dst = msg->toaddr();
  node_id src = msg->fromaddr();
  timestamp delay;
  if (dst >= my_start_ && dst < my_end_){
    delay = timestamp(inv_min_bw_ * msg->byte_length()); //bw term
    if (1){//src >= my_start_ && src < my_end_){
      //from local to local
      int num_hops = top_->num_hops_to_node(src, dst);
      delay += num_hops * hop_latency_ + 2*inj_lat_; //factor of 2 for in-out
    } //else no delay - remote to local
    send_to_nic(delay, dst, msg);
  }
  else { 
    //form local going remote
    int num_hops = top_->num_hops_to_node(src, dst);
    delay = num_hops * hop_latency_ + 2*inj_lat_; //factor of 2 for in-out
    send_to_switch(delay, dst, msg);
  }
  debug_printf(sprockit::dbg::network_switch,
    "switch %d handling message from %d to %d - delay=%10.6e ms\n%s\n",
    int(my_addr_), int(src), int(dst), delay.msec(), msg->to_string().c_str());
}

void
simple_switch::send_to_nic(timestamp delay, node_id dst, const sst_message::ptr& msg)
{
  SCHEDULE_DELAY(delay, nics_[dst], msg);
}

void
simple_switch::send_to_switch(timestamp delay, node_id dst, const sst_message::ptr& msg)
{
  SCHEDULE_DELAY(delay, neighbors_[dst], msg);
}

#if !SSTMAC_INTEGRATED_SST_CORE
void
simple_switch::set_event_manager(event_manager* m)
{
  network_switch::set_event_manager(m);
  int numsw = top_->num_switches();
  if (m->nworker() != numsw){
    spkt_throw_printf(sprockit::value_error,
        "simple_switch:: topology has %d switches, but we are running with %d SST/macro workers (nproc*nthread)\n"
        "the number of SST/macro workers should match the number of switches",
        numsw, m->nworker());
  }
}
#endif

}
}

#endif // simple_switch_CC


