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

#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/nic/netlink.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/interconnect/switch_interconnect.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/switch/dist_dummyswitch.h>
#include <sstmac/hardware/topology/index_subset.h>
#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sprockit/stl_string.h>
#include <sprockit/util.h>
#include <sprockit/delete.h>
#include <sprockit/output.h>

#define sw_interconn_debug(...) interconn_debug("switch interconnect: %s", sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

SpktRegister("switch", interconnect, switch_interconnect,
            "Models the network interconnect as a topology of connected switches");

#if SSTMAC_INTEGRATED_SST_CORE
void
sst_switch_interconnect::init_factory_params(sprockit::sim_parameters *params)
{
}
#else

macro_switch_interconnect::macro_switch_interconnect() 
{
}

macro_switch_interconnect::~macro_switch_interconnect()
{
  sprockit::delete_vals(switches_);
}

void
macro_switch_interconnect::write_graph_file(const std::string& graph_file)
{
  std::ofstream output;
  output.open(graph_file.c_str());

  long numsw = switches_.size();
  long edges = topology_->max_ports_intra_network() * numsw / 2; //divide by two to make it unidirectional

  output << numsw << " " << edges << std::endl;

  for (long a = 0; a < switches_.size(); a++)
  {
    switch_id addr(a);
    network_switch* net = switches_[addr];
    std::vector<switch_id> con = net->connected_switches();
    for (int i = 0; i < con.size(); i++){
      output << (con[i] + 1) << " ";
    }
    output << std::endl;
  }
  output.close();
}

void
macro_switch_interconnect::deadlock_check()
{
  switch_map::iterator it, end = switches_.end();
  for (it=switches_.begin(); it != end; ++it){
    network_switch* sw = it->second;
    sw->deadlock_check();
  }
}

void
macro_switch_interconnect::init_factory_params(sprockit::sim_parameters* params)
{
  /** This builds the nodes */
  interconnect_base::init_factory_params(params);
  runtime::set_topology(topology_);

  internal_map switches;
  sprockit::sim_parameters* switch_params = params->get_namespace("switch");
  sprockit::factory<connectable>* switch_builder
    = new sprockit::template_factory<connectable, network_switch_factory>(switch_params->get_param("model"));
  network_switch* dummy = new dist_dummy_switch(switch_id());
  topology_->build_internal_connectables(switches, switch_builder, partition_, rt_->me(), switch_params, dummy);
  delete switch_builder;
  copy_map(switches, switches_);
  switch_map::iterator it, end = switches_.end();
  for (it=switches_.begin(); it != end; ++it){
    network_switch* sw = it->second;
    if (sw->ipc_handler()){
      switch_id sid = it->first;
      it->second = new dist_dummy_switch(sid);
    } else {
      sw->set_topology(topology_);
    }
  }
  delete dummy;
  dummy = 0;

  int the_only_port = 0;

  connectable::config nic_cfg;
  nic_cfg.link_weight = 1.0;
  nic_cfg.red = 1;



  if (!netlinks_.empty()){
    //we connect to netlinks, not nics
    topology_->connect_end_points(switches_, netlinks_);
    int netlinks_per_node = topology_->num_nodes_per_netlink();
    node_map::iterator it, end = nodes_.end();
    for (it = nodes_.begin(); it != end; it++) {
      node* the_node = it->second;
      nic* the_nic = safe_cast(nic, the_node->get_nic());
      node_id nid = it->first;
      netlink_id netid = netlink_id(nid / netlinks_per_node);
      int node_offset = nid % netlinks_per_node;
      netlink* nlnk = netlinks_[netid];
      int inj_port = nlnk->node_port(node_offset);
      top_debug("Connecting NIC %d to netlink %d on ports %d:%d",
             int(nid), int(netid), the_only_port, inj_port);
      //injection ports have to be offset
      nlnk->connect(the_only_port, inj_port, connectable::input, the_nic, &nic_cfg);
      the_nic->connect(the_only_port, inj_port, connectable::output, nlnk, &nic_cfg);

      nlnk->connect(inj_port, the_only_port, connectable::output, the_nic, &nic_cfg);
      the_nic->connect(inj_port, the_only_port, connectable::input, nlnk, &nic_cfg);
    }
  } else {
    topology_->connect_end_points(switches_, nics_);
  }

  topology_->connect_topology(switches_);

  network_switch* switch_tmpl = 0;
  { switch_map::const_iterator it, end = switches_.end();
  for (it=switches_.begin(); it != end; ++it){
    network_switch* thesw = it->second;
    if (!thesw->ipc_handler()){
      switch_tmpl = thesw;
      break;
    }
  } }

  if (!nics_.empty()){
    nic* nic_tmpl = nics_.begin()->second;
    injection_latency_ = nic_tmpl->injection_latency();
  } else {
    cerrn << "WARNING: rank was assigned no nodes" << std::endl;
  }

  if (switch_tmpl){
    hop_latency_ = switch_tmpl->hop_latency();
    lookahead_ = switch_tmpl->lookahead();
    hop_bw_ = switch_tmpl->hop_bandwidth();
  } else {
    //don't even need to assign these values - will never be used
    cerrn << "WARNING: rank was assigned no switches" << std::endl;
  }
}

void
macro_switch_interconnect::immediate_send(event_scheduler* src, message* msg, timestamp start) const
{
  node* dst_node = node_at(msg->toaddr());
  int num_hops = topology_->num_hops_to_node(msg->fromaddr(), msg->toaddr());
  timestamp arrival = send_delay(num_hops, msg->byte_length()) + start;
  //double bw_term = msg->byte_length() / hop_bw_;
  //timestamp arrival = src->now() + hop_latency_ * num_hops + timestamp(bw_term) + 2*injection_latency_;

  sw_interconn_debug("immediate_send send from %d to %d to arrive at %es:\t\n%s\n\tnhops=%d bandwidth=%e hop_latency=%es inj_latency=%es",
    int(msg->fromaddr()), int(msg->toaddr()), arrival.sec(),
    msg->to_string().c_str(),
    num_hops, hop_bw_, hop_latency_.sec(), injection_latency_.sec());

  if (dst_node){ //local operation
    src->schedule(arrival, dst_node->get_nic()->mtl_handler(), msg);
  } else {
    src->ipc_schedule(arrival, dst_node, msg);
  }
}

void
macro_switch_interconnect::set_switch_event_manager(int thread_id, switch_id sid, event_manager* m)
{
  network_switch* sw = switches_[sid];
  event_manager* thr_ev_man = m->ev_man_for_thread(thread_id);
  sw->initialize();
  interconn_debug("Rank %d assigned switch %d to thread %d",
    rt_->me(), int(sid), thread_id);
  sw->set_event_manager(thr_ev_man);
  std::vector<node_id> nodes = topology_->nodes_connected_to_injection_switch(sid);
  for (int n=0; n < nodes.size(); ++n){
    node_id nid(nodes[n]);
    node* the_node = nodes_[nid];
    set_node_event_manager(the_node, thr_ev_man);
    interconn_debug("Rank %d assigned node %d to thread %d",
      rt_->me(), int(nid), thread_id);
  }
}

int
macro_switch_interconnect::thread_for_switch(switch_id sid) const
{
  network_switch* sw = this->switch_at(sid);
  return sw->thread_id();
}

void
macro_switch_interconnect::set_event_manager(event_manager* m)
{
  runtime::set_topology(topology_);

  int num_local_switches = partition_->local_num_switches();
  for (int i=0; i < num_local_switches; ++i){
    switch_id sid(partition_->local_switch(i));
    int thr = partition_->thread_for_local_switch(i);
    set_switch_event_manager(thr, sid, m);
  }

  //this has to go AFTER the switch/node sets
  //this assumes that node/switches have been assigned
  //an event manager
  interconnect_base::set_event_manager_common(m);
}


#endif

}
}

