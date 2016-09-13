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

#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/nic/netlink.h>
#include <sstmac/hardware/common/fail_event.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/packet_flow/packet_flow.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/switch/dist_dummyswitch.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/statics.h>
#include <sprockit/delete.h>
#include <sprockit/output.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>

ImplementFactory(sstmac::hw::interconnect)
RegisterDebugSlot(interconnect);
RegisterNamespaces("interconnect");

namespace sstmac {

sprockit::StaticNamespaceRegister node_ns_reg("node");

namespace hw {

//static sprockit::need_delete_statics<interconnect> del_statics;

SpktRegister("switch | simple", interconnect, interconnect);

interconnect* interconnect::static_interconnect_ = 0;

#if !SPKT_DISABLE_REGEX
sprockit::StaticKeywordRegisterRegexp node_failure_ids_keyword("node_failure_\\d+_id");
sprockit::StaticKeywordRegisterRegexp node_failure_time_keyword("node_failure_\\d+_time");
#endif



interconnect*
interconnect::static_interconnect(sprockit::sim_parameters* params, event_manager* mgr)
{
  if (!static_interconnect_){
    sprockit::sim_parameters* ic_params = params;
    if (params->has_namespace("interconnect")){
      ic_params = params->get_namespace("interconnect");
    }
    const char* ic_param = ic_params->has_param("network_name") ? "network_name" : "interconnect";
    parallel_runtime* rt = parallel_runtime::static_runtime(params);
    partition* part = rt ? rt->topology_partition() : nullptr;
    static_interconnect_ = interconnect_factory::get_optional_param(ic_param, "switch", ic_params,
      mgr, part, rt);
  }
  return static_interconnect_;
}

#if !SSTMAC_INTEGRATED_SST_CORE
interconnect::~interconnect()
{
  sprockit::delete_vals(netlinks_);
  sprockit::delete_vals(nodes_);
  sprockit::delete_vals(nics_);
}
#endif

interconnect::interconnect(sprockit::sim_parameters *params, event_manager *mgr,
                           partition *part, parallel_runtime *rt)
{
  if (!static_interconnect_) static_interconnect_ = this;
  topology_ = topology::static_topology(params);
  num_nodes_ = topology_->num_nodes();
  runtime::set_topology(topology_);

#if !SSTMAC_INTEGRATED_SST_CORE
  partition_ = part;
  rt_ = rt;
  int my_rank = rt_->me();
  sprockit::sim_parameters* netlink_params = nullptr;
  if (params->has_namespace("netlink")) netlink_params = params->get_namespace("netlink");
  int netlink_conc = topology_->num_nodes_per_netlink();

  sprockit::sim_parameters* node_params = params->get_namespace("node");

  structured_topology* top = safe_cast(structured_topology, topology_);

  int num_switches = topology_->num_switches();
  for (int i=0; i < num_switches; ++i){
    if (partition_->lpid_for_switch(switch_id(i)) == my_rank){
      std::vector<node_id> nodes = top->nodes_connected_to_switch(switch_id(i));
      for (int n=0; n < nodes.size(); ++n){
        node_id nid = nodes[n];
        node_params->add_param_override("id", int(nid));
        node* nd = node_factory::get_optional_param("model", "simple", node_params,
                                                    nid, mgr);
        nodes_[nid] = nd;
        nics_[nid] = nd->get_nic();
        nd->get_nic()->set_interconnect(this);

        nd->init(0); //emulate SST core
        nd->setup();

        int interf_id = nid / netlink_conc;
        int interf_offset = nid % netlink_conc;
        node_id my_id = node_id(interf_id);
        if (netlink_params && interf_offset == 0){
          top_debug("Adding NIC %d connected to switch %d on rank %d",
            int(my_id), i, my_rank);
          params->add_param_override("id", int(my_id));
          netlink* nlink = netlink_factory::get_param("model", netlink_params, nd);
          netlinks_[my_id] = nlink;
        }
      }
    }
  }

  sprockit::sim_parameters* switch_params = params->get_namespace("switch");
  for (int i=0; i < num_switches; ++i){
    switch_id sid(i);
    top_debug("Switch %d belongs to rank %d for building NICs: my_rank=%d",
      i, part->lpid_for_switch(sid), my_rank);
    if (part->lpid_for_switch(sid) == my_rank){
      params->add_param_override("id", i);
      switches_[sid] = network_switch_factory::get_param("model", switch_params,
                                                         sid, mgr);
    } else {
      switches_[sid] = new dist_dummy_switch(params, sid, mgr);
    }
  }

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

  network_switch* switch_tmpl = nullptr;
  for (auto& pair : switches_){
    network_switch* netsw = pair.second;
    if (!netsw->ipc_handler()){
      netsw->initialize();
      switch_tmpl = netsw;
    }
  }

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

  int failure_num = 1;
  while(1){
    std::string next_param_name = sprockit::printf("node_failure_%d_id", failure_num);
    if (!params->has_param(next_param_name)){
      break;
    }

    int node_to_fail = params->get_int_param(next_param_name);
    next_param_name = sprockit::printf("node_failure_%d_time", failure_num);
    timestamp fail_time = params->get_time_param(next_param_name);
    failures_to_schedule_.push_back(node_fail_event(fail_time, node_id(node_to_fail)));
    ++failure_num;
  }
#endif
}

#if !SSTMAC_INTEGRATED_SST_CORE
void
interconnect::handle(event* ev)
{
  spkt_throw(sprockit::value_error, "interconnect should never handle messages");
}

void
interconnect::write_graph_file(const std::string& graph_file)
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
interconnect::deadlock_check()
{
  for (auto& entry : switches_){
    entry.second->deadlock_check();
  }
  for (auto& entry : netlinks_){
    entry.second->deadlock_check();
  }
}

void
interconnect::immediate_send(event_scheduler* src, message* msg, timestamp start) const
{
  node* dst_node = node_at(msg->toaddr());
  int num_hops = topology_->num_hops_to_node(msg->fromaddr(), msg->toaddr());
  timestamp arrival = send_delay(num_hops, msg->byte_length()) + start;
  //double bw_term = msg->byte_length() / hop_bw_;
  //timestamp arrival = src->now() + hop_latency_ * num_hops + timestamp(bw_term) + 2*injection_latency_;

  interconn_debug("immediate_send send from %d to %d to arrive at %es:\t\n%s\n\tnhops=%d bandwidth=%e hop_latency=%es inj_latency=%es",
    int(msg->fromaddr()), int(msg->toaddr()), arrival.sec(),
    msg->to_string().c_str(),
    num_hops, hop_bw_, hop_latency_.sec(), injection_latency_.sec());

  if (dst_node){ //local operation
    src->schedule(arrival, dst_node->get_nic()->mtl_handler(), msg);
  } else {
    src->ipc_schedule(arrival, dst_node, msg);
  }
}

int
interconnect::thread_for_switch(switch_id sid) const
{
  network_switch* sw = this->switch_at(sid);
  return sw->thread_id();
}
#endif

}
}

