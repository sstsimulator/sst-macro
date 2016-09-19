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
#include <sstmac/hardware/simple/simple_switch.h>
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
  sprockit::delete_vector(netlinks_);
  sprockit::delete_vector(nodes_);
  sprockit::delete_vector(nics_);
}
#endif

interconnect::interconnect(sprockit::sim_parameters *params, event_manager *mgr,
                           partition *part, parallel_runtime *rt)
{
  if (!static_interconnect_) static_interconnect_ = this;
  topology_ = topology::static_topology(params);
  num_nodes_ = topology_->num_nodes();
  num_switches_ = topology_->num_switches();
  runtime::set_topology(topology_);

#if !SSTMAC_INTEGRATED_SST_CORE
  partition_ = part;
  rt_ = rt;
  int my_rank = rt_->me();
  int nproc = rt_->nproc();
  sprockit::sim_parameters* netlink_params = params->get_optional_namespace("netlink");
  sprockit::sim_parameters* nlink_inj_params =
      netlink_params->get_optional_namespace("injection");
  num_speedy_switches_with_extra_node_ = num_nodes_ % nproc;
  num_nodes_per_speedy_switch_ = num_nodes_ / nproc;
  node_to_speedy_switch_.resize(num_nodes_);
  speedy_overlay_switches_.resize(nproc);

  sprockit::sim_parameters* node_params = params->get_namespace("node");
  sprockit::sim_parameters* nic_params = node_params->get_namespace("nic");
  sprockit::sim_parameters* inj_params = nic_params->get_namespace("injection");
  sprockit::sim_parameters* switch_params = params->get_namespace("switch");
  sprockit::sim_parameters* ej_params = switch_params->get_namespace("ejection");
  topology* top = topology_;

  bool simple_model = switch_params->get_param("model") == "simple";

  switches_.resize(num_switches_);
  nodes_.resize(num_nodes_);
  nics_.resize(num_nodes_);
  netlinks_.resize(num_nodes_);

  local_speedy_switch_ = my_rank;
  simple_switch* local_speedy_switch;
  for (int i=0; i < nproc; ++i){
    switch_id sid(i);
    switch_params->add_param_override("id", int(sid));
    if (i == my_rank){
      speedy_overlay_switches_[sid] = local_speedy_switch = new simple_switch(switch_params, sid, mgr);
    } else {
      speedy_overlay_switches_[sid] = new dist_dummy_switch(switch_params, sid, mgr);
    }
  }

  build_endpoints(node_params, nic_params,netlink_params, mgr);
  if (!simple_model){
    build_switches(switch_params, mgr);
    connect_switches(switch_params);
    if (netlinks_.empty()){
      connect_endpoints(inj_params, ej_params);
    } else {
      connect_endpoints(nlink_inj_params, ej_params);
    }
  }

  sprockit::sim_parameters* link_params = switch_params->get_namespace("link");
  if (link_params->has_param("send_latency")){
    hop_latency_ = link_params->get_time_param("send_latency");
  } else {
    hop_latency_ = link_params->get_time_param("latency");
  }
  hop_bw_ = link_params->get_bandwidth_param("bandwidth");
  lookahead_ = hop_latency_;
  injection_latency_ = inj_params->get_time_param("latency");

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
interconnect::connect_endpoints(sprockit::sim_parameters* inj_params,
                                sprockit::sim_parameters* ej_params)
{
  int num_nodes = topology_->num_nodes();
  for (int i=0; i < num_nodes; ++i){
    node_id netlink_id;
    node_id ep_id(i);
    int netlink_offset;
    connectable* ep = nullptr;
    bool has_netlink = topology_->node_to_netlink(i, netlink_id, netlink_offset);
    if (has_netlink) {
      if (netlink_offset == 0){
        ep = netlinks_[netlink_id];
        ep_id = netlink_id;
      }
    } else {
      ep_id = i;
      ep = nics_[i];
    }
    if (!ep) continue; //no connection required

    //map to topology-specific port
    int num_ports;
    int ports[32];
    node_id nodeaddr(i);
    switch_id injaddr = topology_->endpoint_to_injection_switch(nodeaddr, ports, num_ports);
    connectable* injsw = switches_[injaddr];

    for (int i=0; i < num_ports; ++i){
      int injector_port = i;
      int switch_port = ports[i];
      interconn_debug("connecting switch %d to injector %d on ports %d:%d",
          int(injaddr), int(nodeaddr), switch_port, injector_port);
      injsw->connect_input(ej_params, injector_port, switch_port, ep);
      ep->connect_output(inj_params, injector_port, switch_port, injsw);
    }

    switch_id ejaddr = topology_->endpoint_to_ejection_switch(nodeaddr, ports, num_ports);
    connectable* ejsw = switches_[ejaddr];

    for (int i=0; i < num_ports; ++i){
      int ejector_port = i;
      int switch_port = ports[i];
      interconn_debug("connecting switch %d to ejector %d on ports %d:%d",
          int(ejaddr), int(nodeaddr), switch_port, ejector_port);
      ejsw->connect_output(ej_params, switch_port, ejector_port, ep);
      ep->connect_input(inj_params, switch_port, ejector_port, ejsw);
    }
  }
}

void
interconnect::build_endpoints(sprockit::sim_parameters* node_params,
                  sprockit::sim_parameters* nic_params,
                  sprockit::sim_parameters* netlink_params,
                  event_manager* mgr)
{
  sprockit::sim_parameters* nlink_inj_params =
      netlink_params->get_optional_namespace("injection");
  sprockit::sim_parameters* inj_params = nic_params->get_namespace("injection");

  int my_rank = rt_->me();
  simple_switch* local_speedy_switch = safe_cast(simple_switch,
                                         speedy_overlay_switches_[my_rank]);

  for (int i=0; i < num_switches_; ++i){
    switch_id sid(i);
    int target_rank = partition_->lpid_for_switch(sid);
    std::vector<topology::injection_port> nodes;
    topology_->nodes_connected_to_injection_switch(sid, nodes);
    if (target_rank == my_rank){
      for (int n=0; n < nodes.size(); ++n){
        node_id nid = nodes[n].nid;
        node_to_speedy_switch_[nid] = switch_id(target_rank);
        if (my_rank == target_rank){
          //local node - actually build it
          node_params->add_param_override("id", int(nid));
          node* nd = node_factory::get_optional_param("model", "simple", node_params,
                                                      nid, mgr);
          nic* the_nic = nd->get_nic();
          nodes_[nid] = nd;
          nics_[nid] = the_nic;

          the_nic->set_speedy_switch(local_speedy_switch);
          local_speedy_switch->add_nic(the_nic->mtl_handler(), nid);

          nd->init(0); //emulate SST core
          nd->setup();

          node_id netlink_id;
          int netlink_offset;
          bool has_netlink = topology_->node_to_netlink(nid, netlink_id, netlink_offset);
          if (has_netlink && netlink_offset == 0){
            interconn_debug("Adding netlink %d connected to switch %d on rank %d",
              int(netlink_id), i, my_rank);
            netlink_params->add_param_override("id", int(netlink_id));
            netlink* nlink = netlink_factory::get_param("model", netlink_params, nd);
            netlinks_[netlink_id] = nlink;

            int the_only_port = 0;
            int inj_port = nlink->node_port(netlink_offset);
            nlink->connect_input(nlink_inj_params,
                          the_only_port, inj_port,
                          the_nic);
            the_nic->connect_output(inj_params,
                             the_only_port, inj_port,
                             nlink);

            nlink->connect_output(nlink_inj_params,
                          inj_port, the_only_port,
                          the_nic);
            the_nic->connect_input(inj_params,
                             inj_port, the_only_port,
                             nlink);
          }
        } else {
          local_speedy_switch->add_switch(speedy_overlay_switches_[target_rank], nid);
        }
      }
    }
  }
}

void
interconnect::build_switches(sprockit::sim_parameters* switch_params,
                             event_manager* mgr)
{
  bool simple_model = switch_params->get_param("model") == "simple";
  if (simple_model) return; //nothing to do

  int my_rank = rt_->me();
  bool all_switches_same = topology_->uniform_switches();
  for (int i=0; i < num_switches_; ++i){
    switch_params->add_param_override("id", i);
    if (partition_->lpid_for_switch(i) == my_rank){
      if (!all_switches_same)
        topology_->configure_nonuniform_switch_params(i, switch_params);
      switches_[i] = network_switch_factory::get_param("model",
                      switch_params, i, mgr);
    } else {
      switches_[i] = new dist_dummy_switch(switch_params, i, mgr);
    }
  }

  for (network_switch* netsw : switches_){
    if (!netsw->ipc_handler()){
      netsw->compatibility_check();
      break;
    }
  }
}

void
interconnect::connect_switches(sprockit::sim_parameters* switch_params)
{
  bool simple_model = switch_params->get_param("model") == "simple";
  if (simple_model) return; //nothing to do

  std::vector<topology::connection> outports(64); //allocate 64 spaces optimistically

  //might be super uniform in which all ports are the same
  bool all_ports_same = topology_->uniform_network_ports();
  //or it might be mostly uniform in which all the switches are the same
  //even if the individual ports on each switch are different
  bool all_switches_same = topology_->uniform_switches_non_uniform_network_ports();

  sprockit::sim_parameters* port_params;
  if (all_ports_same){
    port_params = switch_params->get_namespace("link");
  } else if (all_switches_same){
    topology_->configure_individual_port_params(switch_id(0), switch_params);
  }

  for (int i=0; i < num_switches_; ++i){
    switch_id src(i);
    topology_->connected_outports(src, outports);
    network_switch* src_sw = switches_[src];
    if (!all_switches_same) topology_->configure_individual_port_params(src, switch_params);
    for (topology::connection& conn : outports){
      if (!all_ports_same){
        port_params = topology::get_port_params(switch_params, conn.src_outport);
      }

      network_switch* dst_sw = switches_[conn.dst];

      interconn_debug("%s connecting to %s on ports %d:%d",
                topology_->label(src).c_str(),
                topology_->label(conn.dst).c_str(),
                conn.src_outport, conn.dst_inport);

      src_sw->connect_output(port_params,
                             conn.src_outport,
                             conn.dst_inport,
                             dst_sw);
      dst_sw->connect_input(port_params,
                            conn.src_outport,
                            conn.dst_inport,
                            src_sw);
    }
  }

}

void
interconnect::deadlock_check()
{
  for (network_switch* netsw : switches_){
    if (netsw && !netsw->ipc_handler())
      netsw->deadlock_check();
  }
  for (netlink* nlink : netlinks_){
    if (nlink) nlink->deadlock_check();
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

