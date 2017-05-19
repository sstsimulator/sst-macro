/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/nic/netlink.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/pisces/pisces.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/switch/dist_dummyswitch.h>
#include <sstmac/hardware/logp/logp_switch.h>
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


RegisterDebugSlot(interconnect);
RegisterNamespaces("interconnect");
RegisterKeywords("network_name", "interconnect");

namespace sstmac {
namespace hw {

interconnect* interconnect::static_interconnect_ = nullptr;

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
    static_interconnect_ = interconnect::factory::get_optional_param(ic_param, "switch", ic_params,
      mgr, part, rt);
  }
  return static_interconnect_;
}

interconnect*
interconnect::static_interconnect()
{
  if (!static_interconnect_){
    spkt_abort_printf("interconnect not initialized");
  }
  return static_interconnect_;
}

#if !SSTMAC_INTEGRATED_SST_CORE
interconnect::~interconnect()
{
  sprockit::delete_vector(netlinks_);
  sprockit::delete_vector(nodes_);
  for (network_switch* sw : logp_overlay_switches_){
    if (sw) delete sw;
  }
  for (network_switch* sw : switches_){
    delete sw;
  }
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
  int nthread = rt_->nthread();
  sprockit::sim_parameters* netlink_params = params->get_optional_namespace("netlink");
  sprockit::sim_parameters* nlink_inj_params =
      netlink_params->get_optional_namespace("injection");
  num_speedy_switches_with_extra_node_ = num_nodes_ % nproc;
  num_nodes_per_speedy_switch_ = num_nodes_ / nproc;
  logp_overlay_switches_.resize(nproc*nthread);

  sprockit::sim_parameters* node_params = params->get_namespace("node");
  sprockit::sim_parameters* nic_params = node_params->get_namespace("nic");
  sprockit::sim_parameters* inj_params = nic_params->get_namespace("injection");
  sprockit::sim_parameters* switch_params = params->get_namespace("switch");
  sprockit::sim_parameters* ej_params = switch_params->get_namespace("ejection");
  topology* top = topology_;

  bool logp_model = switch_params->get_param("model") == "logP";

  switches_.resize(top->max_switch_id());
  nodes_.resize(top->max_node_id());
  netlinks_.resize(top->max_netlink_id());

  local_logp_switch_ = my_rank;
  logp_switch* local_logp_switch;
  for (int i=0; i < nproc; ++i){
    int offset = i*nthread;
    for (int t=0; t < nthread; ++t){
      switch_id sid(t+offset);
      switch_params->add_param_override("id", int(sid));
      if (i == my_rank){
        logp_overlay_switches_[sid] = new logp_switch(switch_params, sid, mgr->ev_man_for_thread(t));
      } else {
        logp_overlay_switches_[sid] = new dist_dummy_switch(switch_params, sid, mgr, 
                                                            device_id::logp_overlay);
      }
    }
  }

  build_endpoints(node_params, nic_params,netlink_params, mgr);
  if (!logp_model){
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
#endif
}

switch_id
interconnect::node_to_logp_switch(node_id nid) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return topology_->node_to_logp_switch(nid);
#else
  int ignore;
  switch_id real_sw_id = topology_->node_to_injection_switch(nid, ignore);
  int target_rank = partition_->lpid_for_switch(real_sw_id);
  return target_rank;
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
  int me = rt_->me();
  for (int nodeaddr=0; nodeaddr < num_nodes; ++nodeaddr){
    node_id netlink_id;
    int netlink_offset;
    connectable* ep = nullptr;
    switch_id injaddr;
    switch_id ejaddr;
    int ep_id;
    //map to topology-specific port
    int num_inj_ports;
    int inj_ports[32];
    int num_ej_ports;
    int ej_ports[32];
    bool has_netlink = topology_->node_to_netlink(nodeaddr, netlink_id, netlink_offset);
    if (has_netlink) {
      if (netlink_offset == 0){
        injaddr = topology_->netlink_to_injection_switch(netlink_id, inj_ports, num_inj_ports);
        ejaddr = topology_->netlink_to_injection_switch(netlink_id, ej_ports, num_ej_ports);
        ep_id = netlink_id;
      } else {
        continue; //no connection required
      }
    } else {
      injaddr = topology_->node_to_injection_switch(nodeaddr, inj_ports, num_inj_ports);
      ejaddr = topology_->node_to_injection_switch(nodeaddr, ej_ports, num_ej_ports);
      ep_id = nodeaddr;
    }

    //parallel - I don't own this
    int target_rank = partition_->lpid_for_switch(injaddr);
    if (target_rank != me) continue;

    if (has_netlink){
      ep = netlinks_[netlink_id];
    }	else {
      ep = nodes_[nodeaddr]->get_nic();
    }

    network_switch* injsw = switches_[injaddr];
    for (int i=0; i < num_inj_ports; ++i){
      int injector_port = i;
      int switch_port = inj_ports[i];
      interconn_debug("connecting switch %d to injector %d on ports %d:%d",
          int(injaddr), ep_id, switch_port, injector_port);
      injsw->connect_input(ej_params, injector_port, switch_port,
                           ep->credit_handler(injector_port));
      ep->connect_output(inj_params, injector_port, switch_port,
                         injsw->payload_handler(switch_port));
    }


    network_switch* ejsw = switches_[ejaddr];
    for (int i=0; i < num_ej_ports; ++i){
      int ejector_port = i;
      int switch_port = ej_ports[i];
      interconn_debug("connecting switch %d to ejector %d on ports %d:%d",
          int(ejaddr), ep_id, switch_port, ejector_port);
      ejsw->connect_output(ej_params, switch_port, ejector_port,
                           ep->payload_handler(ejector_port));
      ep->connect_input(inj_params, switch_port, ejector_port,
                        ejsw->credit_handler(switch_port));
    }
  }
}

void
interconnect::build_endpoints(sprockit::sim_parameters* node_params,
                  sprockit::sim_parameters* nic_params,
                  sprockit::sim_parameters* netlink_params,
                  event_manager* mgr)
{
  sprockit::sim_parameters* nlink_ej_params =
      netlink_params->get_optional_namespace("ejection");
  sprockit::sim_parameters* inj_params = nic_params->get_namespace("injection");

  int my_rank = rt_->me();
  logp_switch* local_logp_switch = safe_cast(logp_switch,
                                         logp_overlay_switches_[my_rank]);

  for (int i=0; i < num_switches_; ++i){
    switch_id sid(i);
    int thread = partition_->thread_for_switch(i);
    event_manager* thread_mgr = mgr->ev_man_for_thread(thread);
    int target_rank = partition_->lpid_for_switch(sid);
    std::vector<topology::injection_port> nodes;
    topology_->nodes_connected_to_injection_switch(sid, nodes);
    interconn_debug("switch %d maps to target rank %d", i, target_rank);

    for (int n=0; n < nodes.size(); ++n){
      node_id nid = nodes[n].nid;
      if (!topology_->node_id_slot_filled(nid))
        continue;

      int port = nodes[n].port;
      if (my_rank == target_rank){
        //local node - actually build it
        node_params->add_param_override("id", int(nid));
        node* nd = node::factory::get_optional_param("model", "simple", node_params,
                                                    nid, thread_mgr);
        nic* the_nic = nd->get_nic();
        nodes_[nid] = nd;

        the_nic->connect_output(
              inj_params,
              nic::LogP,
              0, //does not matter
              local_logp_switch->payload_handler(port));
        local_logp_switch->connect_output(inj_params,
                                    nid, //the outport is he node
                                    logp_switch::Node, //signal node connection
                                    the_nic->mtl_handler());

        nd->init(0); //emulate SST core
        nd->setup();

        netlink_id net_id;
        int netlink_offset;
        bool has_netlink = topology_->node_to_netlink(nid, net_id, netlink_offset);
        if (has_netlink){
          netlink_params->add_param_override("id", int(net_id));
          netlink* nlink = netlinks_[net_id];
          if (!nlink){
            nlink = netlink::factory::get_param("model", netlink_params, nd);
            netlinks_[net_id] = nlink;
          }
          int inj_port = nlink->node_port(netlink_offset);
          interconn_debug("Adding netlink %d connected to switch %d, node %d on port %d for rank %d",
            int(net_id), i, nid, inj_port, my_rank);

          nlink->connect_input(nlink_ej_params,
                        nic::Injection, inj_port,
                        the_nic->credit_handler(nic::Injection));
          the_nic->connect_output(inj_params,
                           nic::Injection, inj_port,
                           nlink->payload_handler(inj_port));

          nlink->connect_output(nlink_ej_params,
                        inj_port, nic::Injection,
                        the_nic->payload_handler(nic::Injection));
          the_nic->connect_input(inj_params,
                           inj_port, nic::Injection,
                           nlink->credit_handler(inj_port));
        }
      } else {
        local_logp_switch->connect_output(
          inj_params,
          target_rank,
          logp_switch::Switch,
          logp_overlay_switches_[target_rank]->payload_handler(port));
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
  switch_id num_switch_ids = topology_->max_switch_id();
  for (switch_id i=0; i < num_switch_ids; ++i){
    if (!topology_->switch_id_slot_filled(i))
      continue; //don't build

    switch_params->add_param_override("id", int(i));
    if (partition_->lpid_for_switch(i) == my_rank){
      if (!all_switches_same)
        topology_->configure_nonuniform_switch_params(i, switch_params);
      int thread = partition_->thread_for_switch(i);
      event_manager* thread_mgr = mgr->ev_man_for_thread(thread);
      switches_[i] = network_switch::factory::get_param("model",
                      switch_params, i, thread_mgr);
    } else {
      switches_[i] = new dist_dummy_switch(switch_params, i, mgr, device_id::router);
    }
  }

  for (network_switch* netsw : switches_){
    netsw->compatibility_check();
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
                topology_->switch_label(src).c_str(),
                topology_->switch_label(conn.dst).c_str(),
                conn.src_outport, conn.dst_inport);

      src_sw->connect_output(port_params,
                             conn.src_outport,
                             conn.dst_inport,
                             dst_sw->payload_handler(conn.dst_inport));
      dst_sw->connect_input(port_params,
                            conn.src_outport,
                            conn.dst_inport,
                            src_sw->credit_handler(conn.src_outport));
    }
  }

}

void
interconnect::deadlock_check()
{
  for (network_switch* netsw : switches_){
    if (netsw)
      netsw->deadlock_check();
  }
  for (netlink* nlink : netlinks_){
    if (nlink) nlink->deadlock_check();
  }
  for (node* nd: nodes_){
    if (nd) nd->deadlock_check();
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