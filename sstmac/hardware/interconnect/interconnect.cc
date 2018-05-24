/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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
#include <sstmac/hardware/logp/logp_param_expander.h>
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

namespace sstmac {
namespace hw {

interconnect* interconnect::static_interconnect_ = nullptr;

interconnect*
interconnect::static_interconnect(sprockit::sim_parameters* params, event_manager* mgr)
{
  if (!static_interconnect_){
    parallel_runtime* rt = parallel_runtime::static_runtime(params);
    partition* part = rt ? rt->topology_partition() : nullptr;
    static_interconnect_ = interconnect::factory::get_value("switch", params,
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
  sprockit::delete_vector(logp_switches_);
  sprockit::delete_vector(switches_);
}
#endif

interconnect::interconnect(sprockit::sim_parameters *params, event_manager *mgr,
                           partition *part, parallel_runtime *rt)
{
  if (!static_interconnect_) static_interconnect_ = this;
  topology_ = topology::static_topology(params);
  num_nodes_ = topology_->num_nodes();
  num_switches_ = topology_->num_switches();
  num_leaf_switches_ = topology_->num_leaf_switches();
  runtime::set_topology(topology_);

#if !SSTMAC_INTEGRATED_SST_CORE
  components_.resize(topology_->num_nodes() + topology_->num_switches());

  partition_ = part;
  rt_ = rt;
  int nproc = rt_->nproc();
  num_speedy_switches_with_extra_node_ = num_nodes_ % nproc;
  num_nodes_per_speedy_switch_ = num_nodes_ / nproc;

  sprockit::sim_parameters* node_params = params->get_namespace("node");
  sprockit::sim_parameters* nic_params = node_params->get_namespace("nic");
  sprockit::sim_parameters* inj_params = nic_params->get_namespace("injection");
  sprockit::sim_parameters* switch_params = params->get_namespace("switch");
  sprockit::sim_parameters* ej_params = switch_params->get_optional_namespace("ejection");
  sprockit::sim_parameters* netlink_params = params->get_optional_namespace("netlink");
  sprockit::sim_parameters* nlink_inj_params = netlink_params->get_optional_namespace("injection");
  sprockit::sim_parameters* nlink_ej_params = netlink_params->get_optional_namespace("ejection");

  topology* top = topology_;

  std::string switch_model = switch_params->get_lowercase_param("model");
  bool logp_model = switch_model == "logp" || switch_model == "simple" || switch_model == "macrels";

  switches_.resize(top->max_switch_id());
  nodes_.resize(top->max_node_id());
  netlinks_.resize(top->max_netlink_id());

  sprockit::sim_parameters logp_params;
  if (logp_model){
    switch_params->combine_into(&logp_params);
  } else {
    logp_param_expander expander;
    expander.expand_into(&logp_params, params, switch_params);
  }

  logp_switches_.resize(rt_->nthread());
  uint32_t my_offset = rt_->me() * rt_->nthread() + top->num_nodes() + top->num_switches();
  for (int i=0; i < rt_->nthread(); ++i){
    uint32_t id = my_offset + i;
    logp_switches_[i] = new logp_switch(&logp_params, id, mgr->thread_manager(i));
  }



  interconn_debug("Interconnect building endpoints");
  build_endpoints(node_params, nic_params,netlink_params, mgr);
  if (!logp_model){
    interconn_debug("Interconnect building switches");
    build_switches(switch_params, mgr);
    interconn_debug("Interconnect connecting switches");
    connect_switches(mgr, switch_params);
    interconn_debug("Interconnect connecting endpoints");
    if (netlinks_.empty()){
      connect_endpoints(mgr, inj_params, inj_params, ej_params);
    } else {
      connect_endpoints(mgr, nlink_inj_params, nlink_ej_params, ej_params);
    }
    configure_interconnect_lookahead(params);
  } else {
    //lookahead is actually higher
    logp_switch* lsw = logp_switches_[0];
    lookahead_ = lsw->send_latency(nullptr);
  }

  timestamp lookahead_check = lookahead_;
  if (event_link::min_remote_latency().ticks() > 0){
    lookahead_check = event_link::min_remote_latency();
  }
  if (event_link::min_thread_latency().ticks() > 0){
    lookahead_check = std::min(lookahead_check, event_link::min_thread_latency());
  }

  if (lookahead_check < lookahead_){
    spkt_abort_printf("invalid lookahead compute: computed lookahead to be %8.4e, "
                      "but have link with lookahead %8.4e", lookahead_.sec(), lookahead_check.sec());
  }

#endif
}

#if !SSTMAC_INTEGRATED_SST_CORE
void
interconnect::configure_interconnect_lookahead(sprockit::sim_parameters* params)
{
  sprockit::sim_parameters* switch_params = params->get_namespace("switch");
  sprockit::sim_parameters* inj_params = params->get_namespace("node")
      ->get_namespace("nic")->get_namespace("injection");
  sprockit::sim_parameters* ej_params = params->get_optional_namespace("ejection");

  sprockit::sim_parameters* link_params = switch_params->get_namespace("link");
  timestamp hop_latency;
  if (link_params->has_param("send_latency")){
    hop_latency = link_params->get_time_param("send_latency");
  } else {
    hop_latency = link_params->get_time_param("latency");
  }
  timestamp injection_latency = inj_params->get_time_param("latency");

  timestamp ejection_latency = injection_latency;
  if (ej_params->has_param("latency")){
    ejection_latency = ej_params->get_time_param("latency");
  } else if (ej_params->has_param("send_latency")){
    ejection_latency = ej_params->get_time_param("send_latency");
  }

  lookahead_ = std::min(injection_latency, hop_latency);
  lookahead_ = std::min(lookahead_, ejection_latency);
}
#endif

switch_id
interconnect::node_to_logp_switch(node_id nid) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return topology_->node_to_logp_switch(nid);
#else
  uint16_t ignore;
  switch_id real_sw_id = topology_->node_to_injection_switch(nid, ignore);
  int target_rank = partition_->lpid_for_switch(real_sw_id);
  return target_rank;
#endif
}


#if !SSTMAC_INTEGRATED_SST_CORE
event_link*
interconnect::allocate_local_link(event_scheduler* src, event_scheduler* dst,
                                  event_handler* handler, timestamp latency)
{
  bool threads_equal = src && dst ? src->thread() == dst->thread() : false;
  event_manager* mgr = src ? src->event_mgr() : dst->event_mgr();
  if (mgr->nthread() == 1 || threads_equal){
    return new local_link(latency,src,dst,handler);
  } else {
    return new multithread_link(handler,latency,src,dst);
  }
}

void
interconnect::connect_endpoints(event_manager* mgr,
                                sprockit::sim_parameters* ep_inj_params,
                                sprockit::sim_parameters* ep_ej_params,
                                sprockit::sim_parameters* sw_ej_params)
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
    uint16_t inj_ports[32];
    int num_ej_ports;
    uint16_t ej_ports[32];
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

    event_scheduler* parent_node = nodes_[nodeaddr];
    if (has_netlink){
      ep = netlinks_[netlink_id];
    }	else {
      ep = nodes_[nodeaddr]->get_nic();
    }

    // connect endpoints to switches
    network_switch* injsw = switches_[injaddr];
    for (int i=0; i < num_inj_ports; ++i){
      int injector_port = i;
      int switch_port = inj_ports[i];
      interconn_debug("connecting switch %d:%p to injector %d:%p on ports %d:%d",
          int(injaddr), injsw, ep_id, ep, switch_port, injector_port);
      auto credit_link = allocate_local_link(injsw, parent_node, ep->credit_handler(injector_port),
                                             injsw->credit_latency(sw_ej_params));
      injsw->connect_input(sw_ej_params, injector_port, switch_port, credit_link);
      auto payload_link = allocate_local_link(parent_node, injsw, injsw->payload_handler(switch_port),
                                              ep->send_latency(ep_inj_params));
      ep->connect_output(ep_inj_params, injector_port, switch_port, payload_link);
    }

    // connect switches to endpoints
    network_switch* ejsw = switches_[ejaddr];
    for (int i=0; i < num_ej_ports; ++i){
      int ejector_port = i;
      int switch_port = ej_ports[i];
      interconn_debug("connecting switch %d:%p to ejector %d:%p on ports %d:%d",
          int(ejaddr), ejsw, ep_id, ep, switch_port, ejector_port);
      auto payload_link = allocate_local_link(ejsw, parent_node, ep->payload_handler(ejector_port),
                                              ejsw->send_latency(sw_ej_params));
      ejsw->connect_output(sw_ej_params, switch_port, ejector_port, payload_link);
      auto credit_link = allocate_local_link(parent_node, ejsw, ejsw->credit_handler(switch_port),
                                             ep->credit_latency(ep_ej_params));
      ep->connect_input(ep_ej_params, switch_port, ejector_port, credit_link);
    }
  }
}

void
interconnect::setup()
{
  for (node* nd : nodes_){
    if (nd){
      nd->init(0); //emulate SST core
      nd->setup();
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
  sprockit::sim_parameters* nlink_inj_params =
      netlink_params->get_optional_namespace("injection");
  sprockit::sim_parameters* inj_params = nic_params->get_namespace("injection");

  int my_rank = rt_->me();

  for (int i=0; i < num_leaf_switches_; ++i){
    switch_id sid(i);
    int thread = partition_->thread_for_switch(i);
    int target_rank = partition_->lpid_for_switch(sid);
    std::vector<topology::injection_port> nodes;
    topology_->nodes_connected_to_injection_switch(sid, nodes);
    interconn_debug("switch %d maps to target rank %d", i, target_rank);

    for (int n=0; n < nodes.size(); ++n){
      node_id nid = nodes[n].nid;
      if (!topology_->node_id_slot_filled(nid))
        continue;

      auto local_logp_switch = logp_switches_[thread];

      int port = nodes[n].port;
      if (my_rank == target_rank){
        //local node - actually build it
        node_params->add_param_override("id", int(nid));
        uint32_t comp_id = nid;
        event_manager* node_mgr = mgr->thread_manager(thread);
        node* nd = node::factory::get_optional_param("model", "simple", node_params,
                                                     comp_id, node_mgr);
        node_params->remove_param("id"); //you don't have to let it linger
        nic* the_nic = nd->get_nic();

        //nic sends to only its specific logp switch
        the_nic->set_logp_switch(local_logp_switch);

        nodes_[nid] = nd;
        components_[nid] = nd;

        for (int i=0; i < rt_->nthread(); ++i){
          event_link* out_link = allocate_local_link(nullptr, nd, nd->payload_handler(nic::LogP),
                                                     local_logp_switch->send_latency(nullptr));
          logp_switches_[i]->connect_output(nid, out_link);
        }


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
          netlink_params->remove_param("id");
          int inj_port = nlink->node_port(netlink_offset);
          interconn_debug("Adding netlink %d connected to switch %d, node %d on port %d for rank %d",
            int(net_id), i, nid, inj_port, my_rank);

          // connect nic to netlink

          auto link = allocate_local_link(nd, nd, the_nic->credit_handler(nic::Injection),
                                          nlink->credit_latency(nlink_inj_params));
          nlink->connect_input(nlink_ej_params,
                               nic::Injection, inj_port,
                               link);
          link = allocate_local_link(nd, nd, nlink->payload_handler(inj_port),
                                     the_nic->send_latency(inj_params));
          the_nic->connect_output(inj_params,
                           nic::Injection, inj_port,
                           link);

          // connect netlink to nic
          link = allocate_local_link(nd, nd, the_nic->payload_handler(nic::Injection),
                                     nlink->send_latency(nlink_ej_params));
          nlink->connect_output(nlink_ej_params,
                        inj_port, nic::Injection,
                        link);
          link = allocate_local_link(nd, nd, nlink->credit_handler(inj_port),
                                     the_nic->credit_latency(inj_params));
          the_nic->connect_input(inj_params,
                           inj_port, nic::Injection,
                           link);
        }
      } else {
        for (int i=0; i < rt_->nthread(); ++i){
          event_link* link = new ipc_link(local_logp_switch->send_latency(nullptr),
                                          target_rank, nullptr,
                                          nid, nic::LogP, false);
          logp_switches_[i]->connect_output(nid, link);
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
  switch_id num_switch_ids = topology_->max_switch_id();
  int id_offset = topology_->num_nodes();
  for (switch_id i=0; i < num_switch_ids; ++i){
    if (!topology_->switch_id_slot_filled(i))
      continue; //don't build

    switch_params->add_param_override("id", int(i));
    if (partition_->lpid_for_switch(i) == my_rank){
      if (!all_switches_same)
        topology_->configure_nonuniform_switch_params(i, switch_params);
      int thread = partition_->thread_for_switch(i);
      uint32_t comp_id = i + topology_->num_nodes();
      switches_[i] = network_switch::factory::get_param("model",
                      switch_params, comp_id, mgr->thread_manager(thread));
    } else {
      switches_[i] = nullptr;
    }
    switch_params->remove_param("id");
    components_[i+id_offset] = switches_[i];
  }

  for (network_switch* netsw : switches_){
    if (netsw) netsw->compatibility_check();
  }
}

uint32_t
interconnect::switch_component_id(switch_id sid) const
{
  return topology_->num_nodes() + sid;
}

void
interconnect::connect_switches(event_manager* mgr, sprockit::sim_parameters* switch_params)
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
    int thread = partition_->thread_for_switch(i);
    topology_->connected_outports(src, outports);
    network_switch* src_sw = switches_[src];
    if (!all_switches_same) topology_->configure_individual_port_params(src, switch_params);
    interconn_debug("interconnect: num intranetwork ports: %i", outports.size());
    for (topology::connection& conn : outports){
      if (!all_ports_same){
        port_params = topology::get_port_params(switch_params, conn.src_outport);
      }

      network_switch* dst_sw = switches_[conn.dst];
      interconn_debug("%s connecting to %s on ports %d:%d",
                topology_->switch_label(src).c_str(),
                topology_->switch_label(conn.dst).c_str(),
                conn.src_outport, conn.dst_inport);

      if (src_sw){
        event_link* payload_link = nullptr;
        if (dst_sw){
          payload_link = allocate_local_link(src_sw, dst_sw, dst_sw->payload_handler(conn.dst_inport),
                                             src_sw->send_latency(port_params));
        } else {
          int dst_rank = partition_->lpid_for_switch(conn.dst);
          uint32_t dst = switch_component_id(conn.dst);
          payload_link = new ipc_link(src_sw->send_latency(port_params),
                                      dst_rank, src_sw,
                                      dst, conn.dst_inport, false);
        }
        src_sw->connect_output(port_params,
                               conn.src_outport,
                               conn.dst_inport,
                               payload_link);
      }

      if (dst_sw){
        event_link* credit_link = nullptr;
        if (src_sw){
          credit_link = allocate_local_link(dst_sw, src_sw, src_sw->credit_handler(conn.src_outport),
                                            dst_sw->credit_latency(port_params));
        } else {
          int src_rank = partition_->lpid_for_switch(conn.src);
          uint32_t src = switch_component_id(conn.src);
          credit_link = new ipc_link(dst_sw->credit_latency(port_params),
                                     src_rank, dst_sw, src, conn.src_outport, true);
        }
        dst_sw->connect_input(port_params,
                              conn.src_outport,
                              conn.dst_inport,
                              credit_link);
      }
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
#endif

}
}
