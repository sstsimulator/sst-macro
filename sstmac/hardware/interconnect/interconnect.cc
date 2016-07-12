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
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/nic/netlink.h>
#include <sstmac/hardware/common/fail_event.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/packet_flow/packet_flow.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/statics.h>
#include <sprockit/delete.h>
#include <sprockit/output.h>
#include <sprockit/sim_parameters.h>

ImplementFactory(sstmac::hw::interconnect)
RegisterDebugSlot(interconnect);
RegisterNamespaces("interconnect");

namespace sstmac {

sprockit::StaticNamespaceRegister node_ns_reg("node");

namespace hw {

//static sprockit::need_delete_statics<interconnect> del_statics;

interconnect* interconnect::static_interconnect_ = 0;

#if !SPKT_DISABLE_REGEX
sprockit::StaticKeywordRegisterRegexp node_failure_ids_keyword("node_failure_\\d+_id");
sprockit::StaticKeywordRegisterRegexp node_failure_time_keyword("node_failure_\\d+_time");
#endif

#if SSTMAC_INTEGRATED_SST_CORE
SpktRegister("sst", interconnect, sst_interconnect,
            "Skeleton sst interconnect to make things work");
#endif

interconnect::interconnect() :
 topology_(0)
{
}

interconnect::~interconnect() 
{
  sprockit::delete_vals(nodes_);
  sprockit::delete_vals(nics_);
}

int
interconnect::num_nodes() const
{
  return topology_->num_nodes();
}

void
interconnect::init_factory_params(sprockit::sim_parameters *params)
{
  STATIC_INIT_TOPOLOGY(params)
}

interconnect*
interconnect::static_interconnect(sprockit::sim_parameters* params)
{
  if (!static_interconnect_){
    sprockit::sim_parameters* ic_params = params->top_parent()->get_namespace("interconnect");
    static_interconnect_ = interconnect_factory::get_optional_param("name", "sst", ic_params,
      0/*no partition*/, 0/*no parallel runtime*/);
  }
  return static_interconnect_;
}

void
interconnect::set_topology(topology* top)
{
  topology_ = top;
}

#if SSTMAC_INTEGRATED_SST_CORE
void
sst_interconnect::init_factory_params(sprockit::sim_parameters* params)
{
  interconnect::init_factory_params(params);
}

void
sst_interconnect::kill_node(node_id nid)
{
  spkt_throw(sprockit::unimplemented_error, "interconnect::kill_node");
}

void
sst_interconnect::kill_node(node_id nid, timestamp t)
{
  spkt_throw(sprockit::unimplemented_error, "interconnect::kill_node");
}
#else
macro_interconnect::macro_interconnect() :
  partition_(0)
{
}

macro_interconnect::~macro_interconnect()
{
  delete topology_;
  sprockit::delete_vals(netlinks_);
}


void
macro_interconnect::kill_node(node_id nid)
{
  spkt_throw(sprockit::unimplemented_error, "interconnect::kill_node");
}

void
macro_interconnect::kill_node(node_id nid, timestamp t)
{
  spkt_throw(sprockit::unimplemented_error, "interconnect::kill_node");
}

void
macro_interconnect::init_factory_params(sprockit::sim_parameters* params)
{
  interconnect::init_factory_params(params);


  /** sstkeyword {
      gui=torus;
      docstring=Specify the basic topology type used for the
      network interconnect;
  }
  */
  sprockit::sim_parameters* top_params = params->get_namespace("topology");
  topology_ = topology_factory::get_param("name", top_params);

  runtime::set_topology(topology_);

  endpoint_map nodes;
  endpoint_map nics;
  endpoint_map netlinks;

  sprockit::sim_parameters* node_params = params->get_namespace("node");
  sprockit::factory<connectable>* node_builder
    = new sprockit::template_factory<connectable, node_factory>(node_params->get_param("model"));
  topology_->build_endpoint_connectables(nodes, node_builder, partition_, rt_->me(), node_params);
  delete node_builder;

  int nic_conc = 1;
  sprockit::sim_parameters* nic_params = params->get_namespace("nic");
  sprockit::factory2<connectable>* nic_builder
    = new sprockit::template_factory2<connectable, nic_factory>(nic_params->get_param("model"));
  topology_->build_interface_connectables(nic_conc, nics, nic_builder, partition_, rt_->me(), nic_params, this);
  delete nic_builder;

  if (params->has_namespace("netlink")){
    int netlink_conc = topology_->num_nodes_per_netlink();
    sprockit::sim_parameters* netlink_params = params->get_namespace("netlink");
    sprockit::factory2<connectable>* netlink_builder
      = new sprockit::template_factory2<connectable, netlink_factory>(netlink_params->get_param("model"));
    topology_->build_interface_connectables(netlink_conc, netlinks, netlink_builder,
                  partition_, rt_->me(), netlink_params, this);
    delete netlink_builder;
  }


  copy_map(nodes, nodes_);
  copy_map(nics, nics_);
  copy_map(netlinks, netlinks_);

  node_map::iterator it, end = nodes_.end();
  for (it=nodes_.begin(); it != end; ++it){
    node_id nid = it->first;
    node* nd = it->second;
    nic* nc = nics_[nid];
    nd->set_nic(nc);
    nc->set_node(nd);
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
}

void
macro_interconnect::handle(event* ev)
{
  spkt_throw(sprockit::value_error, "interconnect should never handle messages");
}

void
macro_interconnect::set_event_manager_common(event_manager* m)
{
  std::list<node_fail_event>::iterator it, end = failures_to_schedule_.end();
  for (it=failures_to_schedule_.begin(); it != end; ++it){
    fail_event* fail_msg = new fail_event;
    node_fail_event ev = *it;

    //I might not own this node
    node_map::iterator it = nodes_.find(ev.second);
    if (it != nodes_.end()){
      //yep this is mine
      node* node_to_fail = it->second;
      timestamp time_to_fail = ev.first;
      node_to_fail->schedule(time_to_fail, node_to_fail, fail_msg);
      coutn << "scheduling failure at " << time_to_fail
            << " on node " << ev.second << std::endl;
    }
    else if (ev.second > num_nodes()){
      spkt_throw_printf(sprockit::value_error,
        "interconnect::set_event_manager: invalid node id %d for failure",
        int(ev.second));
    }
  }
}

void
macro_interconnect::set_node_event_manager(node* the_node, event_manager* m)
{
  the_node->set_event_manager(m);
  if (!netlinks_.empty()){
    netlink_id netid(the_node->addr()/topology_->num_nodes_per_netlink());
    node* theNode = nodes_[netid];
    netlinks_[netid]->set_event_parent(theNode);
  }
}

void
macro_interconnect::set_event_manager(event_manager* m)
{
  runtime::set_topology(topology_);

  node_map::iterator it, end = nodes_.end();
  for (it=nodes_.begin(); it != end; ++it) {
    node* the_node = it->second;
    set_node_event_manager(the_node, m);
  }

  set_event_manager_common(m);
}
#endif

}
}

