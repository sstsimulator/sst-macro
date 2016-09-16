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


#include <sstmac/hardware/simple/simple_topology.h>
#include <sstmac/hardware/topology/cartesian_topology.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <stdio.h>
#include <sstream>

namespace sstmac {
namespace hw {

SpktRegister("simple", topology, simple_topology,
            "simple topology that emulates a simple_interconnect via switches");

switch_id
simple_topology::endpoint_to_injection_switch(node_id nodeaddr, int &switch_port) const
{
  int first_cutoff = (endpoints_per_switch_ + 1) * num_switches_with_extra_node_;
  if (nodeaddr >= first_cutoff){
    int offsetaddr = nodeaddr - first_cutoff;
    int id = (offsetaddr / endpoints_per_switch_) + num_switches_with_extra_node_;
    switch_port = injection;
    return switch_id(id);
  }
  else {
    int id = nodeaddr / (endpoints_per_switch_+1);
    switch_port = injection;
    return switch_id(id);
  }
}

switch_id
simple_topology::endpoint_to_ejection_switch(node_id nodeaddr, int &switch_port) const
{
  return endpoint_to_injection_switch(nodeaddr, switch_port);
}

simple_topology::~simple_topology()
{
  if (actual_topology_) delete actual_topology_;
}

switch_id
simple_topology::node_to_ejection_switch(node_id nodeaddr, int &switch_port) const
{
  return endpoint_to_injection_switch(nodeaddr, switch_port);
}

simple_topology::simple_topology(sprockit::sim_parameters* params) :
  topology(params,
          InitMaxPortsIntra::I_Remembered,
          InitGeomEjectID::I_Remembered)
{
  actual_topology_ = topology_factory::get_param("actual_name", params);
  num_nodes_ = actual_topology_->num_nodes();
  num_switches_ = params->get_int_param("nworkers");

  concentration_ = num_nodes_ / num_switches_;
  if (num_nodes_ % num_switches_) ++concentration_;

  max_ports_intra_network_ = num_switches_;
  eject_geometric_id_ = max_ports_intra_network_;

  endpoints_per_switch_ = num_nodes_ / num_switches_;
  num_switches_with_extra_node_ = num_nodes_ % num_switches_;
}

cartesian_topology*
simple_topology::cart_topology() const
{
  return safe_cast(cartesian_topology, const_cast<simple_topology*>(this));
}

int
simple_topology::minimal_distance(switch_id src, switch_id dst) const
{
  return actual_topology_->minimal_distance(src, dst);
}

void
simple_topology::minimal_route_to_switch(
  switch_id src,
  switch_id dst,
  structured_routable::path& path) const
{
  spkt_throw(sprockit::unimplemented_error,
    "simple_topology::minimal_route_to_coords: should never be called");
}

void
simple_topology::nodes_connected_to_injection_switch(switch_id swaddr,
                                           std::vector<node_id>& nodes) const
{
  if (swaddr >= num_switches_){
    spkt_throw_printf(sprockit::value_error,
        "simple_topology::nodes_connected_to_switch: requesting invalid switch %d",
        int(swaddr));
  }
  int addr_start, addr_end;
  if (swaddr >= num_switches_with_extra_node_){
    int offset_addr = swaddr - num_switches_with_extra_node_;
    int extras_offset = num_switches_with_extra_node_ * (endpoints_per_switch_ + 1);
    addr_start = extras_offset + offset_addr * endpoints_per_switch_;
    addr_end = addr_start + endpoints_per_switch_;
  }
  else {
    addr_start = (endpoints_per_switch_ + 1) * swaddr;
    addr_end = addr_start + (endpoints_per_switch_ + 1);
  }

  int addr_range = addr_end - addr_start;
  nodes.resize(addr_range);
  for (int i=0; i < addr_range; ++i){
    node_id nid(i + addr_start);
    nodes[i] = nid;
  }
}

void
simple_topology::nodes_connected_to_ejection_switch(switch_id swaddr,
                                                    std::vector<node_id>& nodes) const
{
  nodes_connected_to_injection_switch(swaddr, nodes);
}

void
simple_topology::connect_objects(sprockit::sim_parameters* params,
                                 internal_connectable_map& objects)
{
  top_debug("simple topology: connecting %d switches", int(objects.size()));
  for (auto& src : objects) {
    for (auto& dst : objects) {
      if (src.first != dst.first){
        src.second->connect_output(
          params,
          network,
          network,
          dst.second);
      }
    }
  }
}

}
} //end of namespace sstmac


