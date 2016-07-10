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


#include <sstmac/hardware/analytic/simple/simple_topology.h>
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
    switch_port = nodeaddr % endpoints_per_switch_;
    return switch_id(id);
  }
  else {
    int id = nodeaddr / (endpoints_per_switch_+1);
    switch_port = nodeaddr % (endpoints_per_switch_+1);
    return switch_id(id);
  }
}

switch_id
simple_topology::endpoint_to_ejection_switch(node_id nodeaddr, int &switch_port) const
{
  return endpoint_to_injection_switch(nodeaddr, switch_port);
}

coordinates
simple_topology::node_coords(node_id uid) const
{
  auto t = safe_cast(structured_topology, actual_topology_);
  return t->node_coords(uid);
}

void
simple_topology::init_factory_params(sprockit::sim_parameters* params)
{
  actual_topology_ = topology_factory::get_param("actual_name", params);
  num_nodes_ = actual_topology_->num_nodes();
  num_switches_ = params->get_int_param("nworkers");

  endpoints_per_switch_ = num_nodes_ / num_switches_;
  num_switches_with_extra_node_ = num_nodes_ % num_switches_;

  structured_topology::init_factory_params(params);
}

int
simple_topology::ndimensions() const
{
  structured_topology* stop = safe_cast(structured_topology, actual_topology_);
  return stop->ndimensions();
}

int
simple_topology::diameter() const
{
  structured_topology* stop = safe_cast(structured_topology, actual_topology_);
  return stop->diameter();
}

int
simple_topology::num_hops_to_node(node_id src, node_id dst) const
{
  return actual_topology_->num_hops_to_node(src, dst);
}

void
simple_topology::finalize_init()
{
}

int
simple_topology::convert_to_port(int dim, int dir) const
{
  spkt_throw(sprockit::unimplemented_error,
    "simple_topology::convert_to_port: should never be called");
}

void
simple_topology::compute_switch_coords(switch_id uid, coordinates& coords) const
{
  spkt_throw(sprockit::unimplemented_error,
    "simple_topology::compute_switch_coords: should never be called");
}

switch_id
simple_topology::switch_number(const coordinates& coords) const
{
  spkt_throw(sprockit::unimplemented_error,
    "simple_topology::switch_number: should never be called");
}

void
simple_topology::minimal_route_to_coords(
  const coordinates &src_coords,
  const coordinates &dest_coords,
  geometry_routable::path& path) const
{
  spkt_throw(sprockit::unimplemented_error,
    "simple_topology::minimal_route_to_coords: should never be called");
}

int
simple_topology::minimal_distance(
  const coordinates &src_coords,
  const coordinates &dest_coords) const
{
  spkt_throw(sprockit::unimplemented_error,
    "simple_topology::minimal_distance: should never be called");
}

std::vector<node_id>
simple_topology::nodes_connected_to_switch(switch_id swaddr) const
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
  std::vector<node_id> nids;
  nids.resize(addr_range);
  for (int i=0; i < addr_range; ++i){
    node_id nid(i + addr_start);
    nids[i] = nid;
  }

  return nids;
}

void
simple_topology::connect_objects(internal_connectable_map& objects)
{
  top_debug("simple topology: connecting %d switches", int(objects.size()));
  internal_connectable_map::iterator ait, aend = objects.end();
  int ignore_port = 0;
  connectable::config cfg;
  cfg.link_weight = 1.0;
  cfg.red = 1;
  for (ait =  objects.begin(); ait != aend; ++ait) {
    switch_id a(ait->first);
    internal_connectable_map::iterator bit, bend = objects.end();
    for (bit = objects.begin(); bit != bend; ++bit) {
      switch_id b(bit->first);
      if (a != b){
        objects[a]->connect(
        ignore_port, ignore_port,
        connectable::output,
        objects[b], &cfg);
      }
    }
  }

}

void
simple_topology::productive_path(
  int dim,
  const coordinates &src_coords,
  const coordinates &dst_coords,
  geometry_routable::path& path) const
{
  spkt_throw(sprockit::unimplemented_error,
    "simple_topology::productive_path: should never be called");
}


}
} //end of namespace sstmac


