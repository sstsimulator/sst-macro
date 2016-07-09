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

#ifndef SSTMAC_HARDWARE_NETWORK_SWITCHES_REGULAR_TOPOLOGY_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_SWITCHES_REGULAR_TOPOLOGY_H_INCLUDED

#include <sstmac/hardware/topology/topology.h>

namespace sstmac {
namespace hw {

/**
Encapsulates a topology like torus, fat tree, butterfly which
has a regular, well-defined structure.  Routing on these topologies
is regular enough to be done with simple math computations without
resorting to a large routing table.  This contrasts with unstructured
topologies like #top_from_file where there is no regular structure
and routing must be done via lookup table.

The major defining characteristic of a structured topology is
being able to define a coordinate system.  Each switch in the
topology has a unique number and that number can be mapped
to a unique set of coordinates.  A torus has an obvious
mapping of index to X,Y,Z coordinates.  A butterfly or fat tree
also has a well-defined coordinate system, but is slightly less intuitive.
@class structured_topology
*/
class structured_topology : public topology
{

 public:
  virtual ~structured_topology() {}

  /**** BEGIN PURE VIRTUAL INTERFACE *****/
  /**
    Structured topologies can be direct (torus) or indirect (fat tree).
    We therefore need to distinguish the total number of switches and
    the number of leaf switches - i.e. those directly connected to nodes.
    For direct topologies, num_switches and num_leaf_switches are the same.
    For indirect, num_leaf_switches < num_switches.
    @return The number of leaf switches directly connected to compute nodes
  */
  virtual int
  num_leaf_switches() const = 0;

  /**
   * @brief Given a set of connectables, connect them appropriately
   * @param objects The set of objects to connect
   * @param cloner   If in parallel mode, not all objects may exist.
   *                 The factory creates missing objects.
   * @throws value_error If invalid switchid is passed to cloner
   */
  virtual void
  connect_objects(internal_connectable_map& objects) = 0;

  virtual void
  build_internal_connectables(
    internal_connectable_map& connectables,
    sprockit::factory<connectable>* factory,
    partition *part,
    int my_rank,
    sprockit::sim_parameters *params,
    connectable* dummy);

  virtual void
  build_endpoint_connectables(
    end_point_connectable_map& connectables,
    sprockit::factory<connectable>* factory,
    partition *part,
    int my_rank,
    sprockit::sim_parameters *params);

  virtual void
  build_interface_connectables(
    int conc,
    end_point_connectable_map& connectables,
    sprockit::factory2<connectable>* nic_factory,
    partition *part,
    int my_rank,
    sprockit::sim_parameters* params,
    sprockit::factory_type* interconnect);

  /**
    Workhorse function for implementing #minimal_route_to_switch
    and #minimal_route_to_node.
    Given source/dest coordinates, find the minimal path.
    @param current_sw_addr The addr of the current switch
    @param dest_sw_addr The addr of the destination switch
    @param path [inout] A complete path descriptor to the destination switch
  */
  virtual void
  minimal_route_to_coords(
    const coordinates& src_coords,
    const coordinates& dest_coords,
    routable::path& path) const = 0;

  virtual void
  minimal_routes_to_coords(
    const coordinates& src_coords,
    const coordinates& dest_coords,
    routable::path& current_path,
    routable::path_set& paths) const {
    //by default, most things only have one path
    paths.resize(1);
    minimal_route_to_coords(src_coords, dest_coords, paths[0]);
  }

  /**
    The function accepts either source or node coordinates.
    This gives the minimal distance counting the number of hops between switches.
    If node coordinates are given, the last coordinate is just ignored.
    @param src_coords. The source coordinates. This can be either switch or node coordinates.
    @param dest_coords. The destination coordinates. This can be either switch or node coordinates.
    @return The number of hops to final destination
  */
  virtual int
  minimal_distance(
    const coordinates& src_coords,
    const coordinates& dest_coords) const = 0;

  /**
      The number of distinct 'dimensions'
      in the topology.  This can correspond directly to standard
      X,Y,Z dimensions or the the number of levels in a fat tree.
      The keyword topology_redundant vector should have this many
      entries.
  */
  virtual int
  ndimensions() const = 0;

  virtual int
  diameter() const = 0;

  virtual switch_id
  switch_number(const coordinates& coords) const = 0;

  /**
    @param dim The dimension you want to move in.
                You might need to traverse other dimensions FIRST
                before you make progress on this dimension.
    @param src The coordinates of the source switch
    @param dst The coordinates of the dest switch
    @param path [inout] The path configuration for making progress on dim
  */
  virtual void
  productive_path(
    int dim,
    const coordinates& src,
    const coordinates& dst,
    routable::path& path) const = 0;
  /**** END PURE VIRTUAL INTERFACE *****/

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  /**
   * Given a switch address, return number of nodes connected to it
   */
  virtual int
  endpoints_per_switch(switch_id addr) const {
    return endpoints_per_switch_;
  }

  virtual int
  num_nodes() const {
    return endpoints_per_switch_ * num_leaf_switches() * num_nodes_per_netlink_;
  }

  virtual int
  num_endpoints() const {
    return endpoints_per_switch_ * num_leaf_switches();
  }

  /**
    Compute coordinates (e.g. X,Y,Z for 3D torus)
    @param swid The unique index defining a switch location
    @return The unique coordinates of the switch
  */
  coordinates
  switch_coords(switch_id swid) const;

  /**
    Compute coordinates (e.g. X,Y,Z for 3D torus).
    In many cases, this will just be the switch coordinates
    if #nps_ = 1, one node per switch.  If there are multiple
    nodes per switch, this will usually be the coordinates of
    the connected injection switch PLUS an extra index
    describing if it is the 0th, 1st, 2nd node
    on that switch.
    e.g. nps = 1, inj_sw = <0,2,1>, node = <0,2,1>
    e.g. nps = 3, inj_sw = <0,2,1>, node = <0,2,1,1>
    @param nid The unique index defining a node location
    @return The unique coordinates of the node
  */
  virtual coordinates
  node_coords(node_id nid) const;

  coordinates
  endpoint_coords(node_id nid) const;

  virtual switch_id
  endpoint_to_ejection_switch(node_id nodeaddr, int& switch_port) const;

  virtual switch_id
  endpoint_to_injection_switch(node_id nodeaddr, int& switch_port) const;

  virtual int
  endpoint_to_ejection_port(node_id addr) const;

  virtual int
  endpoint_to_injection_port(node_id addr) const;

  virtual int
  endpoint_to_switch_port(node_id nid) const {
    return nid % endpoints_per_switch_;
  }

  void
  finalize_init();

  virtual void
  productive_paths(
    routable::path_set &paths,
    const coordinates &current,
    const coordinates &dst);

  std::string
  label(node_id nid) const;

  std::string
  label(switch_id sid) const;

  /**
    Implementation of topology::minimal_route_to_switch.
    For structured topologies, this basically just computes
    the coordinates of source, dest switch and calls
    #minimal_route_to_coords.
    @param current_sw_addr The addr of the current switch
    @param dest_sw_addr The addr of the destination switch
    @param path [inout] A complete path descriptor to the destination switch
  */
  virtual void
  minimal_route_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    routable::path& path
  ) const;

  virtual void
  minimal_routes_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    routable::path& current_path,
    routable::path_set& paths) const {
    paths.resize(1);
    minimal_route_to_switch(current_sw_addr, dest_sw_addr, paths[0]);
  }

  virtual void
  eject_paths_on_switch(
    node_id dest_addr,
    switch_id sw_addr,
    routable::path_set& paths) const;

  virtual int
  num_hops_to_node(node_id src, node_id dst) const;

  virtual std::vector<node_id>
  nodes_connected_to_injection_switch(switch_id swaddr) const {
    return nodes_connected_to_switch(swaddr);
  }

  virtual std::vector<node_id>
  nodes_connected_to_ejection_switch(switch_id swid) const {
    return nodes_connected_to_switch(swid);
  }

  virtual coordinates
  neighbor_at_port(switch_id sid, int port);

  virtual node_id
  node_addr(const coordinates& coords) const;

  virtual node_id
  node_addr(const coordinates& sw_coords, int port) const;

  virtual std::string
  default_router() const {
    return "minimal";
  }

  virtual void
  send_partners(
    traffic_pattern::type_t ty,
    node_id src_node,
    std::vector<node_id>& partners) const;

  virtual void
  recv_partners(
    traffic_pattern::type_t ty,
    node_id src_node,
    std::vector<node_id>& partners) const;

 protected:
  void
  configure_injection_geometry(std::vector<int>& redundancies);

  virtual std::vector<node_id>
  nodes_connected_to_switch(switch_id swaddr) const;

  structured_topology();

  /**
    Compute coordinates (e.g. X,Y,Z for 3D torus)
    @param swid The unique index defining a switch location
    @param coords [inout] The unique coordinates of the switch
  */
  virtual void
  compute_switch_coords(switch_id swid, coordinates& coords) const = 0;

  void
  partners(
    bool get_send_partner,
    traffic_pattern::type_t ty,
    node_id src,
    std::vector<node_id>& partner_list) const;

  virtual void
  nearest_neighbor_partners(
    const coordinates& src_sw_coords,
    int port,
    std::vector<node_id>& partners) const;

  virtual void
  tornado_recv_partners(
    const coordinates& src_sw_coords,
    int port,
    std::vector<node_id>& partners) const;

  virtual void
  tornado_send_partners(
    const coordinates& src_sw_coords,
    int port,
    std::vector<node_id>& partners) const;

  virtual void
  bit_complement_partners(
    const coordinates& src_sw_coords,
    int port,
    std::vector<node_id>& partners) const;

 protected:
  int eject_geometric_id_;
  int injection_redundancy_;
  bool outputgraph_;

};
}
}

#endif

