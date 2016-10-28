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
  enum class InitMaxPortsIntra {
    I_Remembered
  };
  enum class InitGeomEjectID {
    I_Remembered
  };

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
  num_leaf_switches() const override = 0;

  int
  max_num_ports() const override {
    return max_ports_injection_ + max_ports_intra_network_;
  }

  bool
  node_to_netlink(node_id nid, node_id& net_id, int& offset) const override {
    net_id = nid / num_nodes_per_netlink_;
    offset = nid % num_nodes_per_netlink_;
    return num_nodes_per_netlink_ > 1;
  }

  virtual void
  nodes_connected_to_injection_switch(switch_id swid,
                                      std::vector<injection_port>& nodes) const override;

  virtual void
  nodes_connected_to_ejection_switch(switch_id swid,
                                     std::vector<injection_port>& nodes) const override;

  /**
   * Given a switch address, return number of nodes connected to it
   */
  int
  netlinks_per_switch() {
    return netlinks_per_switch_;
  }

  int
  concentration() const {
    return concentration_;
  }

  int
  num_nodes() const override {
    return concentration_ * num_leaf_switches();
  }

  int
  num_netlinks() const override {
    return netlinks_per_switch_ * num_leaf_switches();
  }

  int
  num_hops_to_node(node_id src, node_id dst) const override {
    switch_id src_sw = src / concentration_;
    switch_id dst_sw = dst / concentration_;
    return minimal_distance(src_sw, dst_sw);
  }

  switch_id
  max_switch_id() const override {
    return num_switches();
  }

  bool
  switch_id_slot_filled(switch_id sid) const override{
    return true;
  }

  netlink_id
  max_netlink_id() const override {
    if (num_nodes_per_netlink_ > 1){
      return num_nodes() / num_nodes_per_netlink_;
    } else {
      return 0;
    }
  }

  bool
  netlink_id_slot_filled(netlink_id sid) const override{
    return true;
  }

  node_id
  max_node_id() const override {
    return num_nodes();
  }

  bool
  node_id_slot_filled(node_id nid) const override{
    return true;
  }

  virtual int
  diameter() const = 0;

  /**** END PURE VIRTUAL INTERFACE *****/


  virtual void
  endpoint_eject_paths_on_switch(
    node_id dest_addr,
    switch_id sw_addr,
    routable::path_set& paths) const;

  void
  node_eject_paths_on_switch(
      node_id dest_addr,
      switch_id sw_addr,
      routable::path_set& paths) const {
    endpoint_eject_paths_on_switch(
          dest_addr / num_nodes_per_netlink_,
          sw_addr, paths);
  }

  virtual switch_id
  netlink_to_injection_switch(netlink_id nodeaddr, int &switch_port) const override {
    switch_id sid = nodeaddr / netlinks_per_switch_;
    switch_port = nodeaddr % netlinks_per_switch_ + max_ports_intra_network_;
    return sid;
  }

  virtual switch_id
  netlink_to_ejection_switch(node_id nodeaddr, int &switch_port) const override {
    return netlink_to_injection_switch(nodeaddr, switch_port);
  }

  switch_id
  node_to_ejection_switch(node_id addr, int& port) const override {
    node_id netid(addr / num_nodes_per_netlink_);
    return netlink_to_ejection_switch(netid, port);
  }

  switch_id
  node_to_injection_switch(node_id addr, int& port) const override {
    node_id netid(addr / num_nodes_per_netlink_);
    return netlink_to_injection_switch(netid, port);
  }

 protected:
  void
  configure_injection_geometry(std::vector<int>& redundancies);

  structured_topology(sprockit::sim_parameters* params,
                      InitMaxPortsIntra i1,
                      InitGeomEjectID i2);

 protected:
  /**
    Nodes per switch.  The number of nodes connected to a leaf switch.
    In many topologies, there is a 1-1 correspondence. For others,
    you might have many compute nodes connected to a single injection/ejection switch.
  */
  int netlinks_per_switch_;

  int concentration_;

  int num_nodes_per_netlink_;

  int max_ports_intra_network_;

  int max_ports_injection_;

  int eject_geometric_id_;

  int injection_redundancy_;
};

}
}

#endif

