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
  connect_objects(sprockit::sim_parameters* params,
                  internal_connectable_map& objects) = 0;

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
          dest_addr / num_nodes_per_endpoint_,
          sw_addr, paths);
  }

  virtual switch_id
  endpoint_to_injection_switch(node_id nodeaddr, int &switch_port) const {
    switch_id sid = nodeaddr / endpoints_per_switch_;
    switch_port = nodeaddr % endpoints_per_switch_ + max_ports_intra_network_;
    return sid;
  }

  switch_id
  node_to_ejection_switch(node_id addr, int& port) const {
    node_id netid(addr / num_nodes_per_endpoint_);
    return endpoint_to_ejection_switch(netid, port);
  }

  virtual switch_id
  endpoint_to_ejection_switch(node_id nodeaddr, int &switch_port) const {
    return endpoint_to_injection_switch(nodeaddr, switch_port);
  }

 protected:
  void
  configure_injection_geometry(std::vector<int>& redundancies);

  structured_topology(sprockit::sim_parameters* params,
                      InitMaxPortsIntra i1,
                      InitGeomEjectID i2);


};

}
}

#endif

