#ifndef SIMPLE_TOPOLOGY_H
#define SIMPLE_TOPOLOGY_H

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

// hdtorus.h: Interface for torus networks.
//
// Author: Jeremiah Wilke <jjwilke@sandia.gov>

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_HDTORUS_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_HDTORUS_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

/**
 * @class hdtorus
 * Implements a high dimensional torus network.
 */

class simple_topology :
  public structured_topology
{

 public:
  virtual std::string
  to_string() const {
    return "simple topology";
  }

  virtual ~simple_topology();

  simple_topology() :
    actual_topology_(nullptr)
  {
  }

  void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  finalize_init();

  int
  num_leaf_switches() const {
    return num_switches_;
  }

  int
  num_switches() const {
    return num_switches_;
  }

  int
  num_endpoints() const {
    return num_nodes_;
  }

  int
  num_nodes() const {
    return num_nodes_;
  }

  void
  compute_switch_coords(switch_id swid, coordinates &coords) const;

  int
  convert_to_port(int dim, int dir) const;

  void
  connect_objects(internal_connectable_map& objects);

  coordinates
  node_coords(node_id uid) const;

  void
  minimal_route_to_coords(
    const coordinates& src_coords,
    const coordinates& dest_coords,
    structured_routable::path& path) const;

  int
  minimal_distance(
    const coordinates& src_coords,
    const coordinates& dest_coords) const;

  virtual int
  num_hops_to_node(node_id src, node_id dst) const;

  virtual int
  ndimensions() const;

  virtual int
  diameter() const;

  virtual int
  radix() const {
    return num_switches_ - 1;
  }

  void
  configure_vc_routing(std::map<routing::algorithm_t, int> &m) const {
    topology::configure_vc_routing(m);
  }

  switch_id
  switch_number(const coordinates& coords) const;

  virtual std::vector<node_id>
  nodes_connected_to_switch(switch_id swaddr) const;

  virtual switch_id
  endpoint_to_ejection_switch(node_id nodeaddr, int &switch_port) const;

  virtual switch_id
  endpoint_to_injection_switch(node_id nodeaddr, int &switch_port) const;

  virtual void
  productive_path(
    int dim,
    const coordinates& src,
    const coordinates& dst,
    structured_routable::path& path) const;

 protected:
  int num_switches_;
  int num_nodes_;
  int num_switches_with_extra_node_;
  hw::topology* actual_topology_;

};

}
} //end of namespace sstmac

#endif



#endif // SIMPLE_TOPOLOGY_H
