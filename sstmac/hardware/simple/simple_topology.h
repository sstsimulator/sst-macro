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

class simple_topology : public topology
{

 public:
  typedef enum {
    injection,
    network
  } port;

  virtual std::string
  to_string() const override {
    return "simple topology";
  }

  virtual ~simple_topology();

  simple_topology(sprockit::sim_parameters* params);

  cartesian_topology*
  cart_topology() const override;

  int
  num_leaf_switches() const override {
    return num_switches_;
  }

  int
  num_switches() const override {
    return num_switches_;
  }


  void
  connect_objects(sprockit::sim_parameters* params,
                  internal_connectable_map& objects) override;

  int
  minimal_distance(switch_id src, switch_id dst) const override;

  void
  minimal_route_to_switch(
    switch_id src,
    switch_id dst,
    structured_routable::path& path) const override;

  void
  configure_vc_routing(std::map<routing::algorithm_t, int> &m) const override {
    topology::configure_vc_routing(m);
  }

  virtual void
  nodes_connected_to_injection_switch(switch_id swaddr,
                                      std::vector<node_id>& nodes) const override;

  virtual void
  nodes_connected_to_ejection_switch(switch_id swaddr,
                                      std::vector<node_id>& nodes) const override;

  switch_id
  endpoint_to_ejection_switch(node_id nodeaddr, int &switch_port) const override;

  switch_id
  endpoint_to_injection_switch(node_id nodeaddr, int &switch_port) const override;

  switch_id
  node_to_ejection_switch(node_id addr, int &port) const override;

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
