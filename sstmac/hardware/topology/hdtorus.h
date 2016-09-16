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

#include <sstmac/hardware/topology/cartesian_topology.h>

namespace sstmac {
namespace hw {

/**
 * @class hdtorus
 * Implements a high dimensional torus network.
 */

class hdtorus :
  public cartesian_topology
{
 public:
  hdtorus(sprockit::sim_parameters* params);

  typedef enum {
    pos = 0,
    neg = 1
  } direction_t;

  virtual std::string
  to_string() const {
    return "hdtorus";
  }

  virtual ~hdtorus() {}

  int
  diameter() const {
    return diameter_;
  }

  /// Returns the vector giving each dimension of the torus.
  const std::vector<int> &
  dimensions() const {
    return dimensions_;
  }

  coordinates
  neighbor_at_port(switch_id sid, int port);

  virtual int
  num_switches() const {
    return num_switches_;
  }

  virtual int
  num_leaf_switches() const {
    return num_switches();
  }

  void
  configure_geometric_paths(std::vector<int> &redundancies);

  void
  minimal_route_to_switch(
    switch_id sid,
    switch_id dst,
    structured_routable::path& path) const;

  virtual void
  connect_objects(sprockit::sim_parameters* params,
                  internal_connectable_map& switches);

  int
  minimal_distance(
    switch_id sid,
    switch_id dst) const;

  void
  configure_vc_routing(std::map<routing::algorithm_t, int> &m) const;

  coordinates
  switch_coords(switch_id) const;

  switch_id
  switch_addr(const coordinates &coords) const;

 private:
  inline int
  convert_to_port(int dim, int dir) const {
    return 2*dim + dir;
  }

  void
  torus_path(bool reset_dim, bool wrapped, int dim, int dir,
             structured_routable::path& path) const;

  void
  down_path(
    int dim, int src, int dst,
    structured_routable::path& path) const;

  void
  up_path(
    int dim, int src, int dst,
    structured_routable::path& path) const;

  int
  shortest_distance(int dim, int src, int dst) const;

  bool
  shortest_path_positive(
    int dim, int src, int dst) const;

 private:
  virtual void
  connect_dim(
    sprockit::sim_parameters* params,
    int dim,
    connectable* center,
    connectable* plus,
    connectable* minus);

 protected: //must be visible to hypercube
  int diameter_;
  long num_switches_;

};

}
} //end of namespace sstmac

#endif

