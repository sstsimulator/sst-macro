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
  typedef enum {
    pos = 0,
    neg = 1
  } direction_t;

  virtual std::string
  to_string() const {
    return "hdtorus";
  }

  virtual ~hdtorus() {}

  void
  init_factory_params(sprockit::sim_parameters* params);

  int
  shortest_distance(int dim, int src, int dst) const;

  int
  distance(int dim, int dir, int src, int dst) const;

  void
  down_path(
    int dim,
    const coordinates& src,
    const coordinates& dst,
    geometry_routable::path& path) const;

  void
  up_path(
    int dim,
    const coordinates& src,
    const coordinates& dst,
    geometry_routable::path& path) const;

  bool
  shortest_path_positive(
    int dim,
    const coordinates& src,
    const coordinates& dst) const;

  virtual void
  productive_path(
    int dim,
    const coordinates& src,
    const coordinates& dst,
    geometry_routable::path& path) const;

  void
  pick_vc(geometry_routable::path& path) const;

  switch_id
  switch_number(const coordinates& v) const;

  int
  diameter() const {
    return diameter_;
  }

  std::string
  name() const;

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

  virtual int
  ndimensions() const {
    return dimensions_.size();
  }

  void
  minimal_route_to_coords(
    const coordinates &src_coords,
    const coordinates &dest_coords,
    geometry_routable::path& path) const;

  virtual void
  connect_objects(internal_connectable_map& switches);

  virtual int
  convert_to_port(int dim, int dir) const;

  int
  minimal_distance(
    const coordinates& src_coords,
    const coordinates& dest_coords) const;

  void
  partners(
    traffic_pattern::type_t ty,
    const coordinates &src_sw_coords,
    std::list<node_id>& partners) const;

  void
  configure_vc_routing(std::map<routing::algorithm_t, int> &m) const;

 protected:
  virtual void
  compute_switch_coords(switch_id uid, coordinates& coords) const;

  void
  nearest_neighbor_partners(
    const coordinates& src_sw_coords,
    int port,
    std::vector<node_id>& partners) const;

  void
  bit_complement_partners(
    const coordinates &src_sw_coords,
    int port,
    std::vector<node_id>& partners) const;

  void
  tornado_send_partners(
    const coordinates &src_sw_coords,
    int port,
    std::vector<node_id>& partners) const;

  void
  tornado_recv_partners(
    const coordinates &src_sw_coords,
    int port,
    std::vector<node_id>& partners) const;

 protected:
  virtual void
  connect_dim(int dim,
    connectable* center,
    connectable* plus,
    connectable* minus);

 protected:
  int diameter_;
  long num_switches_;
  std::vector<int> dimensions_;

};

}
} //end of namespace sstmac

#endif

