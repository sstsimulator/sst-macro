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
  to_string() const override {
    return "hdtorus";
  }

  virtual ~hdtorus() {}

  int
  diameter() const override {
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
  num_switches() const override {
    return num_switches_;
  }

  bool
  uniform_network_ports() const override {
    return false;
  }

  bool
  uniform_switches() const override {
    return true;
  }

  bool
  uniform_switches_non_uniform_network_ports() const override {
    return true;
  }

  void
  connected_outports(switch_id src, std::vector<connection>& conns) const override;

  void
  configure_individual_port_params(switch_id src,
            sprockit::sim_parameters *switch_params) const override;

  virtual int
  num_leaf_switches() const override {
    return num_switches();
  }

  void
  minimal_route_to_switch(
    switch_id sid,
    switch_id dst,
    routable::path& path) const override;

  int
  minimal_distance(
    switch_id sid,
    switch_id dst) const override;

  void
  configure_vc_routing(std::map<routing::algorithm_t, int> &m) const override;

  coordinates
  switch_coords(switch_id) const override;

  switch_id
  switch_addr(const coordinates &coords) const override;


 protected:
  inline int
  convert_to_port(int dim, int dir) const {
    return 2*dim + dir;
  }

 private:
  void
  torus_path(bool reset_dim, bool wrapped, int dim, int dir,
             routable::path& path) const;

  void
  down_path(
    int dim, int src, int dst,
    routable::path& path) const;

  void
  up_path(
    int dim, int src, int dst,
    routable::path& path) const;

  int
  shortest_distance(int dim, int src, int dst) const;

  bool
  shortest_path_positive(
    int dim, int src, int dst) const;

 protected: //must be visible to hypercube
  int diameter_;
  long num_switches_;

};

}
} //end of namespace sstmac

#endif

