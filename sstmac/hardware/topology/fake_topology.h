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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_FAKE_TOPOLOGY_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_FAKE_TOPOLOGY_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

class fake_topology :
  public structured_topology
{
 public:
  virtual std::string
  to_string() const override {
    return "fake topology";
  }

  virtual ~fake_topology() {}

  switch_id
  switch_number(const coordinates& coords) const {
    return switch_id();
  }

  virtual void
  productive_path(
    int dim,
    const coordinates& src,
    const coordinates& dst,
    routable::path& path) const {
  }

  int
  num_hops(node_id src, node_id dst) const {
    return 1;
  }

  void
  connect_objects(internal_connectable_map& objects) {
    //do nothing
  }

  void
  minimal_route_to_coords(
    const coordinates &current_coords,
    const coordinates &dest_coords,
    routable::path& path) const {
    //do nothing
  }

  int
  minimal_distance(
    const coordinates &current_coords,
    const coordinates &dest_coords) const {
    return 0;
  }

  virtual int
  diameter() const {
    spkt_throw(sprockit::unimplemented_error,
              "switchinterconnect::fake_topology::diameter: don't call this");
  }

  switch_id
  switch_number(const std::vector<int>& coords) const {
    return switch_id();
  }

  node_id
  node_addr(const std::vector<int>& coords) const {
    return node_id();
  }

  int
  num_switches() const {
    return 0;
  }

  int
  num_leaf_switches() const {
    return 0;
  }

  int
  convert_to_port(int dim, int dir) const {
    return 0;
  }

  void
  convert_to_dimdir(int outport, int &dim, int&dir) const {
    dim = 0;
    dir = 0;
  }


 protected:
  void
  compute_switch_coords(switch_id swid, coordinates &coords) const {
    //do nothing
  }


};
}
}

#endif

