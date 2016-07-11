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

// crossbar.h: Interface for crossbar networks.
//
// Author: Curtis Janssen <cljanss@ca.sandia.gov>
#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_CROSSBAR_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_CROSSBAR_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

/**
 *  @class crossbar
 *  The crossbar network generates a network which connects
    all nodes with only two hops: those to and from the crossbar.
 */
class crossbar : public structured_topology
{
 public:
  virtual std::string
  to_string() const {
    return "crossbar topology";
  }

  virtual ~crossbar() {}

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  int
  diameter() const {
    return 1;
  }

  std::string
  name() const;

  int
  ndimensions() const {
    return 1;
  }

  int
  ncoords() const {
    return 1;
  }

  int
  num_leaf_switches() const {
    return size_;
  }

  virtual void
  connect_objects(internal_connectable_map& switches);

  void
  configure_vc_routing(std::map<routing::algorithm_t, int> &m) const;

  void
  minimal_route_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    geometry_routable::path& path) const;

  void
  minimal_route_to_coords(
    const coordinates &src_coords,
    const coordinates &dest_coords,
    geometry_routable::path& path) const;

  int
  minimal_distance(
    const coordinates& src_coords,
    const coordinates& dest_coords
  ) const;

  virtual void
  productive_path(
    int dim,
    const coordinates& src,
    const coordinates& dst,
    geometry_routable::path& path) const;

  virtual int
  convert_to_port(int dim, int dir) const;

  switch_id
  switch_number(const coordinates& coords) const {
    return switch_id(coords[0]);
  }

  virtual int
  num_switches() const {
    return size_;
  }

 protected:
  virtual void
  compute_switch_coords(switch_id uid, coordinates& coords) const;

 private:
  long size_;

};

}
} //end of namespace sstmac

#endif

