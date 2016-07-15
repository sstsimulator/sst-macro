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

// flattenedbutterfly.h: Interface for torus networks.
//
// Author: Ali Pinar <apinar@sandia.gov>

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_flattenedbutterfly_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_flattenedbutterfly_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/topology/butterfly.h>

namespace sstmac {
namespace hw {

/**
    See High Performance Datacenter Networks by
    Dennis Abts
    John Kim
    For figures/detailed explanation
*/
class flattened_butterfly :
  public abstract_butterfly
{

 public:
  virtual std::string
  to_string() const {
    return "flattened butterfly topology";
  }

  virtual ~flattened_butterfly() {}

  void
  init_factory_params(sprockit::sim_parameters *params);

  int
  num_switches() const {
    return nswitches_per_col_;
  }

  int
  ndimensions() const {
    return nfly_ - 1;
  }

  void
  minimal_route_to_coords(
    const coordinates &src_coords,
    const coordinates &dest_coords,
    geometry_routable::path& path) const;

  int
  minimal_distance(const coordinates &src_coords,
                   const coordinates &dest_coords) const;

  virtual void
  connect_objects(internal_connectable_map& switches);

  virtual int
  convert_to_port(int dim, int dir) const;

  switch_id
  switch_number(const coordinates &coords) const;

  virtual void
  productive_path(
    int dim,
    const coordinates& src,
    const coordinates& dst,
    geometry_routable::path& path) const;

 protected:
  virtual void
  compute_switch_coords(switch_id uid, coordinates& coords) const;

};

}
} //end of namespace sstmac

#endif

