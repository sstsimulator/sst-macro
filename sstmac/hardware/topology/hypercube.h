
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


#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_HYPERCUBE_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_HYPERCUBE_H_INCLUDED

#include <sstmac/hardware/topology/hdtorus.h>

namespace sstmac {
namespace hw {

class hypercube :
  public hdtorus
{
 public:
  virtual std::string
  to_string() const {
    return "hdtorus topology";
  }

  virtual ~hypercube() {}

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  productive_path(
    int dim,
    const coordinates& src,
    const coordinates& dst,
    geometry_routable::path& path) const;

  std::string
  name() const;

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

 protected:
  int radix_;
  int ndim_;
  std::vector<int> dim_to_outport_;

};

}
} //end of namespace sstmac

#endif // HYPERCUBE_H

