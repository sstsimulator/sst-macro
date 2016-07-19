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

// hypercube.cc: Implementation of a high dimensional torus network.
//
// Author: Ali Pinar <apinar@sandia.gov>
//

#include <sstmac/hardware/topology/hypercube.h>
#include <stdio.h>
#include <sstream>
#include <sprockit/stl_string.h>

namespace sstmac {
namespace hw {

SpktRegister("hypercube", topology, hypercube,
            "hypercube implements a high-dimension torus with an arbitrary number of dimensions");

void
hypercube::init_factory_params(sprockit::sim_parameters* params)
{
  hdtorus::init_factory_params(params);
  ndim_ = dimensions_.size();
  dim_to_outport_.resize(ndim_);
  int offset = 0;
  for (int i=0; i < ndim_; ++i) {
    dim_to_outport_[i] = offset;
    offset += dimensions_[i];
  }
  max_ports_intra_network_ = offset;
  eject_geometric_id_ = max_ports_intra_network_;
}

/// Returns a descriptive name of the graph.
std::string
hypercube::name() const
{
  int64_t dsize = dimensions_.size();
  std::ostringstream ostr;
  ostr << "hypercube(";
  for (int i = 0; i < dsize; i++) {
    ostr << dimensions_[i] << ",";
  }
  ostr << ")";
  return ostr.str();
}

void
hypercube::minimal_route_to_coords(
  const coordinates &src_coords,
  const coordinates &dest_coords,
  geometry_routable::path& path) const
{
  for (int i=0; i < src_coords.size(); ++i) {
    if (src_coords[i] != dest_coords[i]) {
      int dim = i;
      int dir = dest_coords[i];
      path.vc = 0;
      path.outport = convert_to_port(dim, dir);
      return;
    }
  }
}

int
hypercube::minimal_distance(
  const coordinates &src_coords,
  const coordinates &dest_coords) const
{
  int dist = 0;
  for (int i=0; i < src_coords.size(); ++i) {
    if (src_coords[i], dest_coords[i]) {
      ++dist;
    }
  }
  return dist;
}

void
hypercube::connect_objects(internal_connectable_map& objects)
{
  connectable::config cfg;
  cfg.ty = connectable::RedundantConnection;
  top_debug("hypercube: connecting %d switches",
    int(objects.size()));

  internal_connectable_map::iterator it;
  for (it = objects.begin(); it != objects.end(); ++it) {
    switch_id me(it->first);
    coordinates coords = switch_coords(me);

    //loop every dimension and connect to all "neighbors"
    for (int dim=0; dim < dimensions_.size(); ++dim) {
      coordinates neighbor_coords = coords;
      switch_id my_id = switch_number(coords);
      int dimsize = dimensions_[dim];
      int my_idx = coords[dim];
      cfg.red = red_[dim];
      int inport = convert_to_port(dim, my_idx);
      for (int dir=0; dir < dimsize; ++dir) {
        if (dir != my_idx) {
          neighbor_coords[dim] = dir;
          switch_id neighbor_id = switch_number(neighbor_coords);
          connectable* neighbor_sw = objects[neighbor_id];
          top_debug("hypercube connecting %s(%d) to %s(%d) at dim=%d,dir=%d",
              stl_string(coords).c_str(), int(my_id),
              stl_string(neighbor_coords).c_str(), int(neighbor_id),
              dim, dir);

          int outport = convert_to_port(dim, dir);

          objects[me]->connect(
            outport,
            inport,
            connectable::output,
            neighbor_sw, &cfg);

          neighbor_sw->connect(
            outport,
            inport,
            connectable::input,
            objects[me], &cfg);

        }
      }
    }
  }
}

void
hypercube::productive_path(
  int dim,
  const coordinates &src,
  const coordinates &dst,
  geometry_routable::path& path) const
{
  path.outport = dim_to_outport_[dim] + dst[dim];
  path.vc = 0;
}

int
hypercube::convert_to_port(int dim, int dir) const
{
  return dim_to_outport_[dim] + dir;
}

}
} //end of namespace sstmac




