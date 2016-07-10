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
// crossbar.cc: Implementation of crossbar networks.
//
// Author: Curtis Janssen <cljanss@sandia.gov>

#include <sstream>
#include <sstmac/hardware/topology/crossbar.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

SpktRegister("crossbar | xbar", topology, crossbar);

void
crossbar::init_factory_params(sprockit::sim_parameters* params)
{
  std::vector<int> args;
  params->get_vector_param("geometry", args);
  size_ = args[0];
  max_ports_injection_ = endpoints_per_switch_ = params->get_optional_int_param("concentration", 1);
  max_ports_intra_network_ = num_switches();
  eject_geometric_id_ = max_ports_intra_network_;
  structured_topology::init_factory_params(params);
}

void
crossbar::configure_vc_routing(std::map<routing::algorithm_t, int> &m) const
{
  m[routing::minimal] = 1;
  m[routing::minimal_adaptive] = 1;
  m[routing::valiant] = 2;
  m[routing::ugal] = 3;
}

void
crossbar::compute_switch_coords(switch_id uid, coordinates &coords) const
{
  coords[0] = uid;
}

void
crossbar::productive_path(
  int dim,
  const coordinates &src,
  const coordinates &dst,
  geometry_routable::path& path) const
{
#if SSTMAC_SANITY_CHECK
  if (dim == topology::eject) {
    spkt_throw(sprockit::value_error,
              "crossbar::get_productive_path: not compatible with eject dimension");
  }
  else if (dim != 0) {
    spkt_throw_printf(sprockit::value_error,
                     "crossbar::get_productive_path: received non-zero dimension %d", dim);
  }
#endif
  path.vc = 0;
  path.outport = dst[0];
}

int
crossbar::convert_to_port(int dim, int dir) const
{
  return dir;
}

std::string
crossbar::name() const
{
  std::ostringstream ostr;
  ostr << "crossbar(" << num_switches() << ")";
  return ostr.str();
}

void
crossbar::minimal_route_to_coords(
  const coordinates &src_coords,
  const coordinates &dest_coords,
  geometry_routable::path& path) const
{
  path.vc = 0;
  path.outport = dest_coords[0];
}

void
crossbar::minimal_route_to_switch(
  switch_id current_sw_addr,
  switch_id dest_sw_addr,
  geometry_routable::path& path) const
{
  //current switch actually doesn't matter
  path.vc = 0;
  path.outport = dest_sw_addr;
}

int
crossbar::minimal_distance(const coordinates &src_coords,
                           const coordinates &dest_coords) const
{
  return 1;
}

void
crossbar::connect_objects(internal_connectable_map& objects)
{
  connectable::config cfg;
  cfg.ty = connectable::BasicConnection;
  for (int i = 0; i < objects.size(); i++) {
    switch_id me(i);

    for (int j = 0; j < objects.size(); j++) {
      switch_id them(j);
      if (i != j) {
        int outport = convert_to_port(0, j);
        int inport = convert_to_port(0, i);

        objects[me]->connect(
          outport,
          inport,
          connectable::output,
          objects[them], &cfg);

        objects[them]->connect(
          outport, inport,
          connectable::input,
          objects[me], &cfg);
      }
    }
  }
}

}
} //end of namespace sstmac

