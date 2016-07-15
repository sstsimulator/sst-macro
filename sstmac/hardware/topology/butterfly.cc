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
// butterfly.cc: Implementation of butterfly networks.
//
// Based on butterfly.c by Jim Schutt
// Adapted by Curtis Janssen <cljanss@ca.sandia.gov>

#include <sstmac/hardware/topology/butterfly.h>
#include <sprockit/sim_parameters.h>

#include <math.h>

namespace sstmac {
namespace hw {

SpktRegister("butterfly | bfly", topology, butterfly);

void
abstract_butterfly::compute_switch_coords(switch_id uid,
    coordinates& coords) const
{
  long group_size = nswitches_per_col_;
  long next_group_size = group_size / kary_;
  for (int l=0; l < (nfly_ - 1); ++l) {
    //figure out the group offset
    long group_relative_row = uid % group_size;
    long group_number = group_relative_row / next_group_size;
    coords[l] = group_number;

    group_size = next_group_size;
    next_group_size /= kary_;
  }
}

void
abstract_butterfly::productive_path(
  int dim,
  const coordinates &src,
  const coordinates &dst,
  geometry_routable::path& path) const
{
  spkt_throw(sprockit::unimplemented_error,
    "butterfly::productive_path: should never be called");
}

switch_id
abstract_butterfly::switch_number(const coordinates& coords) const
{
  long index_multiplier = nswitches_per_col_ / kary_;
  long nid = 0;
  for (int l=0; l < (nfly_ - 1); ++l) {
    nid += coords[l] * index_multiplier;
    index_multiplier /= kary_;
  }
  return switch_id(nid);
}

void
abstract_butterfly::init_factory_params(sprockit::sim_parameters* params)
{
  /**
   sstkeyword {
   gui=4 3;
   docstring=Specify the geometry of the butterfly network.ENDL
   Should be a vector of size 2 for K-ary N-fly.ENDL
   First index, K, is the radix of the butterfly.ENDL
   Second index, N, is the number of stages.ENDL
   Network will have K^(N-1) leaf switches and K^N network endpoints.;
   }
   */
  std::vector<int> args;
  params->get_vector_param("geometry", args);
  kary_ = args[0];
  nfly_ = args[1];
  max_ports_injection_ = endpoints_per_switch_ = params->get_optional_int_param("concentration", kary_);
  //a 4-ary 3-fly has three levels of router
  //assume for simplicity nps = kary
  //we have 4^3 = 64 nodes
  //we need 4^(3-1) = 16 switches per level to support 64 nodes
  nswitches_per_col_ = pow(kary_, nfly_ - 1);
  structured_topology::init_factory_params(params);
}

void
abstract_butterfly::configure_vc_routing(std::map<routing::algorithm_t, int> &m) const
{
  m[routing::minimal] = 1;
  m[routing::minimal_adaptive] = 1;
  m[routing::valiant] = 2;
  m[routing::ugal] = 3;
}

void
butterfly::compute_switch_coords(switch_id uid, coordinates& coords) const
{
  abstract_butterfly::compute_switch_coords(uid, coords);

  //figure out which column
  int col = uid / nswitches_per_col_;
  coords[nfly_-1] = col;
}

void
butterfly::init_factory_params(sprockit::sim_parameters* params)
{
  abstract_butterfly::init_factory_params(params);
  last_col_index_start_ = nswitches_per_col_ * (nfly_ - 1);
  max_ports_intra_network_ = kary_;
  eject_geometric_id_ = max_ports_intra_network_;
}

void
butterfly::connect_objects(internal_connectable_map& objects)
{
  /**
    In 4-ary 3-fly, we have 16 switches per col
    with 3 columns or stages.  Thus we can label
    each switch by its row,col index
    Lets label switch 0 in each col A0, B0, C0...
    B0 = 16, B1 = 17 when using absolute indexing
    C0 = 32, C1 = 33 when using absolute indexing
    The first two cols connect in strides of 4
    A0 -> B0, B4, B8, B12 = 16, 20, 24, 28
    A1 -> B1, B5, B9, B13 = 17, 21, 25, 29
    ...
    A4 -> B0, B4, B8, B12
    etc
    The second and third cols connect in groups of 4
    with stride 1
    B0 -> C0, C1, C2, C3 = 32, 33, 34, 35
    B1 -> C0, C1, C2, C3
    B2 -> C0, C1, C2, C3
    B3 -> C0, C1, C2, C3
    B4 -> C4, C5, C6, C7 = 36, 37, 38, 39
    etc

    We distinguish between blocks and groups.
    A block is set of switches whose links "cross" and
    cannot be separated.  A group is a set of switches
    who connect to the same partners.
    On dimension 0, connecting A to B,
    we have one block of size 16 and 4 groups of size 4.
    On dimension1, connecting B to C,
    we now have four blocks of size 4 and still
    4 groups of size 4.
  */

  long connection_stride = nswitches_per_col_ / kary_;
  long block_size = nswitches_per_col_;
  long group_size = kary_;
  connectable::config cfg;
  cfg.ty = connectable::BasicConnection;
  for (int l=0; l < (nfly_-1); ++l) {
    long col_start_index = l * nswitches_per_col_;
    for (long i=0; i < nswitches_per_col_; ++i) {
      long my_switch_index = col_start_index + i;
      switch_id my_addr(my_switch_index);
      connectable* my_sw = objects[my_addr];

      long my_block = i / block_size;
      long my_intra_block_index = i % block_size;
      long my_block_index_start = my_block * block_size;

      long my_group_offset = my_intra_block_index % connection_stride;
      long my_group_index_start = my_block_index_start + my_group_offset;
      long my_index_in_my_group = (my_switch_index - my_group_index_start) / connection_stride;

      //make sure to include column offset
      long up_group_partner = my_group_index_start + col_start_index +
                              nswitches_per_col_;
      long num_connections = kary_;

      int inport = my_index_in_my_group;
      for (long c=0; c < num_connections;
           ++c, up_group_partner += connection_stride) {
        //printf("Connecting %ld:%ld->%ld\n", my_switch_index, c, up_group_partner);
        switch_id up_group_partner_addr(up_group_partner);
        connectable* partner_sw = objects[up_group_partner_addr];
        int outport = convert_to_port(up_dimension, c);
        my_sw->connect(
          outport,
          inport,
          connectable::output,
          partner_sw, &cfg);

        partner_sw->connect(
          outport,
          inport,
          connectable::input,
          my_sw, &cfg);
      }
    }

    //the next set of connections is more compact - lower the stride
    connection_stride /= kary_;
    block_size /= kary_;
  }

}

void
butterfly::minimal_route_to_coords(
  const coordinates &src_coords,
  const coordinates &dest_coords,
  geometry_routable::path& path) const
{
  //we have to route our current level
  int current_dim = src_coords[nfly_ - 1];
  //path.dim = up_dimension;
  //path.dir = dest_coords[current_dim];
  int dim = up_dimension;
  int dir = dest_coords[current_dim];
  path.outport = convert_to_port(dim, dir);
  path.vc = 0;
}

void
butterfly::productive_paths(
  geometry_routable::path_set &paths,
  const coordinates &current,
  const coordinates &dst)
{
  paths.resize(1);
  minimal_route_to_coords(current, dst, paths[0]);
}

int
butterfly::minimal_distance(const coordinates &src_coords,
                            const coordinates &dest_coords) const
{
  int eject_dim = nfly_ - 1;
  int lastidx = eject_dim;
  int current_dim = src_coords[lastidx];
  int dest_dim = dest_coords[lastidx];
  if (dest_dim != eject_dim) {
    spkt_throw_printf(sprockit::value_error,
                     "invalid butterfly destination coordinates: %s -> %s",
                      src_coords.to_string().c_str(),
                      dest_coords.to_string().c_str());
  }
  //this many hops remaining
  return (eject_dim - current_dim);
}

switch_id
butterfly::endpoint_to_ejection_switch(node_id addr, int& switch_port) const
{
  long node_idx = addr;
  //we inject on the first row - eject on the last row
  long ej_idx = node_idx / endpoints_per_switch_ + last_col_index_start_;
  switch_port = node_idx % endpoints_per_switch_;
  return switch_id(ej_idx);
}

switch_id
butterfly::switch_number(const coordinates &coords) const
{
  //get the row number
  long row_id = abstract_butterfly::switch_number(coords);
  long col_id = coords[nfly_-1];
  long nid = col_id * nswitches_per_col_ + row_id;
  return switch_id(nid);
}

std::vector<node_id>
butterfly::nodes_connected_to_ejection_switch(switch_id swaddr) const
{
  int last_row_offset = nswitches_per_col_ * (nfly_ - 1);
  if (swaddr >= last_row_offset) {
    switch_id sid_offset(swaddr - last_row_offset);
    return structured_topology::nodes_connected_to_switch(sid_offset);
  } else {
    return std::vector<node_id>();
  }
}

std::vector<node_id>
butterfly::nodes_connected_to_injection_switch(switch_id swaddr) const
{
  if (swaddr >= nswitches_per_col_) {
    return std::vector<node_id>();
  }
  else {
    return structured_topology::nodes_connected_to_switch(swaddr);
  }
}

int
butterfly::convert_to_port(int dim, int dir) const
{
  return dir;
}

}
} //end of namespace sstmac

