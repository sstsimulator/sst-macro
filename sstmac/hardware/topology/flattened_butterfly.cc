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
// flattenedbutterfly.cc: Implementation of a high dimensional torus network.
//
// Author: Ali Pinar <apinar@sandia.gov>
//
#include <sstmac/hardware/topology/flattened_butterfly.h>
#include <stdio.h>
#include <sstream>
#include <sprockit/factories/factory.h>

namespace sstmac {
namespace hw {

SpktRegister("fbfly | flattenedbfly | flattenedbutterfly", topology,
            flattened_butterfly);
void
flattened_butterfly::connect_objects(internal_connectable_map& objects)
{
  double link_weight = 1.0;
  int red = 1;
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

    In the flattenedy butterfly, all the routers in a given
    row are actually compressed into a single router.
    Additionally, all links are bidirectional.
    We do exactly the same connection as the butterfly.
    Because links are bidirectional, the lower-indexed
    router is responsible for setting up the link.
  */

  long connection_stride = nswitches_per_col_ / kary_;
  long group_size = kary_;
  long block_size = nswitches_per_col_;
  // Even though we only have one column, we still
  // go through the connection algorithm as if we had a full butterfly
  for (int l=0; l < (nfly_-1); ++l) {
    for (long i=0; i < nswitches_per_col_; ++i) {

      long my_switch_index = i;
      switch_id my_addr(my_switch_index);
      connectable* my_sw = objects[my_addr];

      long my_block = i / block_size;
      long my_intra_block_index = i % block_size;
      long my_block_index_start = my_block * block_size;

      long my_intra_group_index = my_intra_block_index / connection_stride;
      long my_group_offset = my_intra_block_index % connection_stride;
      long my_group_index_start = my_block_index_start + my_group_offset;

      //make sure to include column offset
      long up_group_partner = my_group_index_start;
      long num_connections = kary_;

      for (long c=0; c < num_connections;
           ++c, up_group_partner += connection_stride) {
        if (up_group_partner <= my_switch_index) {
          //the lower indexed switch is responsible for making the links
          //also, skip self links
          continue;
        }
        switch_id up_group_partner_addr(up_group_partner);
        connectable* partner_sw = objects[up_group_partner_addr];
        // we make k connections per level - index appropriately
        int offset = l * kary_ + c;
        //printf("Connecting %ld:%d->%ld\n", my_switch_index, port, up_group_partner);
        int up_port = convert_to_port(up_dimension, offset);
        offset = l * kary_ + my_intra_group_index;
        int down_port = convert_to_port(down_dimension, offset);

        my_sw->connect_weighted(
          up_port, //up is out, down is in - got it?
          down_port,
          connectable::output,
          partner_sw,
          link_weight, red);
        partner_sw->connect(
          up_port,
          down_port,
          connectable::input,
          my_sw);

        //printf("Connecting %ld:%d->%ld\n", up_group_partner, port, my_switch_index);
        partner_sw->connect_weighted(
          down_port,
          up_port,
          connectable::output,
          my_sw,
          link_weight, red);
        my_sw->connect(
          down_port,
          up_port,
          connectable::input,
          partner_sw);
      }
    }

    //the next set of connections is more compact - lower the stride
    connection_stride /= kary_;
    block_size /= kary_;
  }

}

int
flattened_butterfly::minimal_distance(const coordinates &current_coords,
                                      const coordinates &dest_coords) const
{
  int dist = 0;
  for (int i=0; i < current_coords.size(); ++i) {
    if (current_coords[i] != dest_coords[i]) {
      ++dist;
    }
  }
  return dist;
}

int
flattened_butterfly::convert_to_port(int dim, int dir) const
{
  int ndim = nfly_ - 1;
  int nconnections_per_dim = ndim * kary_;
  if (dim == flattened_butterfly::up_dimension) {
    return dir;
  }
  else {
    return dir + nconnections_per_dim;
  }
}

void
flattened_butterfly::productive_path(int i, const coordinates &src_coords, const coordinates &dest_coords, routing_info::path &path) const
{
  // this is the port that leads to the correct coordinate
  int dir = i * kary_ + dest_coords[i];
  int dim;
  if (src_coords[i] > dest_coords[i]) {
    dim = flattened_butterfly::down_dimension;
    path.vc = 0;
  }
  else {
    dim = flattened_butterfly::up_dimension;
    path.vc = 0;
  }
  path.outport = convert_to_port(dim, dir);
}

void
flattened_butterfly::minimal_route_to_coords(
  const coordinates &src_coords,
  const coordinates &dest_coords,
  routing_info::path& path) const
{
  for (int i=0; i < src_coords.size(); ++i) {
    if (src_coords[i] != dest_coords[i]) {
      productive_path(i, src_coords, dest_coords, path);
      return;
    }
  }
}

void
flattened_butterfly::compute_switch_coords(switch_id uid,
    coordinates& coords) const
{
  abstract_butterfly::compute_switch_coords(uid, coords);
}

switch_id
flattened_butterfly::switch_number(const coordinates &coords) const
{
  return abstract_butterfly::switch_number(coords);
}

void
flattened_butterfly::init_factory_params(sprockit::sim_parameters *params)
{
  abstract_butterfly::init_factory_params(params);
  int nstages = nfly_ - 1;
  int up_radix = kary_ * nstages;
  int down_radix = up_radix;
  max_ports_intra_network_ = up_radix + down_radix;
  eject_geometric_id_ = max_ports_intra_network_;
}

void
flattened_butterfly::display_nodes(vis_switch_map &switches,
                                   vis::vis_engine* eng,
                                   std::list<vis::vis_obj*> &objs)
{
  /* for (long i = 0; i < switches.size(); i++)
   {
   nodeid me = nodeaddress::convert_from_uniqueid(i);

   std::vector<int> coords = get_coords(i);

   double x = 0, y = 0, z = 0;
   x = coords[0] * 2;
   if (coords.size() > 1)
   y = coords[1] * 2;

   if (coords.size() > 2)
   z = coords[2] * 2;

   vis::vis_obj* vo = eng->create_cube(x, y, z, 1);
   switches[me]->set_vis_obj(vo);
   switches[me]->set_vis_engine(eng);
   objs.push_back(vo);

   for (int z = 0; z < dimensions_.size(); z++)
   {
   double x1 = coords[0] * 2;
   double y1 = coords[1] * 2;
   double z1 = coords[2] * 2;

   double x2 = coords[0] * 2;
   double y2 = coords[1] * 2;
   double z2 = coords[2] * 2;
   int dir;

   bool onegood = (coords[z] != dimensions_[z] - 1);
   bool twogood = (coords[z] != 0);

   if (z == 0)
   {

   x1 += 1.5;
   z1 += 0.5;
   y1 += 0.25;

   x2 -= 0.5;
   z2 += 0.5;
   y2 += 0.75;

   dir = vis::vis_engine::VIS_Z;
   }
   else if (z == 1)
   {
   y1 += 1.5;
   z1 += 0.25;
   x1 += 0.5;

   y2 -= 0.5;
   z2 += 0.75;
   x2 += 0.5;
   dir = vis::vis_engine::VIS_Y;
   }
   else
   {
   z1 += 1.5;
   y1 += 0.25;
   x1 += 0.5;

   z2 -= 0.5;
   y2 += 0.75;
   x2 += 0.5;
   dir = vis::vis_engine::VIS_X;
   }

   if (onegood)
   {
   vis::vis_obj* c1 = eng->create_cylinder(x1, y1, z1,
   0.125, 1, dir);
   objs.push_back(c1);
   switches[me]->set_link_vis(c1, z, router::POS);
   }

   if (twogood)
   {
   vis::vis_obj* c2 = eng->create_cylinder(x2, y2, z2,
   0.125, 1, dir);
   switches[me]->set_link_vis(c2, z, router::NEG);
   objs.push_back(c2);
   }
   }
   }*/
}

}
} //end of namespace sstmac



