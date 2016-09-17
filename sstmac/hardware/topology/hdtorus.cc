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

// hdtorus.cc: Implementation of a high dimensional torus network.
//
// Author: Jeremiah Wilke <jjwilke@sandia.gov>
//

#include <sstmac/hardware/topology/hdtorus.h>
#include <stdio.h>
#include <sstream>
#include <sprockit/stl_string.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

SpktRegister("torus | hdtorus", topology, hdtorus,
            "hdtorus implements a high-dimension torus with an arbitrary number of dimensions");

static bool
equals(const std::vector<int>& coords, int x, int y, int z)
{
  if (coords.size() != 3) {
    return false;
  }
  return coords[0] == x && coords[1] == y && coords[2] == z;
}

hdtorus::hdtorus(sprockit::sim_parameters* params) :
  cartesian_topology(params,
                     InitMaxPortsIntra::I_Remembered,
                     InitGeomEjectID::I_Remembered)
{
  num_switches_ = 1;
  diameter_ = 0;
  for (int i = 0; i < (int) dimensions_.size(); i++) {
    num_switches_ *= dimensions_[i];
    diameter_ += dimensions_[i] / 2;
  }

  max_ports_intra_network_ = 2 * dimensions_.size();
  eject_geometric_id_ = max_ports_intra_network_;
}

void
hdtorus::minimal_route_to_switch(
  switch_id src,
  switch_id dst,
  routable::path& path) const
{
  int div = 1;
  int ndim = dimensions_.size();
  for (int i=0; i < ndim; ++i){
    int srcX = (src / div) % dimensions_[i];
    int dstX = (dst / div) % dimensions_[i];
    if (srcX != dstX){
      if (shortest_path_positive(i, srcX, dstX)){
        top_debug("hdtorus routing up on dim %d for switch %d to %d on port %d",
                  i, src, dst, path.outport);
        up_path(i, srcX, dstX, path);
      } else {
        down_path(i, srcX, dstX, path);
        top_debug("hdtorus routing down on dim %d for switch %d to %d on port %d",
                  i, src, dst, path.outport);
      }
      return;
    }
    div *= dimensions_[i];
  }
}

int
hdtorus::shortest_distance(int dim, int src, int dst) const
{
  int up_distance, down_distance;
  if (dst > src) {
    up_distance = dst - src;
    down_distance = src + (dimensions_[dim] - dst);
  }
  else {
    up_distance = dst + (dimensions_[dim] - src);
    down_distance = src - dst;
  }

  if (up_distance > down_distance) {
    //shorter to go down
    return ((src - dst) + dimensions_[dim]) % dimensions_[dim];
  }
  else {
    //shorter to go up
    return ((dst - src) + dimensions_[dim]) % dimensions_[dim];
  }
}

int
hdtorus::minimal_distance(
  switch_id src,
  switch_id dst) const
{
  int div = 1;
  int ndim = dimensions_.size();
  int dist = 0;
  for (int i=0; i < ndim; ++i){
    int srcX = (src / div) % dimensions_[i];
    int dstX = (dst / div) % dimensions_[i];
    dist = shortest_distance(i, srcX, dstX);
    div *= dimensions_[i];
  }

  return dist;
}

bool
hdtorus::shortest_path_positive(
  int dim, int src, int dst) const
{
  int up_distance, down_distance;
  if (dst > src) {
    up_distance = dst - src;
    down_distance = src + (dimensions_[dim] - dst);
  }
  else {
    up_distance = dst + (dimensions_[dim] - src);
    down_distance = src - dst;
  }

  return up_distance <= down_distance;
}

void
hdtorus::torus_path(bool reset_dim, bool wrapped, int dim, int dir,
                    routable::path& path) const
{
  if (wrapped){
    path.set_metadata_bit(routable::crossed_timeline);
  }

  if (path.metadata_bit(routable::crossed_timeline)){
    path.vc = 1;
  } else {
    path.vc = 0;
  }
  path.outport = convert_to_port(dim, dir);

  if (reset_dim){
    path.unset_metadata_bit(routable::crossed_timeline); //we reached this dim
  }
}

void
hdtorus::up_path(
  int dim, int srcX, int dstX,
  routable::path& path) const
{

  bool reset_dim = (srcX + 1) % dimensions_[dim] == dstX;
  bool wrapped = srcX == (dimensions_[dim]-1);

  torus_path(reset_dim, wrapped, dim, pos, path);
}

void
hdtorus::down_path(
  int dim, int src, int dst,
  routable::path& path) const
{
  bool reset_dim = src == ((dst + 1) % dimensions_[dim]);
  bool wrapped = src == 0;

  torus_path(reset_dim, wrapped, dim, neg, path);
}


void
hdtorus::connect_dim(
  sprockit::sim_parameters* params,
  int dim,
  connectable* center,
  connectable* plus_partner,
  connectable* minus_partner)
{
  int pos_outport = convert_to_port(dim, pos);
  int neg_outport = convert_to_port(dim, neg);

  sprockit::sim_parameters* pos_params = get_port_params(params, pos_outport);
  sprockit::sim_parameters* neg_params = get_port_params(params, neg_outport);

  center->connect_output(
    pos_params,
    pos_outport, neg_outport,
    plus_partner);
  plus_partner->connect_input(
    neg_params,
    pos_outport, neg_outport,
    center);

  center->connect_output(
    neg_params,
    neg_outport, pos_outport,
    minus_partner);
  minus_partner->connect_input(
    pos_params,
    neg_outport, pos_outport,
    center);
}

void
hdtorus::connect_objects(sprockit::sim_parameters* params,
                         internal_connectable_map& objects)
{
  top_debug("hdtorus: connecting %d switches",
    int(objects.size()));

  sprockit::sim_parameters* link_params = params->get_namespace("link");
  double bw = link_params->get_bandwidth_param("bandwidth");
  int bufsize = params->get_byte_length_param("buffer_size");
  int ndims = dimensions_.size();
  for (int i=0; i < ndims; ++i){
    double port_bw = bw * red_[i];
    int credits = bufsize * red_[i];
    for (int dir=0; dir < 2; ++dir){
      int port = convert_to_port(i, dir);
      setup_port_params(port, credits, port_bw, link_params, params);
    }
  }

  internal_connectable_map::iterator it;
  for (it =  objects.begin(); it !=  objects.end(); ++it) {
    switch_id me(it->first);
    coordinates coords = switch_coords(me);

    top_debug("hdtorus: connecting switch %d,%s",
        int(me), stl_string(coords).c_str());

    std::vector<int> connected;
    std::vector<int> connected_red;

    for (int z = 0; z < (int) dimensions_.size(); z++) {
      int num_this_dim = dimensions_[z];
      if (num_this_dim == 1){
        continue; //no connections to be done
      }

      coordinates connect1 = coords;
      coordinates connect2 = coords;
      bool wrap_pos = false;
      bool wrap_neg = false;
      if (coords[z] == 0) {
        connect1[z]++;
        connect2[z] = dimensions_[z] - 1;
        wrap_neg = true;
      }
      else if (coords[z] == (dimensions_[z] - 1)) {
        connect1[z] = 0;
        connect2[z]--;
        wrap_pos = true;
      }
      else {
        connect1[z]++;
        connect2[z]--;
      }
      switch_id addr1 = switch_addr(connect1);
      switch_id addr2 = switch_addr(connect2);

      connectable* dest1 = NULL, *dest2 = NULL;

      /** We use a special lookup function rather than directly
          getting from the map.  The object might not exist
          due to parallel partitioning */
      dest1 = objects[addr1];
      dest2 = objects[addr2];



      top_debug("switch %d,%s for dim %d connecting in the pos dir to %d,%s : %s",
         int(me), stl_string(coords).c_str(), z,
         int(addr1), stl_string(connect1).c_str(),
         dest1->to_string().c_str());

      top_debug("switch %d,%s for dim %d connecting in the neg dir to %d,%s : %s",
         int(me), stl_string(coords).c_str(), z,
         int(addr2), stl_string(connect2).c_str(),
         dest2->to_string().c_str());



      connect_dim(params, z, objects[me], dest1, dest2);

    }
  }
}

void
hdtorus::configure_vc_routing(std::map<routing::algorithm_t, int>& m) const
{
  m[routing::minimal] = 2;
  m[routing::minimal_adaptive] = 2;
  m[routing::valiant] = 4;
  m[routing::ugal] = 6;
}

coordinates
hdtorus::switch_coords(switch_id uid) const
{
  int div = 1;
  int ndim = dimensions_.size();
  coordinates coords(ndim);
  for (int i = 0; i < ndim; i++) {
    coords[i] = (uid / div) % dimensions_[i];
    div *= dimensions_[i];
  }
  return coords;
}

switch_id
hdtorus::switch_addr(const coordinates& coords) const
{
  int ret = 0;
  int mult = 1;
  for (int i = 0; i < (int) dimensions_.size(); i++) {
    ret += coords[i] * mult;
    mult *= dimensions_[i];
  }
  return switch_id(ret);
}


}
} //end of namespace sstmac

