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
hdtorus::configure_individual_port_params(switch_id src,
                                          sprockit::sim_parameters *switch_params) const
{
  sprockit::sim_parameters* link_params = switch_params->get_namespace("link");
  double bw = link_params->get_bandwidth_param("bandwidth");
  int bufsize = switch_params->get_byte_length_param("buffer_size");
  int ndims = dimensions_.size();
  for (int i=0; i < ndims; ++i){
    double port_bw = bw * red_[i];
    int credits = bufsize * red_[i];
    for (int dir=0; dir < 2; ++dir){
      int port = convert_to_port(i, dir);
      setup_port_params(port, credits, port_bw, link_params, switch_params);
    }
  }
}

void
hdtorus::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  int ndims = dimensions_.size();
  int dim_stride = 1;
  conns.resize(ndims * 2); //+/-1 for each direction

  int cidx = 0;
  for (int i=0; i < ndims; ++i){
    int plus_jump = 1;
    int minus_jump = -1;
    int srcX = (src / dim_stride) % dimensions_[i];
    int last_row = dimensions_[i] - 1;
    if (srcX == last_row){
      //wrap around
      plus_jump = -last_row;
    } else if (srcX == 0){
      minus_jump = last_row;
    }

    switch_id plus_partner = src + plus_jump * dim_stride;
    int plus_port = convert_to_port(i, pos);

    switch_id minus_partner = src + minus_jump * dim_stride;
    int minus_port = convert_to_port(i, neg);

    conns[cidx].src = src;
    conns[cidx].dst = plus_partner;
    conns[cidx].src_outport = plus_port;
    conns[cidx].dst_inport = minus_port;
    ++cidx;

    conns[cidx].src = src;
    conns[cidx].dst = minus_partner;
    conns[cidx].src_outport = minus_port;
    conns[cidx].dst_inport = plus_port;
    ++cidx;

    dim_stride *= dimensions_[i];
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

