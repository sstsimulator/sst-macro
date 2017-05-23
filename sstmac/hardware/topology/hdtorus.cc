/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sstmac/hardware/topology/hdtorus.h>
#include <stdio.h>
#include <sstream>
#include <sprockit/stl_string.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

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
  int bufsize = link_params->get_byte_length_param("buffer_size");
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