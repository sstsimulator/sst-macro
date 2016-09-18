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
// Author: Jeremiah Wilke <jjwilke@sandia.gov>
//

#include <sstmac/hardware/topology/hypercube.h>
#include <stdio.h>
#include <sstream>
#include <sprockit/stl_string.h>

namespace sstmac {
namespace hw {

SpktRegister("hypercube", topology, hypercube,
            "hypercube implements a high-dimension torus with an arbitrary number of dimensions");

hypercube::hypercube(sprockit::sim_parameters* params) :
  hdtorus(params)
{
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

void
hypercube::minimal_route_to_switch(
  switch_id src,
  switch_id dst,
  routable::path& path) const
{
  int ndim = dimensions_.size();
  int div = 1;
  for (int i=0; i < ndim; ++i){
    int srcX = (src / div) % dimensions_[i];
    int dstX = (dst / div) % dimensions_[i];
    if (srcX != dstX){
      path.vc = 0;
      path.outport = convert_to_port(i, dstX);
      return;
    }
    div *= dimensions_[i];
  }
}

int
hypercube::minimal_distance(
  switch_id src,
  switch_id dst) const
{
  int dist = 0;
  int div = 1;
  int ndim = dimensions_.size();
  for (int i=0; i < ndim; ++i){
    int srcX = (src / div) % dimensions_[i];
    int dstX = (dst / div) % dimensions_[i];
    if (srcX != dstX){
      ++dist;
    }
    div *= dimensions_[i];
  }
  return dist;
}

void
hypercube::configure_individual_port_params(switch_id src,
                                          sprockit::sim_parameters *switch_params) const
{
  sprockit::sim_parameters* link_params = switch_params->get_namespace("link");
  double bw = link_params->get_bandwidth_param("bandwidth");
  int bufsize = switch_params->get_byte_length_param("buffer_size");
  for (int dim=0; dim < dimensions_.size(); ++dim){
    double port_bw = bw * red_[dim];
    int credits = bufsize * red_[dim];
    for (int dir=0; dir < dimensions_[dim]; ++dir){
      int port = convert_to_port(dim, dir);
      setup_port_params(port, credits, port_bw, link_params, switch_params);
    }
  }
}

void
hypercube::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  int nconns = 0;
  for (int dim=0; dim < dimensions_.size(); ++dim){
    nconns += dimensions_[dim] - 1;
  }
  conns.resize(nconns);

  //loop every dimension and connect to all "neighbors"
  int dim_stride = 1;
  int cidx = 0;
  for (int dim=0; dim < dimensions_.size(); ++dim) {
    int dimsize = dimensions_[dim];
    int srcX = (src / dim_stride) % dimensions_[dim];
    for (int dstX=0; dstX < dimsize; ++dstX) {
      if (dstX == srcX) continue;

      switch_id dst = src + (dstX - srcX) * dim_stride;

      int outport = convert_to_port(dim, dstX);
      int inport = convert_to_port(dim, srcX);

      connection& conn = conns[cidx];
      conn.src = src;
      conn.dst = dst;
      conn.src_outport = outport;
      conn.dst_inport = inport;
      ++cidx;
    }

    dim_stride *= dimensions_[dim];
  }
}



}
} //end of namespace sstmac




