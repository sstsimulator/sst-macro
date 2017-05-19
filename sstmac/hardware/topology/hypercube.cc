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

#include <sstmac/hardware/topology/hypercube.h>
#include <stdio.h>
#include <sstream>
#include <sprockit/stl_string.h>

namespace sstmac {
namespace hw {

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
  int bufsize = link_params->get_byte_length_param("buffer_size");
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