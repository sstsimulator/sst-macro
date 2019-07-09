/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

Hypercube::Hypercube(SST::Params& params) :
  Torus(params)
{
  ndim_ = dimensions_.size();
  dim_to_outport_.resize(ndim_);
  int offset = 0;
  for (int i=0; i < ndim_; ++i) {
    dim_to_outport_[i] = offset;
    offset += dimensions_[i];
  }
}

void
Hypercube::endpointsConnectedToInjectionSwitch(SwitchId swaddr,
                                   std::vector<InjectionPort>& nodes) const
{
  int total_ports = 0;
  for (int size : dimensions_){
    total_ports += size;
  }

  nodes.resize(concentration_);
  for (int i = 0; i < concentration_; i++) {
    InjectionPort& port = nodes[i];
    port.nid = swaddr*concentration_ + i;
    port.switch_port = total_ports + i;
    port.ep_port = 0;
  }
}

void
Hypercube::minimalRouteToSwitch(
  SwitchId src,
  SwitchId dst,
  Packet::Header* hdr) const
{
  int ndim = dimensions_.size();
  int div = 1;
  for (int i=0; i < ndim; ++i){
    int srcX = (src / div) % dimensions_[i];
    int dstX = (dst / div) % dimensions_[i];
    if (srcX != dstX){
      hdr->deadlock_vc = 0;
      hdr->edge_port = convertToPort(i, dstX);
      return;
    }
    div *= dimensions_[i];
  }
}

int
Hypercube::minimalDistance(
  SwitchId src,
  SwitchId dst) const
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

double
Hypercube::portScaleFactor(uint32_t addr, int port) const
{
  int port_cutoff = 0;
  int dim=0; 
  for (int size : dimensions_){
    port_cutoff += size;
    if (port < port_cutoff){
      break;
    }
    ++dim;
  }
  if (dim == dimensions_.size()){
    return 1;//injection_redundancy_;
  } else {
    return red_[dim];
  }
}

void
Hypercube::connectedOutports(SwitchId src, std::vector<Connection>& conns) const
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

      SwitchId dst = src + (dstX - srcX) * dim_stride;

      int outport = convertToPort(dim, dstX);
      int inport = convertToPort(dim, srcX);

      Connection& conn = conns[cidx];
      conn.src = src;
      conn.dst = dst;
      conn.src_outport = outport;
      conn.dst_inport = inport;
      ++cidx;
    }

    dim_stride *= dimensions_[dim];
  }
}

Topology::VTKSwitchGeometry
Hypercube::getVtkGeometry(SwitchId sid) const
{
  coordinates coords = switchCoords(sid);
  int ndims = dimensions_.size();
  if (ndims > 3 || ndims < 2){
    spkt_abort_printf("cannot generate xyz coordinates for topologies with ndims=%d - only 2D or 3D torus allowed",
                      ndims);
  }

  /**
   * Each box is size 1x1x1... leave a 0.5 gap between switches
   */
  double xCorner = 1.5*coords[0];
  double yCorner = 1.5*coords[1];
  double zCorner = 1.5*(ndims > 2 ? coords[2] : 0);
  double xSize = 1.0;
  double ySize = 1.0;
  double zSize = 1.0;
  double theta = 0.0;

  int num_ports = 0;
  for (int d=0; d < ndims; ++d){
    num_ports += dimensions_[d];
  }

  //vtk_face_t dim_to_face[] = { plusXface, plusYface, plusZface };

  std::vector<VTKSwitchGeometry::port_geometry> ports(num_ports);
  /**
  for (int d=0; d < ndims; ++d){
    int port_offset = dim_to_outport_[d];
    for (int p=0; p < dimensions_[d]; ++p){
      faces[port_offset+p] = dim_to_face[d];
    }
  }
  */

  VTKSwitchGeometry geom(xSize,ySize,zSize,xCorner,yCorner,zCorner,theta,
                           std::move(ports));

  return geom;
}



}
} //end of namespace sstmac
