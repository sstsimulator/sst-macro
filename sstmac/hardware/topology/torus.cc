/**
Copyright 2009-2021 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2021, NTESS

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

#include <sstmac/hardware/topology/torus.h>
#include <stdio.h>
#include <sstream>
#include <sprockit/stl_string.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

Torus::Torus(SST::Params& params) :
  CartesianTopology(params)
{
  num_switches_ = 1;
  diameter_ = 0;
  for (int i = 0; i < (int) dimensions_.size(); i++) {
    num_switches_ *= dimensions_[i];
    diameter_ += dimensions_[i] / 2;
  }
}

void
Torus::endpointsConnectedToInjectionSwitch(SwitchId swaddr,
                                   std::vector<InjectionPort>& nodes) const
{
  int total_ports = dimensions_.size() * 2;

  nodes.resize(concentration_);
  for (int i = 0; i < concentration_; i++) {
    InjectionPort& port = nodes[i];
    port.nid = swaddr*concentration_ + i;
    port.switch_port = total_ports + i;
    port.ep_port = 0;
  }
}

int
Torus::shortestDistance(int dim, int src, int dst) const
{
  int up_distance, down_distance;
  if (dst > src) {
    up_distance = dst - src;
    down_distance = src + (dimensions_[dim] - dst);
  } else {
    up_distance = dst + (dimensions_[dim] - src);
    down_distance = src - dst;
  }

  if (up_distance > down_distance) {
    //shorter to go down
    return ((src - dst) + dimensions_[dim]) % dimensions_[dim];
  } else {
    //shorter to go up
    return ((dst - src) + dimensions_[dim]) % dimensions_[dim];
  }
}

int
Torus::minimalDistance(
  SwitchId src,
  SwitchId dst) const
{
  int div = 1;
  int ndim = dimensions_.size();
  int dist = 0;
  for (int i=0; i < ndim; ++i){
    int srcX = (src / div) % dimensions_[i];
    int dstX = (dst / div) % dimensions_[i];
    dist = shortestDistance(i, srcX, dstX);
    div *= dimensions_[i];
  }

  return dist;
}

bool
Torus::shortestPathPositive(
  int dim, int src, int dst) const
{
  int up_distance, down_distance;
  if (dst > src) {
    up_distance = dst - src;
    down_distance = src + (dimensions_[dim] - dst);
  } else {
    up_distance = dst + (dimensions_[dim] - src);
    down_distance = src - dst;
  }
  return up_distance <= down_distance;
}

double
Torus::portScaleFactor(uint32_t  /*addr*/, int port) const
{
  if (port >= 2*dimensions_.size()){
    //ejection port
    return injection_redundancy_;
  } else {
    int dim = port / 2;
    return red_[dim];
  }
}

void
Torus::connectedOutports(SwitchId src, std::vector<Connection>& conns) const
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

    SwitchId plus_partner = src + plus_jump * dim_stride;
    int plus_port = convertToPort(i, pos);

    SwitchId minus_partner = src + minus_jump * dim_stride;
    int minus_port = convertToPort(i, neg);

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

coordinates
Torus::switchCoords(SwitchId uid) const
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

SwitchId
Torus::switchAddr(const coordinates& coords) const
{
  int ret = 0;
  int mult = 1;
  for (int i = 0; i < (int) dimensions_.size(); i++) {
    ret += coords[i] * mult;
    mult *= dimensions_[i];
  }
  return SwitchId(ret);
}


Topology::VTKSwitchGeometry
Torus::getVtkGeometry(SwitchId sid) const
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

  std::vector<VTKSwitchGeometry::port_geometry> ports(6);
  /**
  faces[convertToPort(0,0)] = plusXface;
  faces[convertToPort(0,1)] = minusXface;
  faces[convertToPort(1,0)] = plusYface;
  faces[convertToPort(1,1)] = minusYface;
  faces[convertToPort(2,0)] = plusZface;
  faces[convertToPort(2,1)] = minusZface;
  */

  VTKSwitchGeometry geom(xSize,ySize,zSize,xCorner,yCorner,zCorner,theta,
                           std::move(ports));

  return geom;
}


}
} //end of namespace sstmac
