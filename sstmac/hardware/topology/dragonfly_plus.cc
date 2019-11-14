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

#include <sstmac/hardware/topology/dragonfly_plus.h>
#include <sstmac/hardware/router/router.h>
#include <math.h>
#include <sstream>
#include <sprockit/stl_string.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/sim_parameters.h>

RegisterKeywords(
{ "h", "the number inter-group connections per router" },
{ "group_connections", "the number of inter-group connections per router"},
{ "inter_group", "the inter-group wiring scheme"},
{ "vtk_row_spacing", "the relative spacing of dragonfly+ rows" },
);

namespace sstmac {
namespace hw {

// TODOWARNING static const double PI = 3.141592653589793238462;

DragonflyPlus::DragonflyPlus(SST::Params& params) :
  Dragonfly(params)
{
  if (h_ % (g_-1)){
    spkt_abort_printf("dragonfly+ currently requires an all-to-all group connectivity");
  }

  num_leaf_switches_ = a_*g_;

  vtk_row_spacing_ = params.find<double>("vtk_row_spacing", 2.0);
}

void
DragonflyPlus::endpointsConnectedToInjectionSwitch(SwitchId swaddr,
                                   std::vector<InjectionPort>& nodes) const
{
  int row = computeRow(swaddr);
  if (row > 0){
    nodes.clear();
    return;
  }

  nodes.resize(concentration_);
  for (int i = 0; i < concentration_; i++) {
    InjectionPort& port = nodes[i];
    port.nid = swaddr*concentration_ + i;
    port.switch_port = i + a_;
    port.ep_port = 0;
  }
}

int
DragonflyPlus::minimalDistance(SwitchId src, SwitchId dst) const
{
  int srcRow = computeRow(src);
  int dstRow = computeRow(dst);
  int srcGrp = computeG(src);
  int dstGrp = computeG(dst);

  if (srcGrp == dstGrp){
    if (srcRow == dstRow) return 2;
    else return 1;
  } else {
    return 1 + (1-srcRow) + (1-dstRow);
  }
}

std::string
DragonflyPlus::portTypeName(SwitchId sid, int port) const
{
  int row = computeRow(sid);
  if (row == 0){
    if (port < a_){
      return "intra-up";
    } else {
      return "injection";
    }
  } else {
    if (port < a_){
      return "intra-down";
    } else {
      return "global";
    }
  }
}

void
DragonflyPlus::connectedOutports(SwitchId src, std::vector<Connection>& conns) const
{
  conns.clear();

  int myRow;
  int myA;
  int myG;
  getCoords(src, myRow, myA, myG);

  if (myRow == 0){
    for (int a=0; a < a_; ++a){
      Connection next;
      next.src = src;
      next.src_outport = a;
      next.dst = getUid(1, a, myG);
      next.dst_inport = myA;
      top_debug("(%d=%d,%d:%d)->(%d=%d,%d:%d)",
                src, myA, myG, next.src_outport,
                next.dst, a, myG, next.dst_inport);
      conns.push_back(next);
    }
  } else {
    for (int a=0; a < a_; ++a){ //down to leaf
      Connection next;
      next.src = src;
      next.src_outport = a;
      next.dst = getUid(0, a, myG);
      next.dst_inport = myA;
      top_debug("(%d=%d,%d:%d)->(%d=%d,%d:%d)",
                src, myA, myG, next.src_outport,
                next.dst, a, myG, next.dst_inport);
      conns.push_back(next);
    }

    std::vector<int> newConns;
    group_wiring_->connectedRouters(myA, myG, newConns);
    for (int p=0; p < newConns.size(); ++p){
      int relDst = newConns[p];
      Connection next;
      next.src = src;
      next.src_outport = p + a_;
      next.dst = relDst + a_*g_; //num leaf switches
      int dstA = relDst % a_;
      int dstG = relDst / a_;
      next.dst_inport =  a_ + group_wiring_->inputGroupPort(myA, myG, p, dstA, dstG);
      conns.push_back(next);
      top_debug("(%d=%d,%d:%d)->(%d=%d,%d:%d)",
                src, myA, myG, next.src_outport,
                next.dst, dstA, dstG, next.dst_inport);
    }
  }
}

Topology::VTKSwitchGeometry
DragonflyPlus::getVtkGeometry(SwitchId sid) const
{
  int myRow;
  int myA;
  int myG;
  getCoords(sid, myRow, myA, myG);

  //we need to figure out the radian offset of the group
  double inter_group_offset = vtk_group_radians_ * myG;
  double intra_group_start = vtk_switch_radians_ * myA;

  double theta = inter_group_offset + intra_group_start;

  /** With no rotation, these are the corners.
   * These will get rotated appropriately */
  double zCorner = 0.0;
  double yCorner = 0.0;
  double xCorner = vtk_radius_;
  if (myRow == 0){
    //this is the "intra-group" row without group connections
    //put this in the outer circle
    xCorner += vtk_row_spacing_ * vtk_box_length_;
  }

  double xSize = vtk_box_length_;
  double ySize = 0.25; //this is the face pointing "into" the circle
  double zSize = 0.25;

  int num_ports = myRow == 0 ? a_ + concentration() : a_ + h_;
  std::vector<VTKSwitchGeometry::port_geometry> ports(num_ports);
  double y_fraction_a = 1.0 / double(a_);
  double y_fraction_h = 1.0 / double(h_);
  double y_fraction_c = 1.0 / double(concentration());
  if (myRow == 0){
    for (int a=0; a < a_; ++a){
      VTKSwitchGeometry::port_geometry& geom = ports[a];
      geom.x_offset = 0;
      geom.x_size = 0.3;
      geom.y_offset = a * y_fraction_a;
      geom.y_size = y_fraction_a;
      geom.z_offset = 0;
      geom.z_size = 1.0;
    }
    for (int c=0; c < concentration(); ++c){
      VTKSwitchGeometry::port_geometry& geom = ports[a_ + c];
      geom.x_offset = 1;
      geom.x_size = 0.3;
      geom.y_offset = c * y_fraction_c;
      geom.y_size = y_fraction_c;
      geom.z_offset = 0;
      geom.z_size = 1.0;
    }
  } else {
    for (int a=0; a < a_; ++a){
      VTKSwitchGeometry::port_geometry& geom = ports[a];
      geom.x_offset = 1;
      geom.x_size = -0.3;
      geom.y_offset = a * y_fraction_a;
      geom.y_size = y_fraction_a;
      geom.z_offset = 0;
      geom.z_size = 1.0;
    }
    for (int h=0; h < h_; ++h){
      VTKSwitchGeometry::port_geometry& geom = ports[a_ + h];
      geom.x_offset = 0;
      geom.x_size = 0.3;
      geom.y_offset = h * y_fraction_h;
      geom.y_size = y_fraction_h;
      geom.z_offset = 0;
      geom.z_size = 1.0;
    }
  }

  VTKSwitchGeometry geom(xSize, ySize, zSize,
                           xCorner, yCorner, zCorner, theta,
                           std::move(ports));

  return geom;
}


}
} //end of namespace sstmac
