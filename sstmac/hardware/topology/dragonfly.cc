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

#include <sstmac/hardware/topology/dragonfly.h>
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
);

namespace sstmac {
namespace hw {

static const double PI = 3.141592653589793238462;

Dragonfly::Dragonfly(SST::Params& params) :
  CartesianTopology(params)
{
  if (dimensions_.size() != 2){
    spkt_abort_printf("dragonfly topology geometry should have 2 entries: routers per group, num groups");
  }

  static const double TWO_PI = 6.283185307179586;
  vtk_edge_size_ = params->get_optional_double_param("vtk_edge_size", 0.25);

  a_ = dimensions_[0];
  g_ = dimensions_[1];

  //determine radius to make x-dim of switches 0.33
  //r*2pi = edge*n - edge here is 25% larger than requested edge size
  vtk_radius_ = (vtk_edge_size_*1.25*a_*g_) / TWO_PI;
  vtk_box_length_ = 0.2*vtk_radius_;

  vtk_group_radians_ = TWO_PI / g_;
  vtk_switch_radians_ = vtk_group_radians_ / a_ / 1.5;

  if (params->has_param("group_connections")){
    h_ = params->get_int_param("group_connections");
  } else {
    h_ = params->get_int_param("h");
  }

  group_wiring_ = InterGroupWiring::factory::get_optional_param("inter_group", "circulant", params,
    a_, g_, h_);
}

void
Dragonfly::endpointsConnectedToInjectionSwitch(SwitchId swaddr,
                                   std::vector<injection_port>& nodes) const
{
  nodes.resize(concentration_);
  for (int i = 0; i < concentration_; i++) {
    injection_port& port = nodes[i];
    port.nid = swaddr*concentration_ + i;
    port.switch_port = a_ + h_ + i;
    port.ep_port = 0;
  }
}

int
Dragonfly::minimalDistance(SwitchId src, SwitchId dst) const
{
  int srcA, srcG; getCoords(src, srcA, srcG);
  int dstA, dstG; getCoords(dst, dstA, dstG);

  if (srcG == dstG) return 1;

  std::vector<int> connected;
  int directConn = -1;
  group_wiring_->connectedRouters(srcA, srcG, connected);
  for (int i=0; i < connected.size(); ++i){
    if (connected[i] == dst){
      return 1;
    } else if (computeG(connected[i]) == dstG){
      directConn = connected[i];
    }
  }

  if (directConn > 0){
    return 2;
  } else {
    return 3;
  }
}


void
Dragonfly::setupPortParams(SST::Params& params, int red, int port_offset, int num_ports) const
{
  SST::Params link_params = params.get_namespace("link");
  double bw = link_params->get_bandwidth_param("bandwidth");
  int bufsize = params->get_optional_byte_length_param("buffer_size", 0);

  double port_bw = bw * red;
  int credits = bufsize * red;

  for (int i=0; i < num_ports; ++i){
    int port = port_offset + i;
    SST::Params port_params = Topology
        ::setupPortParams(port, credits, port_bw, link_params, params);
  }
}

void
Dragonfly::connectedOutports(SwitchId src, std::vector<connection>& conns) const
{
  int max_num_conns = (a_ - 1) + h_;
  conns.resize(max_num_conns);

  int myA;
  int myG;
  getCoords(src, myA, myG);

  int cidx = 0;

  for (int a = 0; a < a_; a++){
    if (a != myA){
      SwitchId dst = getUid(a, myG);
      connection& conn = conns[cidx++];
      conn.src = src;
      conn.dst = dst;
      conn.src_outport = a;
      conn.dst_inport = myA;
    }
  }

  std::vector<int> groupConnections;
  group_wiring_->connectedRouters(myA, myG, groupConnections);
  for (int h = 0; h < groupConnections.size(); ++h){
    SwitchId dst = groupConnections[h];
    int dstG = computeG(dst);
    if (dstG != myG){
      connection& conn = conns[cidx++];
      conn.src = src;
      conn.dst = dst;
      conn.src_outport = h + a_;
      int dstA = computeA(dst);
      conn.dst_inport = group_wiring_->inputGroupPort(myA, myG, h, dstA, dstG) + a_;

      top_debug("dragonfly (%d,%d:%d)->(%d,%d:%d)",
         myA, myG, conn.src_outport, dstA, dstG, conn.dst_inport);

    }
  }
  conns.resize(cidx);
}

void
Dragonfly::configureIndividualPortParams(SwitchId src, SST::Params& switch_params) const
{
  setupPortParams(switch_params, red_[0], 0, a_);
  setupPortParams(switch_params, red_[1], a_, h_);
}

bool
Dragonfly::isCurvedVtkLink(SwitchId sid, int port) const
{
  if (port >= a_){
    return false; //global link - these are straight lines
  } else {
    return true; //local link - these need to be curved
  }
}

Topology::vtk_switch_geometry
Dragonfly::getVtkGeometry(SwitchId sid) const
{
  /**
   * The switches are arranged in circle. The faces
   * pointing into the circle represent inter-group traffic
   * while the the faces pointing out of the circle are intra-group traffic.
   * The "reference" switch starts at [radius, 0, 0]
   * and has dimensions [1.0,0.25,0.25]
   * This reference switch is rotated by an angle theta determine by the group number
   * and the position of the switch within the group. The radius is chosen to be large
   * enough to fit all switches with an inner size of 2.5
  */

  int myA = computeA(sid);
  int myG = computeG(sid);

  //we need to figure out the radian offset of the group
  double inter_group_offset = myG*vtk_group_radians_;
  double intra_group_start = myA*vtk_switch_radians_;

  double theta = inter_group_offset + intra_group_start;

  /** With no rotation, these are the corners.
   * These will get rotated appropriately */
  double zCorner = 0.0;
  double yCorner = 0.0;
  double xCorner = vtk_radius_;

  double xSize = vtk_box_length_;
  double ySize = 0.25; //this is the face pointing "into" the circle
  double zSize = 0.25;

  std::vector<vtk_switch_geometry::port_geometry> ports(a_ + h_ + concentration());
  double port_fraction_a = 1.0 / a_;
  double port_fraction_h = 1.0 / h_;
  double port_fraction_c = 1.0 / concentration();

  for (int a=0; a < a_; ++a){
    vtk_switch_geometry::port_geometry& p = ports[a];
    p.x_offset = 1.0;
    p.x_size = -0.3;
    p.y_offset = a * port_fraction_a;
    p.y_size = port_fraction_a;
    p.z_offset = 0;
    p.z_size = 1.0;
  }

  for (int h=0; h < h_; ++h){
    vtk_switch_geometry::port_geometry& p = ports[a_ + h];
    p.x_offset = 0;
    p.x_size = 0.3;
    p.y_offset = h * port_fraction_h;
    p.y_size = port_fraction_h;
    p.z_offset = 0;
    p.z_size = 1.0;
  }


  for (int c=0; c < concentration(); ++c){
    vtk_switch_geometry::port_geometry& p = ports[a_ + h_ + c];
    p.x_offset = 0.35;
    p.x_size = 0.35;
    p.y_offset = c * port_fraction_c;
    p.y_size = port_fraction_c;
    p.z_offset = 0;
    p.z_size = 1.0;
  }

  vtk_switch_geometry geom(xSize, ySize, zSize,
                           xCorner, yCorner, zCorner, theta,
                           std::move(ports));

  return geom;
}

InterGroupWiring::InterGroupWiring(SST::Params& params, int a, int g, int h) :
  a_(a), g_(g), h_(h)
{
}

SwitchId
InterGroupWiring::randomIntermediate(Router* rtr, SwitchId current_sw, SwitchId dest_sw, uint32_t seed)
{
  int srcA = current_sw % g_;
  int srcG = current_sw / g_;
  int dstA = dest_sw % g_;
  int dstG = dest_sw / g_;
  SwitchId sid = current_sw;
  uint32_t attempt = 0;
  if (srcG == dstG){
    int interA = srcA;
    while (interA == srcA || interA == dstA){
      interA = rtr->randomNumber(a_, attempt, seed);
      ++attempt;
    }
    return srcG*a_ + interA;
  } else {
    int interA = rtr->randomNumber(a_, attempt, seed);
    int interG = srcG;
    while (interG == srcG || interG == dstG){
      interG = rtr->randomNumber(g_, attempt, seed);
      ++attempt;
    }
    return interG*a_ + interA;
  }
}

static inline int mod(int a, int b){
  int rem = a % b;
  return rem < 0 ? rem + b : rem;
}

class SingleLinkGroupWiring : public InterGroupWiring
{
  FactoryRegister("single", InterGroupWiring, SingleLinkGroupWiring)
 public:
  SingleLinkGroupWiring(SST::Params& params, int a, int g, int h) :
    InterGroupWiring(params, a, g, h)
  {
    if (h_ != 1){
      spkt_abort_printf("h must be 1 for single link inter-group pattern");
    }
  }

  int inputGroupPort(int srcA, int srcG, int srcH, int dstA, int dstG) const override {
    if (srcH != 0){
      spkt_abort_printf("h must be 1 for single link inter-group pattern");
    }
    return 0;
  }

  /**
   * @brief connected_routers
   * @param a The src router index within the group
   * @param g The src router group
   * @param connected [in-out] The routers (switch id) for each inter-group interconnection
   * @return The number of routers in connected array
   */
  void connectedRouters(int srcA, int srcG, std::vector<int>& connected) const override {
    int plusMinus = 1 - 2*(srcA%2);
    int deltaG = (1 + srcA/2)*plusMinus;
    int deltaA = plusMinus;
    int aMax = a_ - 1;
    if ( (a_%2) && (srcA == aMax) ){
      deltaA = 0;
    }
    connected.resize(1);
    int dstG = (srcG+g_+deltaG)%g_;
    int dstA = (srcA+a_+deltaA)%a_;
    SwitchId dst = dstG*a_ + dstA;
    connected[0] = dst;
  }

  /**
   * @brief connected_to_group
   * @param srcG
   * @param dstG
   * @param connected [in-out] The list of all intra-group routers in range (0 ... a-1)
   *                  that have connections to a router in group dstG. The second entry in the pair
   *                  is the "port offset", a unique index for the group link
   * @return The number of routers in group srcG with connections to dstG
   */
  void connectedToGroup(int srcG, int dstG, std::vector<std::pair<int,int>>& connected) const override {
    connected.resize(1);
    for (int a=0; a < a_; ++a){
      int plusMinus = 1 - 2*(a%2);
      int deltaG = (1 + a/2)*plusMinus;
      int testG = (srcG + deltaG + g_) % g_;
      if (testG == dstG){
        connected[0] = std::make_pair(a,0);
      }
    }
  }

};

class CirculantGroupWiring : public InterGroupWiring
{
  FactoryRegister("circulant", InterGroupWiring, CirculantGroupWiring)
 public:
  CirculantGroupWiring(SST::Params& params, int a, int g, int h) :
    InterGroupWiring(params, a, g, h)
  {
    if (h_ % 2){
      spkt_abort_printf("group connections must be even for circulant inter-group pattern");
    }
  }

  int inputGroupPort(int srcA, int srcG, int srcH, int dstA, int dstG) const override {
    if (srcH % 2 == 0){
      return srcH + 1;
    } else {
      return srcH - 1;
    }
  }

  /**
   * @brief connected_routers
   * @param a The src router index within the group
   * @param g The src router group
   * @param connected [in-out] The routers (switch id) for each inter-group interconnection
   * @return The number of routers in connected array
   */
  void connectedRouters(int srcA, int srcG, std::vector<int>& connected) const override {
    connected.clear();
    int dstA = srcA;
    int half = h_  /2;
    for (int h=1; h <= half; ++h){
      int plusG = mod(srcG + srcA*half + h, g_);
      if (plusG != srcG){
        SwitchId dst = plusG*a_ + dstA;
        connected.push_back(dst); //full switch ID
      }
      int minusG = mod(srcG - srcA*half - h, g_);
      if (minusG != srcG){
        SwitchId dst = minusG*a_ + dstA;
        connected.push_back(dst); //full switch ID
      }
    }
  }

  /**
   * @brief connected_to_group
   * @param srcG
   * @param dstG
   * @param connected [in-out] The list of all intra-group routers in range (0 ... a-1)
   *                  that have connections to a router in group dstG. The second entry in the pair
   *                  is the "port offset", a unique index for the group link
   * @return The number of routers in group srcG with connections to dstG
   */
  void connectedToGroup(int srcG, int dstG, std::vector<std::pair<int,int>>& connected) const override {
    connected.clear();
    std::vector<int> tmp;
    for (int a=0; a < a_; ++a){
      connectedRouters(a, srcG, tmp);
      for (int c=0; c < tmp.size(); ++c){
        int g = tmp[c] / a_;
        if (dstG == g){
          connected.emplace_back(a,c);
          break;
        }
      }
    }
  }

};

class AllToAllGroupWiring : public InterGroupWiring
{
  FactoryRegister("alltoall", InterGroupWiring, AllToAllGroupWiring)
 public:
  AllToAllGroupWiring(SST::Params& params, int a, int g, int h) :
    InterGroupWiring(params, a, g, h)
  {
    covering_ = h_ / (g_-1);
    if (covering_ == 0){
      spkt_abort_printf("Group connections h=%d is not able to provide all-to-all covering for g=%d",
                        h_, g_);
    }

    stride_ = a_ / covering_;

    top_debug("alltoall links cover groups %dx with stride=%d", covering_, stride_);

    if (h_ % (g_-1)) spkt_abort_printf("dragonfly #groups-1=%d must evenly divide #group_connections=%d", g_-1, h_);
    if (a_ % covering_) spkt_abort_printf("dragonfly covering=%d must evenly divide group_size=%d", covering_, a_);
  }

  int inputGroupPort(int srcA, int srcG, int srcH, int dstA, int dstG) const override {
    int deltaA = (srcA + a_ - dstA) % a_; //mod arithmetic in case srcA < dstA
    int offset = deltaA / stride_;
    if (srcG < dstG){
      return srcG*covering_ + offset;
    } else {
      return (srcG - 1)*covering_ + offset;
    }
  }

  /**
   * @brief connected_routers
   * @param a The src router index within the group
   * @param g The src router group
   * @param connected [in-out] The routers (switch id) for each inter-group interconnection
   * @return The number of routers in connected array
   */
  void connectedRouters(int srcA, int srcG, std::vector<int>& connected) const override {
    connected.clear();
    for (int g=0; g < g_; ++g){
      if (g == srcG) continue;
      for (int c=0; c < covering_; ++c){
        int dstA = (srcA + stride_*c) % a_;
        int dst = g*a_ + dstA;
        connected.push_back(dst);
      }
    }
  }

  SwitchId randomIntermediate(Router* rtr, SwitchId current_sw, SwitchId dest_sw, uint32_t seed) override
  {
    int srcG = current_sw / a_;
    int dstG = dest_sw / a_;
    uint32_t attempt = 0;
    if (srcG == dstG){
      return InterGroupWiring::randomIntermediate(rtr, current_sw, dest_sw, seed);
    } else {
      int srcA = current_sw % a_;
      int interG = rtr->randomNumber(g_, attempt, seed);
      while (interG == srcG || interG == dstG){
        interG = rtr->randomNumber(g_, attempt, seed);
      }
      //go to a router directly connected to srcA
      int srcH = rtr->randomNumber(h_, attempt, seed);
      int interA = (srcH * stride_ + srcA) % a_;
      return interG*a_ + interA;
    }
  }

  /**
   * @brief connected_to_group
   * @param srcG
   * @param dstG
   * @param connected [in-out] The list of all intra-group routers in range (0 ... a-1)
   *                  that have connections to a router in group dstG. The second entry in the pair
   *                  is the "port offset", a unique index for the group link
   * @return The number of routers in group srcG with connections to dstG
   */
  void connectedToGroup(int srcG, int dstG, std::vector<std::pair<int,int>>& connected) const override {
    connected.clear();
    for (int a=0; a < a_; ++a){
      int offset = dstG * covering_;
      for (int c=0; c < covering_; ++c){
        connected.emplace_back(a, offset+c);
      }
    }
  }

 private:
  int covering_;
  int stride_;
};


}
} //end of namespace sstmac
