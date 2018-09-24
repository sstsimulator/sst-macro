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

dragonfly::dragonfly(sprockit::sim_parameters* params) :
  cartesian_topology(params,
                     InitMaxPortsIntra::I_Remembered,
                     InitGeomEjectID::I_Remembered)
{
  if (dimensions_.size() != 2){
    spkt_abort_printf("dragonfly topology geometry should have 2 entries: routers per group, num groups");
  }

  a_ = dimensions_[0];
  g_ = dimensions_[1];

  if (params->has_param("group_connections")){
    h_ = params->get_int_param("group_connections");
  } else {
    h_ = params->get_int_param("h");
  }

  /** No longer true - JJW 05/18/2018
    can never have more group connections than groups
  if (h_ >= g_){
    cerr0 << sprockit::printf("WARNING: requested %d group connections, "
                        "but max should be %d for %d groups\n",
                        h_, g_-1, g_);
  }
  */

  max_ports_intra_network_ = a_ + h_;
  eject_geometric_id_ = max_ports_intra_network_;

  group_wiring_ = inter_group_wiring::factory::get_optional_param("inter_group", "circulant", params, 
    a_, g_, h_);
}

void
dragonfly::configure_geometric_paths(std::vector<int> &redundancies)
{
  spkt_abort_printf("not implemented: dragonfly::configure geometric paths");
}

void
dragonfly::minimal_route_to_switch(
    switch_id src,
    switch_id dst,
    packet::path &path) const
{
  //see if intra-group
  int srcG = computeG(src);
  int dstG = computeG(dst);
  if (srcG == dstG){
    int dstA = computeA(dst);
    path.set_outport(dstA);
    return;
  }

  int srcA = computeA(src);
  //see if inter-group, but direct connection to that group
  std::vector<int> connected;
  group_wiring_->connected_routers(srcA, srcG, connected);
  for (int c=0; c < connected.size(); ++c){
    int testG = computeG(connected[c]);
    if (testG == dstG){
      path.set_outport(c + a_);
      return;
    }
  }

  //inter-group and we need local hop to get there
  std::vector<std::pair<int,int>> groupConnections;
  group_wiring_->connected_to_group(srcG, dstG, groupConnections);
  int srcRotater = srcA % groupConnections.size();

  auto& pair = groupConnections[srcRotater];
  path.set_outport(pair.first);
}

int
dragonfly::minimal_distance(switch_id src, switch_id dst) const
{
  int srcA, srcG; get_coords(src, srcA, srcG);
  int dstA, dstG; get_coords(dst, dstA, dstG);

  if (srcG == dstG) return 1;

  std::vector<int> connected;
  int directConn = -1;
  group_wiring_->connected_routers(srcA, srcG, connected);
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
dragonfly::setup_port_params(sprockit::sim_parameters* params, int red, int port_offset, int num_ports) const
{
  sprockit::sim_parameters* link_params = params->get_namespace("link");
  double bw = link_params->get_bandwidth_param("bandwidth");
  int bufsize = params->get_optional_byte_length_param("buffer_size", 0);

  double port_bw = bw * red;
  int credits = bufsize * red;

  for (int i=0; i < num_ports; ++i){
    int port = port_offset + i;
    sprockit::sim_parameters* port_params = topology
        ::setup_port_params(port, credits, port_bw, link_params, params);
  }
}

void
dragonfly::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  int max_num_conns = (a_ - 1) + h_;
  conns.resize(max_num_conns);

  int myA;
  int myG;
  get_coords(src, myA, myG);

  int cidx = 0;

  for (int a = 0; a < a_; a++){
    if (a != myA){
      switch_id dst = get_uid(a, myG);
      connection& conn = conns[cidx++];
      conn.src = src;
      conn.dst = dst;
      conn.src_outport = a;
      conn.dst_inport = myA;
    }
  }

  std::vector<int> groupConnections;
  group_wiring_->connected_routers(myA, myG, groupConnections);
  for (int h = 0; h < groupConnections.size(); ++h){
    switch_id dst = groupConnections[h];
    int dstG = computeG(dst);
    if (dstG != myG){
      connection& conn = conns[cidx++];
      conn.src = src;
      conn.dst = dst;
      conn.src_outport = h + a_;
      int dstA = computeA(dst);
      conn.dst_inport = group_wiring_->input_group_port(myA, myG, h, dstA, dstG) + a_;

      top_debug("dragonfly (%d,%d:%d)->(%d,%d:%d)",
         myA, myG, conn.src_outport, dstA, dstG, conn.dst_inport);

    }
  }
  conns.resize(cidx);
}

void
dragonfly::configure_individual_port_params(switch_id src, sprockit::sim_parameters *switch_params) const
{
  setup_port_params(switch_params, red_[0], 0, a_);
  setup_port_params(switch_params, red_[1], a_, h_);
}

inter_group_wiring::inter_group_wiring(sprockit::sim_parameters *params, int a, int g, int h) :
  a_(a), g_(g), h_(h)
{
}

switch_id
inter_group_wiring::random_intermediate(router* rtr, switch_id current_sw, switch_id dest_sw, uint32_t seed)
{
  int srcA = current_sw % g_;
  int srcG = current_sw / g_;
  int dstA = dest_sw % g_;
  int dstG = dest_sw / g_;
  switch_id sid = current_sw;
  uint32_t attempt = 0;
  if (srcG == dstG){
    int interA = srcA;
    while (interA == srcA || interA == dstA){
      interA = rtr->random_number(a_, attempt, seed);
      ++attempt;
    }
    return srcG*a_ + interA;
  } else {
    int interA = rtr->random_number(a_, attempt, seed);
    int interG = srcG;
    while (interG == srcG || interG == dstG){
      interG = rtr->random_number(g_, attempt, seed);
      ++attempt;
    }
    return interG*a_ + interA;
  }
}

static inline int mod(int a, int b){
  int rem = a % b;
  return rem < 0 ? rem + b : rem;
}

class single_link_group_wiring : public inter_group_wiring
{
  FactoryRegister("single", inter_group_wiring, single_link_group_wiring)
 public:
  single_link_group_wiring(sprockit::sim_parameters* params, int a, int g, int h) :
    inter_group_wiring(params, a, g, h)
  {
    if (h_ != 1){
      spkt_abort_printf("h must be 1 for single link inter-group pattern");
    }
  }

  int input_group_port(int srcA, int srcG, int srcH, int dstA, int dstG) const override {
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
  void connected_routers(int srcA, int srcG, std::vector<int>& connected) const override {
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
    switch_id dst = dstG*a_ + dstA;
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
  void connected_to_group(int srcG, int dstG, std::vector<std::pair<int,int>>& connected) const override {
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

class circulant_group_wiring : public inter_group_wiring
{
  FactoryRegister("circulant", inter_group_wiring, circulant_group_wiring)
 public:
  circulant_group_wiring(sprockit::sim_parameters* params, int a, int g, int h) :
    inter_group_wiring(params, a, g, h)
  {
    if (h_ % 2){
      spkt_abort_printf("group connections must be even for circulant inter-group pattern");
    }
  }

  int input_group_port(int srcA, int srcG, int srcH, int dstA, int dstG) const override {
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
  void connected_routers(int srcA, int srcG, std::vector<int>& connected) const override {
    connected.clear();
    int dstA = srcA;
    int half = h_  /2;
    for (int h=1; h <= half; ++h){
      int plusG = mod(srcG + srcA*half + h, g_);
      if (plusG != srcG){
        switch_id dst = plusG*a_ + dstA;
        connected.push_back(dst); //full switch ID
      }
      int minusG = mod(srcG - srcA*half - h, g_);
      if (minusG != srcG){
        switch_id dst = minusG*a_ + dstA;
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
  void connected_to_group(int srcG, int dstG, std::vector<std::pair<int,int>>& connected) const override {
    connected.clear();
    std::vector<int> tmp;
    for (int a=0; a < a_; ++a){
      connected_routers(a, srcG, tmp);
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

class alltoall_group_wiring : public inter_group_wiring
{
  FactoryRegister("alltoall", inter_group_wiring, alltoall_group_wiring)
 public:
  alltoall_group_wiring(sprockit::sim_parameters* params, int a, int g, int h) :
    inter_group_wiring(params, a, g, h)
  {
    covering_ = h_ / (g_-1);
    stride_ = a_ / covering_;

    top_debug("alltoall links cover groups %dx with stride=%d", covering_, stride_);

    if (h_ % (g_-1)) spkt_abort_printf("dragonfly #groups-1=%d must evenly divide #group_connections=%d", g_-1, h_);
    if (a_ % covering_) spkt_abort_printf("dragonfly covering=%d must evenly divide group_size=%d", covering_, a_);
  }

  int input_group_port(int srcA, int srcG, int srcH, int dstA, int dstG) const override {
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
  void connected_routers(int srcA, int srcG, std::vector<int>& connected) const override {
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

  switch_id random_intermediate(router* rtr, switch_id current_sw, switch_id dest_sw, uint32_t seed) override
  {
    int srcG = current_sw / a_;
    int dstG = dest_sw / a_;
    uint32_t attempt = 0;
    if (srcG == dstG){
      return inter_group_wiring::random_intermediate(rtr, current_sw, dest_sw, seed);
    } else {
      int srcA = current_sw % a_;
      int interG = rtr->random_number(g_, attempt, seed);
      while (interG == srcG || interG == dstG){
        interG = rtr->random_number(g_, attempt, seed);
      }
      //go to a router directly connected to srcA
      int srcH = rtr->random_number(h_, attempt, seed);
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
  void connected_to_group(int srcG, int dstG, std::vector<std::pair<int,int>>& connected) const override {
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
