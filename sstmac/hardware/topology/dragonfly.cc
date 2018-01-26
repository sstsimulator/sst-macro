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
  true_random_intermediate_ = params->get_optional_bool_param("true_random_intermediate",
                                      false);

  //can never have more group connections than groups
  if (h_ >= g_){
    cerr0 << sprockit::printf("WARNING: requested %d group connections, "
                        "but max should be %d for %d groups\n",
                        h_, g_-1, g_);
  }

  max_ports_intra_network_ = a_ + h_;
  eject_geometric_id_ = max_ports_intra_network_;

  group_wiring_ = inter_group_wiring::factory::get_optional_param("inter_group", "circulant", params, this);
}

void
dragonfly::configure_geometric_paths(std::vector<int> &redundancies)
{
  spkt_abort_printf("not implemented: dragonfly::configure geometric paths");
}

switch_id
dragonfly::random_intermediate_switch(switch_id current_sw, switch_id dest_sw)
{
  long nid = current_sw;
  uint32_t attempt = 0;
  int srcA = computeA(current_sw);
  int srcG = computeG(current_sw);
  int dstA = computeA(dest_sw);
  int dstG = computeG(dest_sw);
  while (current_sw == nid) {
    dstA = random_number(a_, attempt);
    if (dstG != srcG){
      dstG = random_number(g_, attempt);
    } 
    nid = get_uid(dstA, dstG);
    ++attempt;
  }
  return switch_id(nid);
}

void
dragonfly::minimal_route_to_switch(
    switch_id src,
    switch_id dst,
    routable::path &path) const
{
  path.vc = path.metadata_bit(routable::crossed_timeline) ? 1 : 0;

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
  group_wiring_->connected_to_group(srcG, dstG, connected);
  int srcRotater = srcA % connected.size();

  path.set_outport(connected[srcRotater]);
  path.set_metadata_bit(routable::crossed_timeline);
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
  int bufsize = params->get_byte_length_param("buffer_size");

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

void
dragonfly::configure_vc_routing(std::map<routing::algorithm_t, int>& m) const
{
  m[routing::minimal] = 2;
  m[routing::minimal_adaptive] = 2;
  m[routing::valiant] = 6;
  m[routing::ugal] = 6;
}

void
dragonfly::new_routing_stage(routable* rtbl)
{
  rtbl->current_path().unset_metadata_bit(routable::crossed_timeline);
}

inter_group_wiring::inter_group_wiring(sprockit::sim_parameters *params, dragonfly* top) :
  a_(top->a()), g_(top->g()), h_(top->h())
{
}

static inline int mod(int a, int b){
  int rem = a % b;
  return rem < 0 ? rem + b : rem;
}

class circulant_group_wiring : public inter_group_wiring
{
  FactoryRegister("circulant", inter_group_wiring, circulant_group_wiring)
 public:
  circulant_group_wiring(sprockit::sim_parameters* params, dragonfly* top) :
    inter_group_wiring(params, top)
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
   *                  that have connections to a router in group dstG
   * @return The number of routers in group srcG with connections to dstG
   */
  void connected_to_group(int srcG, int dstG, std::vector<int>& connected) const override {
    connected.clear();
    std::vector<int> tmp;
    for (int a=0; a < a_; ++a){
      connected_routers(a, srcG, tmp);
      for (int c=0; c < tmp.size(); ++c){
        int g = tmp[c] / a_;
        if (dstG == g){
          connected.push_back(a);
          break;
        }
      }
    }
  }

};


}
} //end of namespace sstmac
