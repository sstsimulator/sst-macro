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

  max_ports_intra_network_ = a_ + g_;
  eject_geometric_id_ = max_ports_intra_network_;

  group_wiring_ = inter_group_wiring::factory::get_optional_param("inter_group", "consecutive", params, this);
}

void
dragonfly::configure_geometric_paths(std::vector<int> &redundancies)
{
  spkt_abort_printf("not implemented: dragonfly::configure geometric paths");
}

switch_id
dragonfly::random_intermediate_switch(switch_id current_sw, switch_id dest_sw)
{
  spkt_abort_printf("random selection not implemented");
  return 0;
}

void
dragonfly::minimal_route_to_switch(
    switch_id src,
    switch_id dst,
    routable::path &path) const
{
  path.vc = path.metadata_bit(routable::crossed_timeline) ? 1 : 0;

  int srcG = computeG(src);
  int dstG = computeG(dst);
  int dstA = computeA(dst);
  if (srcG == dstG){
    path.set_outport(dstA);
    return;
  }

  int connected[32];
  int numConns = group_wiring_->connected_to_group(srcG, dstG, connected);

  int srcA = computeA(src);
  int srcRotater = srcA % h_;

  path.set_outport(connected[srcRotater]);
}

int
dragonfly::minimal_distance(switch_id src, switch_id dst) const
{
  int srcA, srcG; get_coords(src, srcA, srcG);
  int dstA, dstG; get_coords(dst, dstA, dstG);

  if (srcG == dstG) return 1;

  int connected[32];
  int directConn = -1;
  int numConns = group_wiring_->connected_routers(srcA, srcG, connected);
  for (int i=0; i < numConns; ++i){
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
    switch_id dst = get_uid(a, myG);
    connection& conn = conns[cidx++];
    conn.src = src;
    conn.dst = dst;
    conn.src_outport = a;
    conn.dst_inport = myA;
  }

  int groupConnections[64];
  int numConns = group_wiring_->connected_routers(myA, myG, groupConnections);
  for (int h = 0; h < numConns; ++h){
    switch_id dst = groupConnections[h];
    connection& conn = conns[cidx++];
    conn.src = src;
    conn.dst = dst;
    conn.src_outport = h + a_;
    int dstA = computeA(dst);
    int dstG = computeG(dst);
    conn.dst_inport = group_wiring_->group_port(dstA, dstG, myG) + a_;
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
  m[routing::valiant] = 3;
  m[routing::ugal] = 3;
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

class consecutive_group_wiring : public inter_group_wiring
{
  FactoryRegister("consecutive", inter_group_wiring, consecutive_group_wiring)
 public:
  consecutive_group_wiring(sprockit::sim_parameters* params, dragonfly* top) :
    inter_group_wiring(params, top)
  {
  }

  int group_port(int srcA, int srcG, int dstG) const override {
    int dstH = dstG - srcA*h_;
    if (dstG < srcG){
      return dstH;
    } else {
      dstH--;
      return dstH;
    }
  }

  /**
   * @brief connected_routers
   * @param a The src router index within the group
   * @param g The src router group
   * @param connected [in-out] The routers (switch id) for each inter-group interconnection
   * @return The number of routers in connected array
   */
  int connected_routers(int a, int g, int connected[]) const override {
    for (int h=0; h < h_; ++h){
      int dstG = a*h_ + h;
      int dstA = 0;
      if (dstG < g){
        dstA = (g - 1) / h_;
      } else {
        dstA = g / h_;
      }
      connected[h] = dstG*a_ + dstA;
    }
    return h_;
  }

  /**
   * @brief connected_to_group
   * @param srcG
   * @param dstG
   * @param connected [in-out] The list of all intra-group routers in range (0 ... a-1)
   *                  that have connections to a router in group dstG
   * @return The number of routers in group srcG with connections to dstG
   */
  int connected_to_group(int srcG, int dstG, int connected[]) const override {
    if (dstG >= srcG){
      dstG--; //mapping formula changes depending on relationship between src/dst
    }
    for (int h=0; h < h_; ++h){
      int srcA = (dstG - h) / h_;
      connected[h] = srcA;
    }
    return h_;
  }
};


}
} //end of namespace sstmac
