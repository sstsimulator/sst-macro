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
);

namespace sstmac {
namespace hw {

static const double PI = 3.141592653589793238462;

dragonfly_plus::dragonfly_plus(sprockit::sim_parameters* params) :
  dragonfly(params)
{
  max_ports_intra_network_ = a_ + h_;
  eject_geometric_id_ = max_ports_intra_network_;

  if (h_ % (g_-1)){
    spkt_abort_printf("dragonfly+ currently requires an all-to-all group connectivity");
  }

  num_leaf_switches_ = a_*g_;
}

void
dragonfly_plus::minimal_route_to_switch(
  int& path_rotater,
  switch_id src,
  switch_id dst,
  packet::path &path) const
{

}

int
dragonfly_plus::minimal_distance(switch_id src, switch_id dst) const
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

void
dragonfly_plus::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  conns.clear();

  int myRow;
  int myA;
  int myG;
  get_coords(src, myRow, myA, myG);

  if (myRow == 0){
    for (int a=0; a < a_; ++a){
      connection next;
      next.src = src;
      next.src_outport = a;
      next.dst = get_uid(1, a, myG);
      next.dst_inport = myA;
      top_debug("(%d=%d,%d:%d)->(%d=%d,%d:%d)",
                src, myA, myG, next.src_outport,
                next.dst, a, myG, next.dst_inport);
      conns.push_back(next);
    }
  } else {
    for (int a=0; a < a_; ++a){ //down to leaf
      connection next;
      next.src = src;
      next.src_outport = a;
      next.dst = get_uid(0, a, myG);
      next.dst_inport = myA;
      top_debug("(%d=%d,%d:%d)->(%d=%d,%d:%d)",
                src, myA, myG, next.src_outport,
                next.dst, a, myG, next.dst_inport);
      conns.push_back(next);
    }

    std::vector<int> newConns;
    group_wiring_->connected_routers(myA, myG, newConns);
    for (int p=0; p < newConns.size(); ++p){
      int relDst = newConns[p];
      connection next;
      next.src = src;
      next.src_outport = p + a_;
      next.dst = relDst + a_*g_; //num leaf switches
      int dstA = relDst % a_;
      int dstG = relDst / a_;
      next.dst_inport =  a_ + group_wiring_->input_group_port(myA, myG, p, dstA, dstG);
      conns.push_back(next);
      top_debug("(%d=%d,%d:%d)->(%d=%d,%d:%d)",
                src, myA, myG, next.src_outport,
                next.dst, dstA, dstG, next.dst_inport);
    }
  }
}

void
dragonfly_plus::configure_individual_port_params(switch_id src, sprockit::sim_parameters *switch_params) const
{
  int row = src / num_leaf_switches();
  if (row == 0){
    dragonfly::setup_port_params(switch_params, red_[0], 0, a_);
  } else {
    dragonfly::setup_port_params(switch_params, red_[0], 0, a_);
    dragonfly::setup_port_params(switch_params, red_[1], a_, h_);
  }


}

}
} //end of namespace sstmac
