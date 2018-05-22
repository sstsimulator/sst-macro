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

#include <sstmac/hardware/router/fat_tree_router.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <cmath>

using namespace std;

#define ftree_rter_debug(...) \
  rter_debug("fat tree: %s", sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

fat_tree_router::fat_tree_router(
    sprockit::sim_parameters* params,
    topology *top,
    network_switch *netsw) :
  router(params, top, netsw)
{
  ft_ = safe_cast(fat_tree, top);
}

void
fat_tree_router::route(packet* pkt) {

  int output_port;
  packet::path& path = pkt->current_path();
  switch_id dst = find_ejection_site(pkt->toaddr(), path);

  // already there
  if (dst == my_addr_){
    path.vc = 0;
    rter_debug("Ejecting %s from switch %d on port %d",
               pkt->to_string().c_str(), dst, path.outport());
  }

  // have to route
  else {
    int my_level = ft_->level(my_addr_);
    int dst_level = ft_->level(dst);
    int my_tree = ft_->subtree(my_addr_);
    int dst_tree = ft_->subtree(dst);

    //definitely have to go up
    if (dst_level >= my_level){
      output_port = get_up_port();
      path.set_outport(output_port);
      path.vc = 0;
      rter_debug("fat_tree: routing up to get to s=%d,l=%d from s=%d,l=%d",
                int(dst), dst_level, int(my_addr_), my_level);
    }

    // definitely have to go down
    else if (my_level == 2){
      output_port = get_core_down_port(dst_tree);
      path.set_outport(output_port);
      path.vc = 0;
      rter_debug("fat_tree: routing down to get to s=%d,l=%d from s=%d,l=%d",
                int(dst), dst_level, int(my_addr_), my_level);
    }

    // aggregator level, can go either way
    else if (my_level == 1){
      // in the right tree, going down
      if (dst_tree == my_tree) {
        output_port = get_agg_down_port(dst);
        path.set_outport(output_port);
        path.vc = 0;
        rter_debug("fat_tree: routing down to get to s=%d,l=%d from s=%d,l=%d",
                  int(dst), dst_level, int(my_addr_), my_level);
      }
      //nope, have to go to core to hop over to other tree
      else {
        int next_tree = ft_->core_subtree();
        output_port = get_up_port();
        path.set_outport(output_port);
        path.vc = 0;
        rter_debug("fat_tree: routing up to get to s=%d,l=%d from s=%d,l=%d",
                  int(dst), dst_level, int(my_addr_), my_level);
      }
    }

    rter_debug("Routing %s to switch %d on port %d",
               pkt->to_string().c_str(), int(dst), path.outport());
  }

}

void
fat_tree_router::rotate_up_next() {
  ++up_next_;
  if (up_next_ >= up_fwd_.size())
    up_next_ = 0;
}

void
fat_tree_router::rotate_subtree_next(int tree) {
  ++subtree_next_[tree];
  if (subtree_next_[tree] >= subtree_fwd_[tree].size())
    subtree_next_[tree] = 0;
}

void
fat_tree_router::rotate_leaf_next(int leaf) {
  ++leaf_next_[leaf];
  if (leaf_next_[leaf] >= leaf_fwd_[leaf].size())
    leaf_next_[leaf] = 0;
}

// up is easy -- any "up" port goes up
int
fat_tree_router::get_up_port() {
  if (up_fwd_.size() == 0) {
    // haven't forwarded up yet
    int n_up = ft_->num_up_ports(my_addr_);
    int first_up = ft_->first_up_port(my_addr_);
    for (int i=0; i<n_up; ++i)
      up_fwd_.push_back(first_up + i);
      up_next_ = 0;
  }
  int port = up_fwd_[up_next_];
  rotate_up_next();
  return port;
}

// going down from core
// we can use any port that puts us on the correct subtree
int
fat_tree_router::get_core_down_port(int next_tree) {
  auto ports = subtree_fwd_.find(next_tree);
  if (ports == subtree_fwd_.end()) {
    // haven't forwarded to subtree yet
    ft_->connected_core_down_ports(
          my_addr_,next_tree,subtree_fwd_[next_tree]);
    subtree_next_[next_tree] = 0;
    ports = subtree_fwd_.find(next_tree);
    if (ports == subtree_fwd_.end())
      spkt_throw_printf(sprockit::value_error,
            "can't find subtree forwarding table");
  }
  int port = ports->second.at(subtree_next_[next_tree]);
  rotate_subtree_next(next_tree);
  return port;
}

// going down from aggregator
// we can use any port that puts us on the correct switch
// (possibly but not necessarily redundant links)
int
fat_tree_router::get_agg_down_port(int dst_leaf) {
  auto ports = leaf_fwd_.find(dst_leaf);
  if (ports == leaf_fwd_.end()) {
    // haven't forwarded to this leaf yet
    ft_->connected_agg_down_ports(
          my_addr_,dst_leaf,leaf_fwd_[dst_leaf]);
    leaf_next_[dst_leaf] = 0;
    ports = leaf_fwd_.find(dst_leaf);
    if (ports == leaf_fwd_.end())
      spkt_throw_printf(sprockit::value_error,
                        "can't find leaf forwarding table");
  }
  int port = ports->second.at(leaf_next_[dst_leaf]);
  rotate_leaf_next(dst_leaf);
  return port;
}

}
}
