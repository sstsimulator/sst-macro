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

#include <sstmac/hardware/router/fat_tree_router.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <cmath>

#define ftree_rter_debug(...) \
  rter_debug("fat tree: %s", sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

fat_tree_router::fat_tree_router(sprockit::sim_parameters* params, topology *top,
                                 network_switch *netsw)
//:  minimal_router(params, top, netsw),
//  rng_(nullptr)
{
//  ftree_ = safe_cast(fat_tree, top);
//  k_ = ftree_->k();
//  l_ = ftree_->l();
//  seed_ = params->get_optional_long_param("router_seed", -1);
//  build_rng();

//  int switchesperlevel = pow(k_, l_ - 1);
//  myL_ = my_addr_ / switchesperlevel;

//  num_leaf_switches_reachable_ = pow(k_, myL_);
//  num_leaf_switches_per_path_ = num_leaf_switches_reachable_ / k_;
//  int level_relative_id = my_addr_ - myL_ * switchesperlevel;

//  int my_leaf_group = level_relative_id / num_leaf_switches_reachable_;
//  min_reachable_leaf_id_ = my_leaf_group * num_leaf_switches_reachable_;
//  max_reachable_leaf_id_ = min_reachable_leaf_id_ + num_leaf_switches_reachable_;

//  numpicked_ = 0;
//  numpicktop_ = 0;

  ft_ = safe_cast(fat_tree, top);
}

void route(packet* pkt) {

  int output_port;
  packet::path& path = pkt->current_path();
  switch_id dst = find_ejection_site(pkt->toaddr(), path);

  // already there
  if (dst == my_addr_){
    path.vc = 0;
    rter_debug("Ejecting %s from switch %d on port %d",
               pkt->to_string().c_str(), sid, path.outport());
  }

  // have to route
  else {
    int my_level = ft_->level(my_addr_);
    int dst_level = ft_->level(dst);
    int my_tree = ft_->subtree(my_addr_);
    int dst_tree = ft_->subtree(dst);

    //definitely have to go up
    if (dst_level >= my_level){
      int next_tree;
      if (my_level == 0) {
        next_tree = my_tree;
      }
      else if (my_level == 1) {
        next_tree = ft_->core_subtree();
      }
      output_port = get_up_port(next_tree);
      path.set_outport(output_port);
      path.vc = 0;
      top_debug("fat_tree: routing up to get to s=%d,l=%d from s=%d,l=%d",
                int(dst), dst_level, int(my_addr_), src_level);
    }

    // definitely have to go down
    else if (my_level == 2){
      output_port = get_core_down_port(dst_tree);
      path.set_outport(output_port);
      path.vc = 0;
      top_debug("fat_tree: routing down to get to s=%d,l=%d from s=%d,l=%d",
                int(dst), dst_level, int(my_addr_), src_level);
    }

    // aggregator level, can go either way
    else if (my_level == 1){
      // in the right tree, going down
      if (dst_tree == my_tree) {
        output_port = get_agg_down_port(dst);
        path.set_outport(output_port);
        path.vc = 0;
        top_debug("fat_tree: routing down to get to s=%d,l=%d from s=%d,l=%d",
                  int(dst), dst_level, int(my_addr_), src_level);
      }
      //nope, have to go to core to hop over to other tree
      else {
        int next_tree = ft_->core_subtree();
        output_port = get_up_port(next_tree);
        path.set_outport(output_port);
        path.vc = 0;
        top_debug("fat_tree: routing up to get to s=%d,l=%d from s=%d,l=%d",
                  int(dst), dst_level, int(my_addr_), src_level);
      }
    }

    rter_debug("Routing %s to switch %d on port %d",
               pkt->to_string().c_str(), sid, path.outport());
  }

}

void
fat_tree_router::rotate_subtree_next(int tree) {
  ++subtree_next_[tree];
  if (subtree_next_[tree] >= subtree_fwd_[tree].size())
    subtree_next_ = 0;
}

void
fat_tree_router::rotate_leaf_next(int leaf) {
  ++leaf_next_[leaf];
  if (leaf_next_[leaf] >= leaf_fwd_[leaf].size())
    leaf_next_ = 0;
}

int
fat_tree_router::get_up_port(int next_tree) {
  int port;
  auto ports = subtree_fwd_.find(next_tree);
  if (ports == subtree_fwd_.end()) {
    // haven't forwarded to subtree yet
    subtree_fwd_.insert(make_pair(next_tree,std::vector<int>));
    ft_->connected_up_ports(my_addr_,subtree_fwd_[next_tree]);
    subtree_next_[next_tree] = 0;
    ports = subtree_fwd_.find(next_tree);
    if (ports = subtree_fwd_.end()) {
      // TODO error
    }
  }
  port = ports->[subtree_next_[next_tree]];
  rotate_subtree_next(next_tree);
  return port;
}

int
fat_tree_router::get_core_down_port(int next_tree) {
  int port;
  auto ports = subtree_fwd_.find(next_tree);
  if (ports == subtree_fwd_.end()) {
    // haven't forwarded to subtree yet
    subtree_fwd_.insert(make_pair(next_tree,std::vector<int>));
    ft_->connected_core_down_ports(my_addr_,next_tree,subtree_fwd_[next_tree]);
    subtree_next_[next_tree] = 0;
    ports = subtree_fwd_.find(next_tree);
    if (ports = subtree_fwd_.end()) {
      // TODO error
    }
  }
  port = ports->[subtree_next_[next_tree]];
  rotate_subtree_next(next_tree);
  return port;
}

int
fat_tree_router::get_agg_down_port(int dst_leaf) {
  int port;
  auto ports = leaf_fwd_.find(dst_leaf);
  if (ports == leaf_fwd_.end()) {
    // haven't forwarded to this leaf yet
    leaf_fwd_.insert(make_pair(dst_leaf,std::vector<int>));
    ft_->connected_agg_down_ports(my_addr_,dst_leaf,leaf_fwd_[dst_leaf]);
    leaf_next_[dst_leaf] = 0;
    ports = leaf_fwd_.find(dst_leaf);
    if (ports = leaf_fwd_.end()) {
      // TODO error
    }
  }
  port = ports->[leaf_next_[dst_leaf]];
  rotate_leaf_next(dst_leaf);
  return port;
}

}
}
