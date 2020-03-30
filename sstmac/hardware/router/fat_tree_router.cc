/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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
  rter_debug("fat tree: %s", sprockit::sprintf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

FatTreeRouter::FatTreeRouter(
    SST::Params& params,
    Topology *top,
    NetworkSwitch *netsw) :
  Router(params, top, netsw)
{
  ft_ = safe_cast(FatTree, top);
  if (my_addr_ >= (ft_->numLeafSwitches() + ft_->numAggSwitches())){
    my_row_ = 2;
    num_up_ports_ = 0;
  } else if (my_addr_ >= (ft_->numLeafSwitches())){
    my_row_ = 1;
    num_up_ports_ = ft_->upPortsPerAggSwitch();
    firstUpPort_ = ft_->firstUpPort(my_addr_);
    my_tree_ = ft_->subtree(my_addr_);
    up_next_ = my_addr_ % num_up_ports_;
  } else {
    my_row_ = 0;
    my_tree_ = ft_->subtree(my_addr_);
    num_up_ports_ = ft_->upPortsPerLeafSwitch();
    firstUpPort_ = ft_->firstUpPort(my_addr_);
    up_next_ = my_addr_ % num_up_ports_;
  }

  if (my_row_ == 2){
    down_routes_.resize(ft_->numAggSubtrees());
    std::vector<Topology::Connection> conns;
    ft_->connectedOutports(my_addr_, conns);
    for (Topology::Connection& conn : conns){
      int subtree = (conn.dst - ft_->numLeafSwitches()) / ft_->aggSwitchesPerSubtree();
      down_routes_[subtree].push_back(conn.src_outport);
    }
  } else if (my_row_ == 1){
    down_routes_.resize(ft_->leafSwitchesPerSubtree());
    std::vector<Topology::Connection> conns;
    ft_->connectedOutports(my_addr_, conns);
    for (Topology::Connection& conn : conns){
      if (conn.dst < my_addr_){
        int leaf = conn.dst % ft_->leafSwitchesPerSubtree();
        down_routes_[leaf].push_back(conn.src_outport);
      }
    }
  }

  down_rotaters_.resize(down_routes_.size());
  for (int i=0; i < down_routes_.size(); ++i){
    //scatter across switches
    down_rotaters_[i] = my_addr_ % down_routes_[i].size();
  }
}

void
FatTreeRouter::route(Packet* pkt) {
  header* hdr = pkt->rtrHeader<header>();
  SwitchId dst = pkt->toaddr() / ft_->concentration();

  // already there
  if (dst == my_addr_){
    hdr->deadlock_vc = 0;
    hdr->edge_port = pkt->toaddr() % ft_->concentration() + ft_->upPortsPerLeafSwitch();
    rter_debug("Ejecting %s from switch %d on port %d",
               pkt->toString().c_str(), dst, int(hdr->edge_port));
  } else { // have to route
    int dst_tree = ft_->subtree(dst);
    if (my_row_ == 0){ //leat switch - going up
      //definitely have to go up since we didn't eject
      hdr->edge_port = getUpPort();
      hdr->deadlock_vc = 0;
      rter_debug("fat_tree: routing up to get to s=%d through l=1 from s=%d,l=0",
                int(dst), int(my_addr_));
    } else if (my_row_ == 2){     // definitely have to go down
      hdr->edge_port = getDownPort(dst_tree);
      hdr->deadlock_vc = 0;
      rter_debug("fat_tree: routing down to get to s=%d through l=1 from s=%d,l=2",
                int(dst), int(my_addr_));
    } else if (my_row_ == 1){ // aggregator level, can go either way
      // in the right tree, going down
      if (dst_tree == my_tree_) {
        int dst_leaf = dst % ft_->leafSwitchesPerSubtree();
        hdr->edge_port = getDownPort(dst_leaf);
        hdr->deadlock_vc = 0;
        rter_debug("fat_tree: routing down to get to s=%d,l=0 from s=%d,l=1",
                  int(dst), int(my_addr_));
      } else { //nope, have to go to core to hop over to other tree
        hdr->edge_port = getUpPort();
        hdr->deadlock_vc = 0;
        rter_debug("fat_tree: routing up to get to s=%d through l=2 from s=%d,l=1",
                  int(dst), int(my_addr_));
      }
    } else {
        spkt_abort_printf("Got bad level=%d on switch %d", my_row_, my_addr_);
    }
    rter_debug("Routing %s to switch %d on port %d",
               pkt->toString().c_str(), int(dst), int(hdr->edge_port));
  }
}

// up is easy -- any "up" port goes up
int
FatTreeRouter::getUpPort() {
  int port = firstUpPort_ + up_next_;
  up_next_ = (up_next_ + 1) % num_up_ports_;
  return port;
}

int
FatTreeRouter::getDownPort(int path)
{
  auto& routes = down_routes_[path];
  int port = routes[down_rotaters_[path]];
  int nroutes = routes.size();
  down_rotaters_[path] = (down_rotaters_[path]+1) % nroutes;
  return port;
}

class TaperedFatTreeMinimalRouter : public Router {
 public:
  SST_ELI_REGISTER_DERIVED(
    Router,
    TaperedFatTreeMinimalRouter,
    "macro",
    "tapered_fat_tree_minimal",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "router implementing fat tree routing")

  struct header : public Packet::Header {};

  TaperedFatTreeMinimalRouter(SST::Params& params, Topology *top,
                         NetworkSwitch *netsw)
    : Router(params, top, netsw)
  {
    tree_ = safe_cast(TaperedFatTree, top);
  }

  std::string toString() const override {
    return "tapered fat tree minimal router";
  }

  int numVC() const override {
    return 1;
  }

  void minimalRoute(SwitchId dst, Packet::Header* hdr){
    int src_level = tree_->level(my_addr_);
    int dst_level = tree_->level(dst);
    //question is whether I go up or down
    if (dst_level >= src_level){ //definitely have to go up
      hdr->edge_port = tree_->upPort(src_level);
      hdr->deadlock_vc = 0;
      top_debug("fat_tree: routing up to get to s=%d,l=%d from s=%d,l=%d",
                int(dst), dst_level,
                int(my_addr_), src_level);
    } else if (src_level == 2){
      //definitely have to go down
      int dst_subtree = dst_level == 0 ? tree_->injSubtree(dst) : tree_->aggSubtree(my_addr_);
      hdr->edge_port = tree_->downPort(dst_subtree);
      hdr->deadlock_vc = 0;
      top_debug("fat_tree: routing down to get to s=%d,l=%d from s=%d,l=%d on port %d",
                int(dst), dst_level, int(my_addr_), src_level, int(hdr->edge_port));
    } else if (src_level == 1){
      //going to level 0, but may have to go up or down to get there
      int my_tree = tree_->aggSubtree(my_addr_);
      int dst_tree = tree_->injSubtree(dst);
      if (dst_tree == my_tree){
        //okay, great, I should have direct link
        hdr->edge_port = dst % tree_->leafSwitchesPerSubtree();
        hdr->deadlock_vc = 0;
        top_debug("fat_tree: routing down to get to s=%d,l=%d from s=%d,l=%d on port %d within tree %d",
                  int(dst), dst_level, int(my_addr_), src_level, int(hdr->edge_port), my_tree);
      } else {
        //nope, have to go to core to hope over to other tree
        hdr->edge_port = tree_->upPort(src_level);
        hdr->deadlock_vc = 0;
        top_debug("fat_tree: routing up to get to s=%d,l=%d from s=%d,l=%d hopping from tree %d to tree %d",
                        int(dst), dst_level, int(my_addr_), src_level, my_tree, dst_tree);
      }
    }
  }

  void route(Packet *pkt) override {
    auto* hdr = pkt->rtrHeader<header>();
    SwitchId ej_addr = pkt->toaddr() / tree_->concentration();
    if (ej_addr == my_addr_){
      hdr->edge_port = pkt->toaddr() % tree_->concentration();
      hdr->deadlock_vc = 0;
      return;
    }

    minimalRoute(ej_addr, hdr);
  }

 private:
  TaperedFatTree* tree_;

};


}
}
