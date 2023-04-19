/**
Copyright 2009-2023 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2023, NTESS

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

#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/router/router.h>

namespace sstmac {
namespace hw {

class XpressRing :
  public StructuredTopology
{
 public:
  typedef enum {
    up_port = 0,
    down_port = 1,
    jump_up_port = 2,
    jump_down_port = 3
  } port_t;

 public:
   SPKT_REGISTER_DERIVED(
    Topology,
    XpressRing,
    "macro",
    "xpress",
    "A ring topology with express cables that make large jumps")

  XpressRing(SST::Params& params)
    : StructuredTopology(params)
  {
    ring_size_ = params.find<int>("xpress_ring_size");
    jump_size_ = params.find<int>("xpress_jump_size");
  }

  virtual ~XpressRing() {}

  std::string toString() const override {
    return "xpress ring topology";
  }

  void connectedOutports(SwitchId src,
        std::vector<Topology::Connection>& conns) const override {
    conns.resize(4); //+1/-1 conns, +jump,-jump conns
    conns[0].src = src;
    conns[0].dst = (src+1) % ring_size_;
    conns[0].src_outport = up_port;
    conns[0].dst_inport = down_port;

    conns[1].src = src;
    conns[1].dst = (src+ring_size_ - 1) % ring_size_;
    conns[1].src_outport = down_port;
    conns[1].dst_inport = up_port;

    conns[2].src = src;
    conns[2].dst = (src+jump_size_) % ring_size_;
    conns[2].src_outport = jump_up_port;
    conns[2].dst_inport = jump_down_port;

    conns[3].src = src;
    conns[3].dst = (src-jump_size_+ring_size_) % ring_size_;
    conns[3].src_outport = jump_down_port;
    conns[3].dst_inport = jump_up_port;
  }

  void endpointsConnectedToInjectionSwitch(SwitchId swid,
               std::vector<InjectionPort> &nodes) const override {
    nodes.resize(concentration());
    for (int i=0; i < concentration(); ++i){
      InjectionPort& port = nodes[i];
      port.ep_port = 0;
      port.switch_port = 4 + i;
      port.nid = concentration() * swid + i;
    }
  }

  int upDistance(SwitchId src, SwitchId dest) const {
    if (dest > src) return dest - src;
    else return ring_size_ - src + dest;
  }

  int downDistance(SwitchId src, SwitchId dest) const {
    if (src > dest) return src - dest;
    else return src + ring_size_ - dest;
  }

  int numHopsToNode(NodeId src_node, NodeId dest_node) const override {
    SwitchId src = src_node / concentration_;
    SwitchId dest = dest_node / concentration_;

    int up_distance = upDistance(src,dest);
    int down_distance = downDistance(src,dest);

    int total_distance = std::min(up_distance, down_distance);
    return numHopsForDistance(total_distance);
  }

  SwitchId numLeafSwitches() const override {
    return ring_size_;
  }

  int maxNumPorts() const override {
    return 4 + concentration();
  }

  SwitchId numSwitches() const override {
    return ring_size_;
  }

  int jumpSize() const {
    return jump_size_;
  }

  int diameter() const override {
    //half-way around the ring is the biggest
    int halfway = ring_size_ / 2;
    return numHopsForDistance(halfway);
  }


 private:
  int numHopsForDistance(int total_distance) const {
    int num_jumps = total_distance / jump_size_;
    int num_steps = total_distance % jump_size_;
    int half_jump = jump_size_ / 2;
    if (num_steps > half_jump) {
      //take an extra jump
      ++num_jumps;
      num_steps = jump_size_ - num_steps;
    }
    return num_jumps + num_steps;
  }

  int ring_size_;

  int jump_size_;

};

class XpressRouter : public Router {
  struct header : public Packet::Header {
    char crossed_timeline : 1;
  };
 public:
   SST_ELI_REGISTER_DERIVED(
    Router,
    XpressRouter,
    "macro",
    "xpress",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "a routing algorithm for minimal routing on the Xpress ring")

   XpressRouter(SST::Params& params,
                Topology* top, NetworkSwitch* netsw)
    : Router(params, top, netsw)
  {
    ring_ = dynamic_cast<XpressRing*>(top);
  }

  int numVC() const override {
    return 2;
  }

  std::string toString() const override {
    return "Xpress router";
  }

  void route(Packet* pkt) override {
    auto* hdr = pkt->rtrHeader<header>();
    constexpr int inj_offset = 4;
    SwitchId ej_addr = pkt->toaddr() / ring_->concentration();
    SwitchId my_addr = addr();
    if (ej_addr == my_addr){
      hdr->edge_port = pkt->toaddr() % ring_->concentration() + inj_offset;
      hdr->deadlock_vc = 0;
      return;
    }

    int up_dist = ring_->upDistance(my_addr, ej_addr);
    int down_dist = ring_->downDistance(my_addr, ej_addr);
    if (up_dist == ring_->jumpSize()){
      //we are directly connected through the jump up port
      hdr->edge_port = ring_->jump_up_port;
    } else if (up_dist == 1){
      //we are directly connected through the regular port
      hdr->edge_port = ring_->up_port;
    } else if (down_dist == ring_->jumpSize()){
      //we are directly connected through the jump down port
      hdr->edge_port = ring_->jump_down_port;
    } else if (down_dist == 1){
      //we are directly connected through the regular port
      hdr->edge_port = ring_->down_port;
    } else {
      if (up_dist <= down_dist){
        if (up_dist > ring_->jumpSize()){
          hdr->edge_port = ring_->jump_up_port;
        } else {
          hdr->edge_port = ring_->up_port;
        }
      } else {
        if (down_dist > ring_->jumpSize()){
          hdr->edge_port = ring_->jump_down_port;
        } else {
          hdr->edge_port = ring_->down_port;
        }
      }
    }
    switch(hdr->edge_port){
      case XpressRing::jump_up_port: {
        int dest = (my_addr + ring_->jumpSize());
        if (dest >= ring_->numLeafSwitches()){
          hdr->crossed_timeline = 1;
        }
        break;
      }
      case XpressRing::up_port: {
        if (my_addr == (ring_->numLeafSwitches()-1)){
          hdr->crossed_timeline = 1;
        }
        break;
      }
      case XpressRing::jump_down_port: {
        if (my_addr < ring_->jumpSize()){
          hdr->crossed_timeline = 1;
        }
        break;
      }
      case XpressRing::down_port: {
        if (my_addr == 0){
          hdr->crossed_timeline = 1;
          hdr->deadlock_vc = 1;
        }
        break;
      }
    }
    //if wrap around, we have to break cyclic dependencies with VCs
    hdr->deadlock_vc = hdr->crossed_timeline ? 1 : 0;
  }

 private:
  XpressRing* ring_;
};

}
}
