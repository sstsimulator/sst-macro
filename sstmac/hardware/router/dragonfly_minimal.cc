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

#include <sstmac/hardware/router/minimal_routing.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/hardware/topology/dragonfly_plus.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <cmath>

#define ftree_rter_debug(...) \
  rter_debug("fat tree: %s", sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

struct dragonfly_minimal_router : public minimal_router {
  FactoryRegister("dragonfly_minimal", router, dragonfly_minimal_router)

  struct header : public packet::header {
    uint8_t num_group_hops : 2;
    uint8_t num_hops : 4;
  };

 public:
  dragonfly_minimal_router(sprockit::sim_parameters* params, topology* top,
                           network_switch* netsw) :
    minimal_router(params, top, netsw)
  {
    dfly_ = dynamic_cast<dragonfly*>(top);
    if (!dfly_){
      spkt_abort_printf("dragonfly router can only be used with dragonfly topology");
    }

    switch_id sid = addr();
    int myA = dfly_->computeA(sid);
    myG_ = dfly_->computeG(sid);
    a_ = dfly_->a();

    group_ports_.resize(dfly_->g());

    std::vector<std::pair<int,int>> groupConnections;
    for (int g=0; g < dfly_->g(); ++g){
      if (g == myG_) continue;

      dfly_->group_wiring()->connected_to_group(myG_, g, groupConnections);
      if (groupConnections.size() == 0){
        spkt_abort_printf("Got zero group connections from %d->%d", myG_, g);
      }
      int rotater = myA % groupConnections.size();
      group_ports_[g] = groupConnections[rotater].first;
    }

    //figure out which groups I have a direct connection to
    std::vector<int> connections;
    dfly_->group_wiring()->connected_routers(myA, myG_, connections);
    for (int c=0; c < connections.size(); ++c){
      switch_id dst = connections[c];
      int dstG = dfly_->computeG(dst);
      group_ports_[dstG] = c + dfly_->a();
    }
  }

  int num_vc() const override {
    return 2;
  }

  std::string to_string() const override {
    return "dragonfly minimal router";
  }

  void route_to_switch(switch_id ej_addr, packet* pkt) override
  {
    packet::path& path = pkt->current_path();
    auto hdr = pkt->get_header<header>();
    path.vc = hdr->num_group_hops;
    int dstG = dfly_->computeG(ej_addr);
    if (dstG == myG_){
      int dstA = dfly_->computeA(ej_addr);
      path.set_outport(dstA);
    } else {
      int dst_port = group_ports_[dstG];
      if (dst_port >= a_){
        hdr->num_group_hops++;
      }
      path.set_outport(dst_port);
    }
  }

 private:
  dragonfly* dfly_;
  std::vector<int> group_ports_;
  int myG_;
  int a_;
};

class dragonfly_plus_minimal_router : public minimal_router {
 public:
  FactoryRegister("dragonfly_plus_minimal",
              router, dragonfly_plus_minimal_router,
              "router implementing minimal routing for dragonfly+")

  dragonfly_plus_minimal_router(sprockit::sim_parameters* params, topology *top,
                         network_switch *netsw)
    : minimal_router(params, top, netsw)
  {
    dfly_ = safe_cast(dragonfly_plus, top);
    num_leaf_switches_ = dfly_->g() * dfly_->a();
    //stagger by switch id
    rotater_ = (my_addr_) % dfly_->a();
  }

  int num_vc() const override {
    return 1;
  }

  std::string to_string() const override {
    return "dragonfly+ minimal router";
  }

 private:
  void route_to_switch(switch_id sid, packet* pkt) override {
    int srcGrp = (my_addr_%num_leaf_switches_) / dfly_->a();
    int dstGrp = (sid%num_leaf_switches_) / dfly_->a();
    int srcRow = my_addr_ / num_leaf_switches_;
    auto& path = pkt->current_path();
    if (srcRow == 0){
      path.set_outport(rotater_);
      rotater_ = (rotater_ + 1) % dfly_->a();
    } else if (srcGrp == dstGrp){
      int dstA = sid % dfly_->a();
      path.set_outport(dstA);
    } else {
      int outport = srcGrp < dstGrp ? (dstGrp - 1) : dstGrp;
      path.set_outport(dfly_->a() + outport);
    }
    path.vc = 0;
  }

  int num_leaf_switches_;
  int rotater_;
  dragonfly_plus* dfly_;
};


}
}
