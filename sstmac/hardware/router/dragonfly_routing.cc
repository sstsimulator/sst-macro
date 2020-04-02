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

#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/hardware/topology/dragonfly_plus.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <cmath>

#define ftree_rter_debug(...) \
  rter_debug("fat tree: %s", sprockit::sprintf(__VA_ARGS__).c_str())

RegisterKeywords(
 {"static", "whether to statically route ports or dynamically rotate paths"},
);

namespace sstmac {
namespace hw {

struct DragonflyMinimalRouter : public Router {
  SST_ELI_REGISTER_DERIVED(
    Router,
    DragonflyMinimalRouter,
    "macro",
    "dragonfly_minimal",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "router implementing minimal dragonfly")

  struct header : public Packet::Header {
    uint8_t num_group_hops : 2;
    uint8_t num_hops : 4;
  };

 public:
  DragonflyMinimalRouter(SST::Params& params, Topology* top,
                           NetworkSwitch* netsw) :
    Router(params, top, netsw)
  {
    dfly_ = dynamic_cast<Dragonfly*>(top);
    if (!dfly_){
      spkt_abort_printf("dragonfly router can only be used with dragonfly topology");
    }

    static_route_ = params.find<bool>("static", false);

    my_a_ = dfly_->computeA(my_addr_);
    my_g_ = dfly_->computeG(my_addr_);

    group_ports_.resize(dfly_->g());
    group_port_rotaters_.resize(dfly_->g());


    std::set<int> directGroupConnections;

    //figure out which groups I have a direct connection to
    std::vector<int> connections;
    dfly_->groupWiring()->connectedRouters(my_a_, my_g_, connections);
    for (int c=0; c < connections.size(); ++c){
      SwitchId dst = connections[c];
      int dstG = dfly_->computeG(dst);
      if (dstG != my_g_){
        group_ports_[dstG].push_back(c + dfly_->a());
        directGroupConnections.insert(dstG);
      }
    }

    std::vector<std::pair<int,int>> groupConnections;
    for (int g=0; g < dfly_->g(); ++g){
      if (g == my_g_) continue;
      if (directGroupConnections.find(g) != directGroupConnections.end()) continue;

      dfly_->groupWiring()->connectedToGroup(my_g_, g, groupConnections);
      if (groupConnections.size() == 0){
        spkt_abort_printf("Got zero group connections from %d->%d", my_g_, g);
      }
      for (auto& pair : groupConnections){
        group_ports_[g].push_back(pair.first);
      }
    }

    for (int i=0; i < group_ports_.size(); ++i){
      if (!group_ports_[i].empty()){
        group_port_rotaters_[i] = my_addr_ % group_ports_[i].size();
      }
    }

  }

  int numVC() const override {
    return 2;
  }

  std::string toString() const override {
    return "dragonfly minimal router";
  }

  void route(Packet *pkt) override
  {
    auto* hdr = pkt->rtrHeader<header>();
    SwitchId ejaddr = pkt->toaddr() / dfly_->concentration();
    if (ejaddr == my_addr_){
      int port = pkt->toaddr() % dfly_->concentration();
      hdr->edge_port = dfly_->a() + dfly_->h() + port;
      hdr->deadlock_vc = 0;
      return;
    }

    routeToSwitch(pkt, ejaddr);
  }

  void routeToSwitch(Packet* pkt, SwitchId ej_addr)
  {
    auto hdr = pkt->rtrHeader<header>();
    hdr->deadlock_vc = hdr->num_group_hops;
    int dstG = dfly_->computeG(ej_addr);
    if (dstG == my_g_){
      int dstA = dfly_->computeA(ej_addr);
      hdr->edge_port = dstA;
    } else {
      int dst_port;
      if (static_route_){
        int rotater = ej_addr % group_ports_[dstG].size();
        dst_port = group_ports_[dstG][rotater];
      } else {
        dst_port = group_ports_[dstG][group_port_rotaters_[dstG]];
        group_port_rotaters_[dstG] = (group_port_rotaters_[dstG] + 1) % group_ports_[dstG].size();
      }
      if (dst_port >= dfly_->a()){
        hdr->num_group_hops++;
      }
      if (dst_port >= (dfly_->a() + dfly_->h())){
        spkt_abort_printf("Got bad group port %d going to group %d from switch=(%d,%d)",
                          dst_port, dstG, my_a_, my_g_);
      }
      hdr->edge_port = dst_port;
    }
  }

 protected:
  Dragonfly* dfly_;

  std::vector<std::vector<int>> group_ports_;
  std::vector<int> group_port_rotaters_;

  bool static_route_;

  int my_g_;
  int my_a_;
};

class DragonflyValiantRouter : public DragonflyMinimalRouter {
 public:
  SST_ELI_REGISTER_DERIVED(
    Router,
    DragonflyValiantRouter,
    "macro",
    "dragonfly_valiant",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "router implementing valiant dragonfly")

  static const char initial_stage = 0;
  static const char valiant_stage = 1;
  static const char final_stage = 2;

  struct header : public DragonflyMinimalRouter::header {
    uint8_t stage_number : 3;
    uint32_t dest_switch : 24;
  };

  DragonflyValiantRouter(SST::Params& params, Topology *top,
                         NetworkSwitch *netsw)
    : DragonflyMinimalRouter(params, top, netsw)
  {
    group_gateways_.resize(dfly_->g());
    gateway_rotater_.resize(dfly_->g());

    std::vector<int> connected;
    for (int a=0; a < dfly_->a(); ++a){
      if (a != my_a_){
        dfly_->groupWiring()->connectedRouters(a, my_g_, connected);
        for (int sid : connected){
          int dst_g = dfly_->computeG(sid);
          group_gateways_[dst_g].emplace_back(a, sid);
        }
      }
    }

    for (int i=0; i < gateway_rotater_.size(); ++i){
      if (i != my_g_){
        if (group_gateways_[i].empty()){
          std::vector<int> connected;
          //find me a router in every group that has a connection
          for (int inter_g=0; inter_g < dfly_->g(); ++inter_g){
            if (inter_g == my_g_ || inter_g == i) continue;

            int dest = -1;
            for (int a=0; a < dfly_->a(); ++a){
              int aa = (a+my_a_) % dfly_->a(); //scatter for diff switches
              dfly_->groupWiring()->connectedRouters(aa, inter_g, connected);
              for (int sid : connected){
                int grp = dfly_->computeG(sid);
                if (grp == i){
                  dest = dfly_->getUid(a, inter_g);
                  break;
                }
              }
            }
            if (dest == -1){
              spkt_abort_printf("Group %d has no extra-hop valiant connections to group %d",
                                inter_g, i);
            }
            auto& ports = group_ports_[inter_g];
            int my_port = ports[my_a_ % ports.size()];
            group_gateways_[i].emplace_back(my_port, dest);
          }
        }
        gateway_rotater_[i] = my_addr_ % group_gateways_[i].size();
      }
    }
  }

  std::string toString() const override {
    return "dragonfly valiant";
  }

  int numVC() const override {
    return 6;
  }

  void checkValiantInterGroup(Packet* pkt, int dst_g)
  {
    uint32_t seed = netsw_->now().time.ticks();
    uint32_t attempt = 0;
    int new_g = dst_g;
    while (new_g == my_g_ || new_g == dst_g){
      new_g = randomNumber(dfly_->g(), attempt++, seed);
    }
    auto hdr = pkt->rtrHeader<header>();
    auto val_dest = group_gateways_[new_g][gateway_rotater_[new_g]];
    hdr->edge_port = val_dest.first;
    hdr->dest_switch = val_dest.second;
    gateway_rotater_[new_g] = (gateway_rotater_[new_g] + 1) % group_gateways_[new_g].size();
    hdr->stage_number = valiant_stage;
  }

  void checkValiantIntraGroup(Packet* pkt, int dst_a){
    uint32_t seed = netsw_->now().time.ticks();
    uint32_t attempt = 0;
    int new_a = dst_a;
    while (new_a == my_a_ || new_a == dst_a){
      new_a = randomNumber(dfly_->a(), attempt++, seed);
    }

    auto hdr = pkt->rtrHeader<header>();
    hdr->edge_port = new_a;
    hdr->dest_switch = dfly_->getUid(new_a, my_g_);
    hdr->stage_number = valiant_stage;
  }

  void route(Packet *pkt) override {
    auto hdr = pkt->rtrHeader<header>();
    SwitchId ej_addr = pkt->toaddr() / dfly_->concentration();
    if (ej_addr == my_addr_){
      hdr->edge_port = dfly_->a() + dfly_->h() + pkt->toaddr() % dfly_->concentration();
      hdr->deadlock_vc = 0;
      return;
    }

    switch(hdr->stage_number){
      case initial_stage: {
        int dst_a = dfly_->computeA(ej_addr);
        int dst_g = dfly_->computeG(ej_addr);
        if (dst_g == my_g_){
          checkValiantIntraGroup(pkt, dst_a);
        } else {
          checkValiantInterGroup(pkt, dst_g);
        }
        hdr->stage_number = valiant_stage;
        break;
      }
      case valiant_stage: {
        if (my_addr_ == hdr->dest_switch){
          hdr->stage_number = final_stage;
        } else {
          routeToSwitch(pkt, hdr->dest_switch);
          break;
        }
      }
      case final_stage: {
        routeToSwitch(pkt, ej_addr);
        hdr->dest_switch = ej_addr;
        break;
      }
      break;
    }
    hdr->deadlock_vc = hdr->num_hops;
    ++hdr->num_hops;
  }

 protected:
  std::vector<int> gateway_rotater_; //for non-minimal
  std::vector<std::vector<std::pair<int,int>>> group_gateways_;
};

class DragonflyUGALRouter : public DragonflyValiantRouter {

 public:
  static const char minimal_only_stage = final_stage + 1;

  SST_ELI_REGISTER_DERIVED(
    Router,
    DragonflyUGALRouter,
    "macro",
    "dragonfly_ugal",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "router implementing UGAL dragonfly")

  DragonflyUGALRouter(SST::Params& params, Topology *top,
                      NetworkSwitch *netsw)
    : DragonflyValiantRouter(params, top, netsw)
  {
    val_threshold_ = params.find<int>("val_threshold", 0);
    vl_queues_ = params.find<bool>("vl_queues", false);
  }

  std::string toString() const override {
    return "dragonfly ugal router";
  }

  int numVC() const override {
    return 6;
  }

  void checkUGALInterGroup(Packet* pkt, int dst_a, int dst_g, char minimal_stage)
  {
    uint32_t seed = netsw_->now().time.ticks();
    uint32_t attempt = 0;
    int new_g = dst_g;
    auto hdr = pkt->rtrHeader<header>();
    int min_port = group_ports_[dst_g][group_port_rotaters_[dst_g]];
    if (dfly_->g() > 2){ //can't do this unless I have an intermediate group!
      while (new_g == my_g_ || new_g == dst_g){
        new_g = randomNumber(dfly_->g(), attempt++, seed);
      }
      int min_dist = 3;
      int val_dist = 5;

      auto val_dest = group_gateways_[new_g][gateway_rotater_[new_g]];

      int vl = vl_queues_ ? pkt->qos() + vl_offset_ : all_vcs;
      bool go_valiant = switchPaths(min_dist, val_dist, min_port, val_dest.first, vl);
      if (go_valiant){
        hdr->edge_port = val_dest.first;
        hdr->dest_switch = val_dest.second;
        gateway_rotater_[new_g] = (gateway_rotater_[new_g] + 1) % group_gateways_[new_g].size();
        hdr->stage_number = valiant_stage;
        rter_debug("chose inter-grp ugal port %d to intermediate %d : pkt=%p:%s",
                   val_dest.first, val_dest.second, pkt, pkt->toString().c_str());
        return;
      }
    } 
    //if reached here, did not go valiant
    hdr->edge_port = min_port;
    hdr->dest_switch = dfly_->getUid(dst_a,dst_g);
    gateway_rotater_[dst_g] = (gateway_rotater_[dst_g] + 1) % group_gateways_[dst_g].size();
    hdr->stage_number = minimal_stage;
    rter_debug("chose inter-grp minimal port %d: pkt=%p:%s",
               min_port, pkt, pkt->toString().c_str());
    group_port_rotaters_[dst_g] = (group_port_rotaters_[dst_g] + 1) % group_ports_[dst_g].size();
  }

  void checkUGALIntraGroup(Packet* pkt, int dst_a, char minimal_stage){
    uint32_t seed = netsw_->now().time.ticks();
    uint32_t attempt = 0;
    int new_a = dst_a;
    while (new_a == my_a_ || new_a == dst_a){
      new_a = randomNumber(dfly_->a(), attempt++, seed);
    }
    int min_dist = 1;
    int val_dist = 2;

    auto hdr = pkt->rtrHeader<header>();
    int vl = vl_queues_ ? pkt->qos() + vl_offset_ : all_vcs;
    bool go_valiant = switchPaths(min_dist, val_dist, dst_a, new_a, vl);
    if (go_valiant){
      hdr->edge_port = new_a;
      hdr->dest_switch = dfly_->getUid(new_a, my_g_);
      hdr->stage_number = valiant_stage;
      rter_debug("chose intra-grp ugal port %d to intermediate %d : pkt=%p:%s",
                 new_a, int(hdr->dest_switch), pkt, pkt->toString().c_str());
    } else { //minimal
      hdr->edge_port = dst_a;
      hdr->dest_switch = dfly_->getUid(dst_a, my_g_);
      hdr->stage_number = minimal_stage;
      rter_debug("chose intra-grp minimal port %d: pkt=%p:%s",
                 dst_a, pkt, pkt->toString().c_str());
    }
  }

  void route(Packet *pkt) override {
    auto hdr = pkt->rtrHeader<header>();
    SwitchId ej_addr = pkt->toaddr() / dfly_->concentration();
    if (ej_addr == my_addr_){
      int port = pkt->toaddr() % dfly_->concentration();
      hdr->edge_port = dfly_->a() + dfly_->h() + port;
      hdr->deadlock_vc = 0;
      return;
    }

    switch(hdr->stage_number){
      case initial_stage: {
        int dst_a = dfly_->computeA(ej_addr);
        int dst_g = dfly_->computeG(ej_addr);
        if (dst_g == my_g_){
          checkUGALIntraGroup(pkt, dst_a, minimal_only_stage);
        } else {
          checkUGALInterGroup(pkt, dst_a, dst_g, minimal_only_stage);
        }
        break;
      }
      case minimal_only_stage: {
        //don't reconsider my decision
        routeToSwitch(pkt, ej_addr);
        rter_debug("continue to minimal %d on port %d: pkt=%p:%s",
                 ej_addr, int(hdr->edge_port), pkt, pkt->toString().c_str());
        break;
      }
      case valiant_stage: {
        if (my_addr_ == hdr->dest_switch){
          hdr->stage_number = final_stage;
        } else {
          routeToSwitch(pkt, hdr->dest_switch);
          rter_debug("route to valiant intermediate %d on port %d: pkt=%p:%s",
                     int(hdr->dest_switch), int(hdr->edge_port),
                     pkt, pkt->toString().c_str());
          break;
        }
      }
      case final_stage: {
        hdr->dest_switch = ej_addr;
        routeToSwitch(pkt, ej_addr);
        rter_debug("route to final %d on port %d: pkt=%p:%s",
                   ej_addr, int(hdr->edge_port),  pkt, pkt->toString().c_str());
        break;
      }
      break;
    }
    hdr->deadlock_vc = hdr->num_hops;
    if (hdr->deadlock_vc >= numVC()){
      spkt_abort_printf("Packet %p:%s too many hops", pkt, pkt->toString().c_str());
    }
    ++hdr->num_hops;
  }

 protected:
  int val_threshold_;
  bool vl_queues_;

};

class DragonflyPARRouter : public DragonflyUGALRouter {
 public:
  SST_ELI_REGISTER_DERIVED(
    Router,
    DragonflyPARRouter,
    "macro",
    "dragonfly_par",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "router implementing PAR dragonfly")

  std::string toString() const override {
    return "dragonfly PAR router";
  }

  DragonflyPARRouter(SST::Params& params, Topology *top,
                       NetworkSwitch *netsw)
    : DragonflyUGALRouter(params, top, netsw)
  {
  }

  void route(Packet *pkt) override {
    auto hdr = pkt->rtrHeader<header>();
    SwitchId ej_addr = pkt->toaddr() / dfly_->concentration();
    if (ej_addr == my_addr_){
      int port = pkt->toaddr() % dfly_->concentration();
      hdr->edge_port = dfly_->a() + dfly_->h() + port;
      hdr->deadlock_vc = 0;
      return;
    }

    switch(hdr->stage_number){
      case initial_stage: {
        int dst_a = dfly_->computeA(ej_addr);
        int dst_g = dfly_->computeG(ej_addr);
        if (dst_g == my_g_){
          checkUGALIntraGroup(pkt, dst_a, initial_stage);
        } else {
          checkUGALInterGroup(pkt, dst_a, dst_g, initial_stage);
        }
        break;
      }
      case valiant_stage: {
        if (my_addr_ == hdr->dest_switch){
          hdr->stage_number = final_stage;
        } else {
          routeToSwitch(pkt, hdr->dest_switch);
          break;
        }
      }
      case final_stage: {
        routeToSwitch(pkt, ej_addr);
        hdr->dest_switch = ej_addr;
        break;
      }
      break;
    }
    hdr->deadlock_vc = hdr->num_hops;
    ++hdr->num_hops;
  }

};

struct DragonflyScatterRouter : public Router {
  SST_ELI_REGISTER_DERIVED(
    Router,
    DragonflyScatterRouter,
    "macro",
    "dragonfly_scatter",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "router implementing dragonfly that obliviously scatters traffic")

  struct header : public Packet::Header {
    uint8_t num_group_hops : 2;
    uint8_t num_hops : 4;
  };

  static const int max_hops = 3;

  struct destination {
    std::set<int>::iterator rotater;
    std::set<int> ports;

    int nextPort(){
      int port = *rotater; //ports[rotater];
      ++rotater;
      if (rotater == ports.end()){
        rotater = ports.begin();
      }
      return port;
    }

    void init(){
      rotater = ports.begin();
    }

    destination(){}
  };

 public:
  DragonflyScatterRouter(SST::Params& params, Topology* top,
                           NetworkSwitch* netsw) :
    Router(params, top, netsw)
  {
    dfly_ = dynamic_cast<Dragonfly*>(top);
    if (!dfly_){
      spkt_abort_printf("dragonfly router can only be used with dragonfly topology");
    }

    my_a_ = dfly_->computeA(my_addr_);
    my_g_ = dfly_->computeG(my_addr_);

    for (int i=1; i <= max_hops; ++i){
      destination_table_[i].resize(dfly_->numSwitches());
    }

    followPath(my_addr_, 0, 0);

    for (int sid=0; sid < dfly_->numSwitches(); ++sid){
      for (int i=1; i <= max_hops; ++i){
        destination& d = destination_table_[i][sid];
        d.init();
        for (int port : d.ports){
          rter_debug("adding %d-hop path to %4d on port %d",
                      i, sid, port);
        }
      }
    }
  }

  void followPathHelper(int sid, int num_hops, int num_group_hops, int port){
    int g = dfly_->computeG(sid);
    if (port < dfly_->a() || g != my_g_){
      //don't follow inter-grp ports for intra-grp sends
      for (int h=num_hops; h <= 3; ++h){
        destination_table_[h][sid].ports.insert(port);
        if (port == my_a_){
          spkt_abort_printf("adding invalid port %d in path to %d",
                            port, sid);
        }
      }
    }
    followPath(sid, num_hops, num_group_hops, port);
  }

  void followPath(int sid, int num_hops, int num_group_hops, int port = -1){
    if (num_hops < 3){
      int a = dfly_->computeA(sid);
      int g = dfly_->computeG(sid);
      if (num_group_hops == 0){
        std::vector<int> connections;
        dfly_->groupWiring()->connectedRouters(a, g, connections);
        for (int c=0; c < connections.size(); ++c){
          int next_port = port == -1 ? c + dfly_->a() : port;
          int next_sid = connections[c];
          followPathHelper(next_sid, num_hops+1, num_group_hops+1, next_port);
        }
      }
      for (int aa=0; aa < dfly_->a(); ++aa){
        if (aa != a){
          int next_sid = dfly_->getUid(aa, g);
          int next_port = port == -1 ? aa : port;
          followPathHelper(next_sid, num_hops+1, num_group_hops, next_port);
        }
      }
    }
  }

  int numVC() const override {
    return 3;
  }

  std::string toString() const override {
    return "dragonfly scatter router";
  }

  void route(Packet *pkt) override
  {
    auto* hdr = pkt->rtrHeader<header>();
    SwitchId ejaddr = pkt->toaddr() / dfly_->concentration();
    if (ejaddr == my_addr_){
      int port = pkt->toaddr() % dfly_->concentration();
      hdr->edge_port = dfly_->a() + dfly_->h() + port;
      hdr->deadlock_vc = 0;
      return;
    }

    int hops = hdr->num_hops;
    int dst_g = dfly_->computeG(ejaddr);
    SwitchId injaddr = pkt->fromaddr() / dfly_->concentration();
    int src_g = dfly_->computeG(injaddr);
    /**

    if (dst_g == my_g_){
      int dst_a = dfly_->computeA(ejaddr);
      hdr->edge_port = dst_a;
    } else {
    */
    int max_hops = src_g == dst_g ? 2 : 3;
    int allowed_hops = max_hops - hops;
    destination& d = destination_table_[allowed_hops][ejaddr];
#if SSTMAC_SANITY_CHECK
    if (d.ports.empty()){
      spkt_abort_printf("Router %d: packet has no path to destination %d from source %d: %s",
                        my_addr_, ejaddr, injaddr, pkt->toString().c_str());
    }
#endif
    int port = d.nextPort();
    hdr->edge_port = port;
    if (port > dfly_->a()){
      //group port
      hdr->num_group_hops++;
    }
    hdr->deadlock_vc = hdr->num_hops;
    hdr->num_hops++;
  }

 protected:
  Dragonfly* dfly_;

  std::vector<destination> destination_table_[max_hops+1];

  int my_g_;
  int my_a_;
};

struct DragonflyRotateRouter : public DragonflyMinimalRouter {
  SST_ELI_REGISTER_DERIVED(
    Router,
    DragonflyRotateRouter,
    "macro",
    "dragonfly_rotate",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "router implementing dragonfly that obliviously scatters traffic")

 public:
  DragonflyRotateRouter(SST::Params& params, Topology* top,
                        NetworkSwitch* netsw) :
    DragonflyMinimalRouter(params, top, netsw)
  {
    std::vector<int> connections;
    dfly_->groupWiring()->connectedRouters(my_a_, my_g_, connections);
    for (int c=0; c < connections.size(); ++c){
      SwitchId dst = connections[c];
      int dstG = dfly_->computeG(dst);
      if (dstG != my_g_){
        int port = c + dfly_->a();
        rter_debug("adding valid group port %d", port);
        valid_ports_.insert(port);
      }
    }

    port_rotater_ = valid_ports_.begin();
  }

  int numVC() const override {
    return 4;
  }

  std::string toString() const override {
    return "dragonfly rotate router";
  }

  void route(Packet *pkt) override
  {
    auto* hdr = pkt->rtrHeader<header>();
    SwitchId ejaddr = pkt->toaddr() / dfly_->concentration();
    if (ejaddr == my_addr_){
      int port = pkt->toaddr() % dfly_->concentration();
      hdr->edge_port = dfly_->a() + dfly_->h() + port;
      hdr->deadlock_vc = 0;
      return;
    }

#if SSTMAC_SANITY_CHECK
    if (hdr->num_hops > 3){
      spkt_abort_printf("Took too many hops on %s going to %d",
                        pkt->toString().c_str(), ejaddr);
    }
#endif

    int dst_g = dfly_->computeG(ejaddr);
    if (hdr->num_group_hops >= 1){
      DragonflyMinimalRouter::routeToSwitch(pkt, ejaddr);
    } else {
      if (dst_g == my_g_){
        int dst_a = dfly_->computeA(ejaddr);
        hdr->edge_port = dst_a;
      } else {
        hdr->edge_port = *port_rotater_;
        hdr->num_group_hops++;
        ++port_rotater_;
        if (port_rotater_ == valid_ports_.end()){
          port_rotater_ = valid_ports_.begin();
        }
      }
    }
    hdr->deadlock_vc = hdr->num_hops;
    hdr->num_hops++;
  }

 private:
  std::set<int>::iterator port_rotater_;
  std::set<int> valid_ports_;
};

}
}
