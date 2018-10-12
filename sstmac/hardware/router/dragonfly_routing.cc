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
  rter_debug("fat tree: %s", sprockit::printf(__VA_ARGS__).c_str())

RegisterKeywords(
 {"static", "whether to statically route ports or dynamically rotate paths"},
);

namespace sstmac {
namespace hw {

struct dragonfly_minimal_router : public router {
  FactoryRegister("dragonfly_minimal", router, dragonfly_minimal_router)

  struct header : public packet::header {
    uint8_t num_group_hops : 2;
    uint8_t num_hops : 4;
  };

 public:
  dragonfly_minimal_router(sprockit::sim_parameters* params, topology* top,
                           network_switch* netsw) :
    router(params, top, netsw)
  {
    dfly_ = dynamic_cast<dragonfly*>(top);
    if (!dfly_){
      spkt_abort_printf("dragonfly router can only be used with dragonfly topology");
    }

    static_route_ = params->get_optional_bool_param("static", false);

    my_a_ = dfly_->computeA(my_addr_);
    my_g_ = dfly_->computeG(my_addr_);

    group_ports_.resize(dfly_->g());
    group_port_rotaters_.resize(dfly_->g());


    std::set<int> directGroupConnections;

    //figure out which groups I have a direct connection to
    std::vector<int> connections;
    dfly_->group_wiring()->connected_routers(my_a_, my_g_, connections);
    for (int c=0; c < connections.size(); ++c){
      switch_id dst = connections[c];
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

      dfly_->group_wiring()->connected_to_group(my_g_, g, groupConnections);
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

  int num_vc() const override {
    return 2;
  }

  std::string to_string() const override {
    return "dragonfly minimal router";
  }

  void route(packet *pkt) override
  {
    switch_id ejaddr = pkt->toaddr() / dfly_->concentration();
    if (ejaddr == my_addr_){
      int port = pkt->toaddr() % dfly_->concentration();
      pkt->current_path().set_outport(dfly_->a() + dfly_->h() + port);
      pkt->current_path().vc = 0;
      return;
    }

    route_to_switch(pkt, ejaddr);
  }

  void route_to_switch(packet* pkt, switch_id ej_addr)
  {
    packet::path& path = pkt->current_path();
    auto hdr = pkt->get_header<header>();
    path.vc = hdr->num_group_hops;
    int dstG = dfly_->computeG(ej_addr);
    if (dstG == my_g_){
      int dstA = dfly_->computeA(ej_addr);
      path.set_outport(dstA);
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
      path.set_outport(dst_port);
    }
  }

 protected:
  dragonfly* dfly_;

  std::vector<std::vector<int>> group_ports_;
  std::vector<int> group_port_rotaters_;

  bool static_route_;

  int my_g_;
  int my_a_;
};

class dragonfly_valiant_router : public dragonfly_minimal_router {
 public:
  static const char initial_stage = 0;
  static const char valiant_stage = 1;
  static const char final_stage = 2;

  struct header : public dragonfly_minimal_router::header {
    uint8_t stage_number : 3;
  };

  FactoryRegister("dragonfly_valiant",
              router, dragonfly_valiant_router,
              "router implementing valint routing for dragonfly")

  dragonfly_valiant_router(sprockit::sim_parameters* params, topology *top,
                           network_switch *netsw)
    : dragonfly_minimal_router(params, top, netsw)
  {
    group_gateways_.resize(dfly_->g());
    gateway_rotater_.resize(dfly_->g());

    std::vector<int> connected;
    for (int a=0; a < dfly_->a(); ++a){
      if (a != my_a_){
        dfly_->group_wiring()->connected_routers(a, my_g_, connected);
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
              dfly_->group_wiring()->connected_routers(aa, inter_g, connected);
              for (int sid : connected){
                int grp = dfly_->computeG(sid);
                if (grp == i){
                  dest = dfly_->get_uid(a, inter_g);
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

  std::string to_string() const override {
    return "dragonfly valiant";
  }

  int num_vc() const override {
    return 6;
  }

  void check_valiant_inter_group(packet* pkt, int dst_g)
  {
    uint32_t seed = netsw_->now().ticks();
    uint32_t attempt = 0;
    int new_g = dst_g;
    while (new_g == my_g_ || new_g == dst_g){
      new_g = random_number(dfly_->g(), attempt++, seed);
    }
    auto hdr = pkt->get_header<header>();
    auto val_dest = group_gateways_[new_g][gateway_rotater_[new_g]];
    pkt->current_path().set_outport(val_dest.first);
    pkt->set_dest_switch(val_dest.second);
    gateway_rotater_[new_g] = (gateway_rotater_[new_g] + 1) % group_gateways_[new_g].size();
    hdr->stage_number = valiant_stage;
  }

  void check_valiant_intra_group(packet* pkt, int dst_a){
    uint32_t seed = netsw_->now().ticks();
    uint32_t attempt = 0;
    int new_a = dst_a;
    while (new_a == my_a_ || new_a == dst_a){
      new_a = random_number(dfly_->a(), attempt++, seed);
    }

    auto hdr = pkt->get_header<header>();
    pkt->current_path().set_outport(new_a);
    pkt->set_dest_switch(dfly_->get_uid(new_a, my_g_));
    hdr->stage_number = valiant_stage;
  }

  void route(packet *pkt) override {
    switch_id ej_addr = pkt->toaddr() / dfly_->concentration();
    if (ej_addr == my_addr_){
      int port = pkt->toaddr() % dfly_->concentration();
      pkt->current_path().set_outport(dfly_->a() + dfly_->h() + port);
      pkt->current_path().vc = 0;
      return;
    }

    auto hdr = pkt->get_header<header>();
    switch(hdr->stage_number){
      case initial_stage: {
        int dst_a = dfly_->computeA(ej_addr);
        int dst_g = dfly_->computeG(ej_addr);
        if (dst_g == my_g_){
          check_valiant_intra_group(pkt, dst_a);
        } else {
          check_valiant_inter_group(pkt, dst_g);
        }
        hdr->stage_number = valiant_stage;
        break;
      }
      case valiant_stage: {
        if (my_addr_ == pkt->dest_switch()){
          hdr->stage_number = final_stage;
        } else {
          route_to_switch(pkt, pkt->dest_switch());
          break;
        }
      }
      case final_stage: {
        route_to_switch(pkt, ej_addr);
        pkt->set_dest_switch(ej_addr);
        break;
      }
      break;
    }
    pkt->current_path().vc = hdr->num_hops;
    ++hdr->num_hops;
  }

 protected:
  std::vector<int> gateway_rotater_; //for non-minimal
  std::vector<std::vector<std::pair<int,int>>> group_gateways_;
};

class dragonfly_ugal_router : public dragonfly_valiant_router {

 public:
  static const char minimal_only_stage = final_stage + 1;
  FactoryRegister("dragonfly_ugal",
              router, dragonfly_ugal_router,
              "router implementing UGAL routing for dragonfly")

  dragonfly_ugal_router(sprockit::sim_parameters* params, topology *top,
                                 network_switch *netsw)
    : dragonfly_valiant_router(params, top, netsw)
  {
    val_threshold_ = params->get_optional_int_param("val_threshold", 0);
  }

  std::string to_string() const override {
    return "dragonfly ugal router";
  }

  int num_vc() const override {
    return 6;
  }

  void check_ugal_inter_group(packet* pkt, int dst_a, int dst_g, char minimal_stage)
  {
    uint32_t seed = netsw_->now().ticks();
    uint32_t attempt = 0;
    int new_g = dst_g;
    while (new_g == my_g_ || new_g == dst_g){
      new_g = random_number(dfly_->g(), attempt++, seed);
    }
    int min_dist = 3;
    int val_dist = 5;

    auto val_dest = group_gateways_[new_g][gateway_rotater_[new_g]];
    int min_port = group_ports_[dst_g][group_port_rotaters_[dst_g]];

    auto hdr = pkt->get_header<header>();
    bool go_valiant = switch_paths(min_dist, val_dist, min_port, val_dest.first);
    if (go_valiant){
      pkt->current_path().set_outport(val_dest.first);
      pkt->set_dest_switch(val_dest.second);
      gateway_rotater_[new_g] = (gateway_rotater_[new_g] + 1) % group_gateways_[new_g].size();
      hdr->stage_number = valiant_stage;
      rter_debug("chose inter-grp ugal port %d to intermediate %d : pkt=%p:%s",
                 val_dest.first, val_dest.second, pkt, pkt->to_string().c_str());
    } else { //minimal
      pkt->current_path().set_outport(min_port);
      pkt->set_dest_switch(dfly_->get_uid(dst_a,dst_g));
      gateway_rotater_[dst_g] = (gateway_rotater_[dst_g] + 1) % group_gateways_[dst_g].size();
      hdr->stage_number = minimal_stage;
      rter_debug("chose inter-grp minimal port %d: pkt=%p:%s",
                 min_port, pkt, pkt->to_string().c_str());
      group_port_rotaters_[dst_g] = (group_port_rotaters_[dst_g] + 1) % group_ports_[dst_g].size();
    }
  }

  void check_ugal_intra_group(packet* pkt, int dst_a, char minimal_stage){
    uint32_t seed = netsw_->now().ticks();
    uint32_t attempt = 0;
    int new_a = dst_a;
    while (new_a == my_a_ || new_a == dst_a){
      new_a = random_number(dfly_->a(), attempt++, seed);
    }
    int min_dist = 1;
    int val_dist = 2;

    auto hdr = pkt->get_header<header>();
    bool go_valiant = switch_paths(min_dist, val_dist, dst_a, new_a);
    if (go_valiant){
      pkt->current_path().set_outport(new_a);
      pkt->set_dest_switch(dfly_->get_uid(new_a, my_g_));
      hdr->stage_number = valiant_stage;
      rter_debug("chose intra-grp ugal port %d to intermediate %d : pkt=%p:%s",
                 new_a, int(pkt->dest_switch()), pkt, pkt->to_string().c_str());
    } else { //minimal
      pkt->current_path().set_outport(dst_a);
      pkt->set_dest_switch(dfly_->get_uid(dst_a, my_g_));
      hdr->stage_number = minimal_stage;
      rter_debug("chose intra-grp minimal port %d: pkt=%p:%s",
                 dst_a, pkt, pkt->to_string().c_str());
    }
  }

  void route(packet *pkt) override {
    switch_id ej_addr = pkt->toaddr() / dfly_->concentration();
    if (ej_addr == my_addr_){
      int port = pkt->toaddr() % dfly_->concentration();
      pkt->current_path().set_outport(dfly_->a() + dfly_->h() + port);
      pkt->current_path().vc = 0;
      return;
    }

    auto hdr = pkt->get_header<header>();
    switch(hdr->stage_number){
      case initial_stage: {
        int dst_a = dfly_->computeA(ej_addr);
        int dst_g = dfly_->computeG(ej_addr);
        if (dst_g == my_g_){
          check_ugal_intra_group(pkt, dst_a, minimal_only_stage);
        } else {
          check_ugal_inter_group(pkt, dst_a, dst_g, minimal_only_stage);
        }
        break;
      }
      case minimal_only_stage: {
        //don't reconsider my decision
        route_to_switch(pkt, ej_addr);
        rter_debug("continue to minimal %d on port %d: pkt=%p:%s",
                 ej_addr, pkt->current_path().outport(),
                 pkt, pkt->to_string().c_str());
        break;
      }
      case valiant_stage: {
        if (my_addr_ == pkt->dest_switch()){
          hdr->stage_number = final_stage;
        } else {
          route_to_switch(pkt, pkt->dest_switch());
          rter_debug("route to valiant intermediate %d on port %d: pkt=%p:%s",
                     (int(pkt->dest_switch())), pkt->current_path().outport(),
                     pkt, pkt->to_string().c_str());
          break;
        }
      }
      case final_stage: {
        pkt->set_dest_switch(ej_addr);
        route_to_switch(pkt, ej_addr);
        rter_debug("route to final %d on port %d: pkt=%p:%s",
                   ej_addr, pkt->current_path().outport(),
                   pkt, pkt->to_string().c_str());
        break;
      }
      break;
    }
    pkt->current_path().vc = hdr->num_hops;
    if (pkt->current_path().vc >= num_vc()){
      spkt_abort_printf("Packet %p:%s too many hops", pkt, pkt->to_string().c_str());
    }
    ++hdr->num_hops;
  }

 protected:
  int val_threshold_;

};

#if !SSTMAC_INTEGRATED_SST_CORE
class dragonfly_ugalG_router : public dragonfly_ugal_router {

  static const char initial_stage = 0;
  static const char hop_to_entry_stage = 1;
  static const char hop_to_exit_stage = 2;
  static const char final_stage = 3;
  static const char intra_grp_stage = 4;

  struct header : public dragonfly_ugal_router::header {
    uint16_t entryA;
    uint8_t entryPort : 8;
    uint16_t exitA;
    uint8_t exitPort : 8;
    uint16_t interGrp;
  };

 public:
  FactoryRegister("dragonfly_ugalG",
              router, dragonfly_ugalG_router,
              "router implementing UGAL-G routing for dragonfly")

  dragonfly_ugalG_router(sprockit::sim_parameters *params, topology *top, network_switch *netsw)
    :  dragonfly_ugal_router(params, top, netsw), ic_(nullptr)
  {
  }

  std::string to_string() const override {
    return "dragonfly UGAL-G router";
  }

  int num_vc() const override {
    return 6;
  }

  void route(packet *pkt) override {
    switch_id ej_addr = pkt->toaddr() / dfly_->concentration();
    if (ej_addr == my_addr_){
      int port = pkt->toaddr() % dfly_->concentration();
      pkt->current_path().set_outport(dfly_->a() + dfly_->h() + port);
      pkt->current_path().vc = 0;
      return;
    }

    int srcGrp = dfly_->computeG(my_addr_);
    int dstGrp = dfly_->computeG(ej_addr);
    int dstA = dfly_->computeA(ej_addr);
    packet::path& path = pkt->current_path();

    header* hdr = pkt->get_header<header>();
    rter_debug("handling packet %p at stage %d: %s",
               pkt, int(hdr->stage_number), pkt->to_string().c_str());
    if (hdr->stage_number == initial_stage){
      if (srcGrp == dstGrp){
        check_ugal_intra_group(pkt, dstA, intra_grp_stage);
        hdr->stage_number = intra_grp_stage;
        return;
      } else {
        hdr->stage_number = hop_to_entry_stage;
        select_ugalG_intermediate(pkt, ej_addr);
      }
    }

    switch(hdr->stage_number){
    case initial_stage:
      spkt_abort_printf("invalid stage number: packet did not have stage initialized");
      break;
    case hop_to_entry_stage:
      if (my_addr_ == pkt->dest_switch()){
        hdr->stage_number = hop_to_exit_stage;
        path.set_outport(hdr->entryPort);
        pkt->set_dest_switch(dfly_->get_uid(hdr->exitA, hdr->interGrp));
        break;
      } else {
        path.set_outport(hdr->entryA);
        break;
      }
      break;
    case hop_to_exit_stage:
      if (my_addr_ == pkt->dest_switch()){
        hdr->stage_number = final_stage;
        pkt->set_dest_switch(ej_addr);
        path.set_outport(hdr->exitPort);
      } else {
        path.set_outport(hdr->exitA);
      }
      break;
    case final_stage:
    case intra_grp_stage:
      path.set_outport(dstA);
      break;
    }

    path.vc = hdr->num_hops;
    ++hdr->num_hops;
  }

  void select_ugalG_intermediate(packet* pkt, switch_id ej_addr){
    int dstGrp = dfly_->computeG(ej_addr);

    rter_debug("selecting ugal intermediate for final group %d for switch %d",
               dstGrp, ej_addr);

    header* hdr = pkt->get_header<header>();
    int minTotalLength = std::numeric_limits<int>::max();
    int minInterGrp = -1;
    std::pair<int,int> minPath = findLocalMinGroupLink(dstGrp, minTotalLength);
    std::pair<int,int> firstHalfPath = minPath;
    std::pair<int,int> secondHalfPath = minPath;

    for (int intGrp=0; intGrp < dfly_->g(); ++intGrp){
      if (intGrp != my_g_ && intGrp != dstGrp){
        int entryLength;
        int exitLength;
        std::pair<int,int> localHalfPath = findLocalMinGroupLink(intGrp, entryLength);
        std::pair<int,int> remoteHalfPath = findMinGroupLink(intGrp, dstGrp, exitLength);
        int totalLength = entryLength + exitLength;
        rter_debug("considering path of length %d through group %d against current min path length %d",
                   totalLength, intGrp, minTotalLength);
        if (totalLength < minTotalLength){
          minInterGrp = intGrp;
          firstHalfPath = localHalfPath;
          secondHalfPath = remoteHalfPath;
          minTotalLength = totalLength;
        }
      }
    }

    //we have now found the intermediate group with the least amount of contention
    switch_id inter_sw;
    hdr->exitA = dfly_->computeA(secondHalfPath.second);
    hdr->exitPort = secondHalfPath.first;
    hdr->entryA = dfly_->computeA(firstHalfPath.second);
    hdr->entryPort = firstHalfPath.first;
    hdr->interGrp = minInterGrp;
    if (minInterGrp == -1){ //route minimally to the dest grp
      inter_sw = dfly_->get_uid(hdr->exitA, my_g_);
      hdr->stage_number = hop_to_exit_stage;
    } else {
      inter_sw = dfly_->get_uid(hdr->entryA, my_g_);
      hdr->stage_number = hop_to_entry_stage;
    }
    pkt->set_dest_switch(inter_sw);


    rter_debug("finally selected path length %d to switch %d through group %d for packet %p:%s: entry=%d:%d->exit=%d:%d",
               minTotalLength, inter_sw, minInterGrp, pkt, pkt->to_string().c_str(),
               int(hdr->entryA),int(hdr->entryPort),int(hdr->exitA),int(hdr->exitPort));
  }

  std::pair<int,int> findLocalMinGroupLink(int dstGrp, int& queueLength){
    if (!ic_) ic_ = netsw_->event_mgr()->interconn();
    queueLength = std::numeric_limits<int>::max();
    std::pair<int,int> toRet;
    for (int p : group_ports_[dstGrp]){
      if (p < dfly_->a()){ //local hop to another gateway switch
        int sid = dfly_->get_uid(p, my_g_);
        network_switch* nsw = ic_->switch_at(sid);
        dragonfly_ugalG_router* rtr = safe_cast(dragonfly_ugalG_router, nsw->rter());
        for (int port : rtr->group_ports_[dstGrp]){
          int testLength = nsw->queue_length(port);
          if (testLength < queueLength){
            rter_debug("port %d on switch %d has good queue length %d",
                       port, sid, testLength);
            toRet.second = sid;
            toRet.first = port;
            queueLength = testLength;
          }
        }
      } else {
        int testLength = netsw_->queue_length(p);
        if (testLength < queueLength){
          rter_debug("port %d on switch %d has good queue length %d",
                     p, my_addr_, testLength);
          toRet.second = my_addr_;
          toRet.first = p;
          queueLength = testLength;
        }
      }
    }

    rter_debug("minimum link to grp %d is on switch %d over port %d - queue length is %d",
               dstGrp, toRet.second, toRet.first, queueLength);
    return toRet;
  }

  std::pair<int,int> findMinGroupLink(int srcGrp, int dstGrp, int& queueLength){
    if (!ic_) ic_ = netsw_->event_mgr()->interconn();
    network_switch* nsw = ic_->switch_at(dfly_->get_uid(0, srcGrp));
    dragonfly_ugalG_router* rtr = safe_cast(dragonfly_ugalG_router, nsw->rter());
    return rtr->findLocalMinGroupLink(dstGrp, queueLength);
  }

 private:
  interconnect* ic_;

};
#endif


class dragonfly_par_router : public dragonfly_ugal_router {
 public:
  FactoryRegister("dragonfly_par",
              router, dragonfly_par_router,
              "router implementing PAR for dragonfly")

  std::string to_string() const override {
    return "dragonfly PAR router";
  }

  dragonfly_par_router(sprockit::sim_parameters* params, topology *top,
                       network_switch *netsw)
    : dragonfly_ugal_router(params, top, netsw)
  {
  }

  void route(packet *pkt) override {
    switch_id ej_addr = pkt->toaddr() / dfly_->concentration();
    if (ej_addr == my_addr_){
      int port = pkt->toaddr() % dfly_->concentration();
      pkt->current_path().set_outport(dfly_->a() + dfly_->h() + port);
      pkt->current_path().vc = 0;
      return;
    }

    auto hdr = pkt->get_header<header>();
    switch(hdr->stage_number){
      case initial_stage: {
        int dst_a = dfly_->computeA(ej_addr);
        int dst_g = dfly_->computeG(ej_addr);
        if (dst_g == my_g_){
          check_ugal_intra_group(pkt, dst_a, initial_stage);
        } else {
          check_ugal_inter_group(pkt, dst_a, dst_g, initial_stage);
        }
        break;
      }
      case valiant_stage: {
        if (my_addr_ == pkt->dest_switch()){
          hdr->stage_number = final_stage;
        } else {
          route_to_switch(pkt, pkt->dest_switch());
          break;
        }
      }
      case final_stage: {
        route_to_switch(pkt, ej_addr);
        pkt->set_dest_switch(ej_addr);
        break;
      }
      break;
    }
    pkt->current_path().vc = hdr->num_hops;
    ++hdr->num_hops;
  }

};

}
}
