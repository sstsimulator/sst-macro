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

#include <sstmac/hardware/router/ugal_routing.h>
#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/hardware/topology/torus.h>
#include <sstmac/hardware/topology/cascade.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sstream>

namespace sstmac {
namespace hw {

ugal_router::ugal_router(sprockit::sim_parameters *params, topology *top, network_switch *netsw)
  :  router(params, top, netsw)
{
  val_threshold_ = params->get_optional_int_param("ugal_threshold", 0);
  val_preference_factor_ = params->get_optional_int_param("valiant_preference_factor",1);
}

void
ugal_router::route_ugal_common(routable* rtbl, switch_id ej_addr)
{
  auto hdr = rtbl->current_path().header<header>();
  switch(hdr->stage_number){
    case initial_stage: {
      switch_id middle_switch = top_->random_intermediate_switch(addr(), ej_addr, netsw_->now().ticks());
      routable::path orig;
      routable::path valiant;
      bool go_valiant = switch_paths(ej_addr, middle_switch, orig, valiant);
      if (go_valiant){
        rtbl->set_dest_switch(middle_switch);
        rtbl->current_path() = valiant;
        hdr->stage_number = valiant_stage;
      } else {
        rtbl->set_dest_switch(ej_addr);
        rtbl->current_path() = orig;
        hdr->stage_number = minimal_only_stage;
      }
      break;
    }
    case minimal_only_stage: {
      //don't reconsider my decision
      break;
    }
    case valiant_stage: {
      if (my_addr_ == rtbl->dest_switch()){
        hdr->stage_number = final_stage;
      } else {
        break;
      }
    }
    case final_stage: {
      rtbl->set_dest_switch(ej_addr);
      break;
    }
    break;
  }
}

bool
ugal_router::route_common(routable* rtbl)
{
  uint16_t dir;
  switch_id ej_addr = top_->netlink_to_ejection_switch(rtbl->toaddr(), dir);
  if (ej_addr == my_addr_){
    rtbl->current_path().outport() = dir;
    rtbl->current_path().vc = 0;
    return true;
  }

  route_ugal_common(rtbl, ej_addr);
  return false;
}

bool
ugal_router::switch_paths(
  switch_id orig_dst,
  switch_id new_dst,
  routable::path& orig_path,
  routable::path& new_path)
{
  switch_id src = my_addr_;
  top_->minimal_route_to_switch(src, orig_dst, orig_path);
  top_->minimal_route_to_switch(src, new_dst, new_path);
  int orig_queue_length = netsw_->queue_length(orig_path.outport());
  int new_queue_length = netsw_->queue_length(new_path.outport());
  int orig_distance = top_->minimal_distance(src, orig_dst);
  int new_distance = top_->minimal_distance(src, new_dst)
                      + top_->minimal_distance(new_dst, orig_dst);
  int orig_weight = orig_queue_length * orig_distance * val_preference_factor_;
  int valiant_weight = new_queue_length * new_distance;
  return valiant_weight < orig_weight;
}


class dragonfly_ugalG_router : public ugal_router {

  static const char initial_stage = 0;
  static const char hop_to_entry_stage = 1;
  static const char hop_to_exit_stage = 2;
  static const char final_stage = 3;
  static const char intra_grp_stage = 4;

  struct header : public ugal_router::header {
    char num_group_hops : 3;
    uint16_t entryA;
    uint16_t exitA;
    uint16_t interGrp;
  };

 public:
  FactoryRegister("dragonfly_ugalG",
              router, dragonfly_ugalG_router,
              "router implementing UGAL-G routing for dragonfly")

  dragonfly_ugalG_router(sprockit::sim_parameters *params, topology *top, network_switch *netsw)
    :  ugal_router(params, top, netsw)
  {
    dfly_ = safe_cast(dragonfly, top);
  }

  std::string to_string() const {
    return "dragonfly UGAL-G router";
  }

  int num_vc() const {
    return 3;
  }

  void route(packet *pkt){
    uint16_t dir;
    routable* rtbl = pkt->interface<routable>();
    routable::path& path = rtbl->current_path();

    switch_id ej_addr = top_->netlink_to_ejection_switch(rtbl->toaddr(), dir);
    if (ej_addr == my_addr_){
      rtbl->current_path().outport() = dir;
      rtbl->current_path().vc = 0;
      return;
    }

    header* hdr = path.header<header>();
    if (hdr->stage_number == initial_stage){
      int srcGrp = dfly_->computeG(my_addr_);
      int dstGrp = dfly_->computeG(ej_addr);
      if (srcGrp == dstGrp){
        route_ugal_common(rtbl, ej_addr);
        hdr->stage_number = intra_grp_stage;
        return;
      } else {
        hdr->stage_number = hop_to_entry_stage;
        select_ugalG_intermediate(rtbl, ej_addr);
      }
    }

    //std::cout << "Got header " << (void*) pkt << " at stage " << (uint16_t) hdr->stage_number << std::endl;

    switch(hdr->stage_number){
    case initial_stage:
      spkt_abort_printf("invalid stage number: packet did not have stage initialized");
      break;
    case hop_to_entry_stage:
      if (my_addr_ == rtbl->dest_switch()){
        hdr->stage_number = hop_to_exit_stage;
        rtbl->set_dest_switch(dfly_->get_uid(hdr->exitA, hdr->interGrp));
      } else {
        top_->minimal_route_to_switch(my_addr_, rtbl->dest_switch(), path);
        break;
      }
    case hop_to_exit_stage:
      if (my_addr_ == rtbl->dest_switch()){
        hdr->stage_number = final_stage;
        rtbl->set_dest_switch(ej_addr);
      } else {
        top_->minimal_route_to_switch(my_addr_, rtbl->dest_switch(), path);
        break;
      }
    case final_stage:
    case intra_grp_stage:
      top_->minimal_route_to_switch(my_addr_, ej_addr, path);
      break;
    }

    path.vc = hdr->num_group_hops;
    if (dfly_->is_global_port(path.outport())){
      ++hdr->num_group_hops;
    }
  }

  void select_ugalG_intermediate(routable* rtbl, switch_id ej_addr){
    //this has to be intialized at runtime
    //this can't happen in the ctor because ic isn't available yet in the event mgr
    if (!ic_) ic_ = netsw_->event_mgr()->interconn();

    int srcGrp = dfly_->computeG(my_addr_);
    int dstGrp = dfly_->computeG(ej_addr);
    routable::path& path = rtbl->current_path();

    header* hdr = path.header<header>();
    int myA = dfly_->computeA(my_addr_);
    int minExitA = 0;
    int minTotalLength = 1e6;
    int minInterGrp = -1;
    findMinGroupLink(srcGrp, dstGrp, minExitA, minTotalLength);

    int minEntryA = 0;
    //std::stringstream sstr;
    //sstr << sprockit::printf("For packet going from (%d,%d) to (%d,%d) with minimal length=%d:\n",
    //                         myA, srcGrp, dfly_->computeA(ej_addr), dstGrp, minTotalLength);
    for (int intGrp=0; intGrp < dfly_->g(); ++intGrp){
      if (intGrp != srcGrp && intGrp != dstGrp){
        int entryA, entryLength;
        int exitA, exitLength;
        findMinGroupLink(srcGrp, intGrp, entryA, entryLength);
        findMinGroupLink(intGrp, dstGrp, exitA, exitLength);
        int totalLength = entryLength + exitLength;
        //sstr << "\tintermediate=" << intGrp << " found queue lengths="
        //     << entryLength << "," << exitLength << " for gateways="
        //    << entryA << "," << exitA << "\n";
        if (totalLength < minTotalLength){
          minInterGrp = intGrp;
          minEntryA = entryA; //gateway in the source group
          minExitA = exitA; //gateway in the inter group
          minTotalLength = totalLength;
        }
      }
    }

    //if (minInterGrp != -1) sstr << "Selected intermediate " << minInterGrp << "!\n";
    //std::cout << sstr.str() << std::endl;

    //we have now found the intermediate group with the least amount of contention
    switch_id inter_sw;
    if (minInterGrp == -1){ //route minimally to the dest grp
      if (minExitA == myA){
        inter_sw = ej_addr;
        hdr->stage_number = final_stage;
      } else {
        inter_sw = dfly_->get_uid(minExitA, srcGrp);
        hdr->stage_number = hop_to_exit_stage;
      }
    } else {
      if (minEntryA == myA){
        inter_sw = dfly_->get_uid(minExitA, minInterGrp);
        hdr->stage_number = hop_to_exit_stage;
      } else {
        inter_sw = dfly_->get_uid(minEntryA, srcGrp);
        hdr->stage_number = hop_to_entry_stage;
      }
    }
    rtbl->set_dest_switch(inter_sw);
    hdr->exitA = minExitA;
    hdr->entryA = minEntryA;
    hdr->interGrp = minInterGrp;
  }

  void findMinGroupLink(int srcGrp, int dstGrp, int& srcA, int& queueLength){
    inter_group_wiring* wiring = dfly_->group_wiring();
    queueLength = 1e6;
    std::vector<std::pair<int,int>> sources;
    wiring->connected_to_group(srcGrp, dstGrp, sources);
    for (auto& pair : sources){
      int a = pair.first;
      int port = dfly_->a() + pair.second;
      network_switch* nsw = ic_->switch_at(dfly_->get_uid(a, srcGrp));
      int testLength = nsw->queue_length(port);
      if (testLength < queueLength){
        srcA = a;
        queueLength = testLength;
      }
    }
  }


 private:
  dragonfly* dfly_;
  interconnect* ic_;

};

class dragonfly_ugal_router : public ugal_router {
  struct header : public ugal_router::header {
     char num_hops : 3;
     char num_group_hops : 2;
  };
 public:
  FactoryRegister("dragonfly_ugal",
              router, dragonfly_ugal_router,
              "router implementing UGAL routing for dragonfly")

  dragonfly_ugal_router(sprockit::sim_parameters* params, topology *top,
                                 network_switch *netsw)
    : ugal_router(params, top, netsw)
  {
    dfly_ = safe_cast(dragonfly, top);
  }

  std::string to_string() const override {
    return "dragonfly ugal router";
  }

  int num_vc() const override {
    return 3;
  }

  void route(packet *pkt) override {
    routable* rtbl = pkt->interface<routable>();
    bool eject = route_common(rtbl);
    if (eject) return;

    routable::path& path = rtbl->current_path();
    dfly_->minimal_route_to_switch(my_addr_, rtbl->dest_switch(), path);
    auto hdr = path.header<header>();
    path.vc = hdr->num_group_hops;
    if (dfly_->is_global_port(path.outport())){
      ++hdr->num_group_hops;
    }
    ++hdr->num_hops;
  }

 private:
  dragonfly* dfly_;
};

class cascade_ugal_router : public ugal_router {
  struct header : public ugal_router::header {
     char num_hops : 3;
     char num_group_hops : 3;
  };
 public:
  FactoryRegister("cascade_ugal",
              router, cascade_ugal_router,
              "router implementing UGAL routing for cascade")

  cascade_ugal_router(sprockit::sim_parameters* params, topology *top,
                                 network_switch *netsw)
    : ugal_router(params, top, netsw)
  {
    cascade_ = safe_cast(cascade, top);
  }

  std::string to_string() const override {
    return "cascade ugal router";
  }

  int num_vc() const override {
    return 3;
  }

  void route(packet *pkt) override {
    routable* rtbl = pkt->interface<routable>();
    bool eject = route_common(rtbl);
    if (eject) return;

    routable::path& path = rtbl->current_path();
    cascade_->minimal_route_to_switch(my_addr_, rtbl->dest_switch(), path);
    auto hdr = path.header<header>();
    path.vc = hdr->num_group_hops;
    if (cascade_->is_global_port(path.outport())){
      ++hdr->num_group_hops;
    }
    ++hdr->num_hops;
  }

 private:
  cascade* cascade_;

};

class torus_ugal_router : public ugal_router {
  struct header : public ugal_router::header {
     char crossed_timeline : 1;
  };
 public:
  FactoryRegister("torus_ugal",
              router, torus_ugal_router,
              "router implementing UGAL routing for dragonfly")

  torus_ugal_router(sprockit::sim_parameters* params, topology *top,
                    network_switch *netsw)
    : ugal_router(params, top, netsw)
  {
    torus_ = safe_cast(torus, top);
  }

  int num_vc() const override {
    return 4;
  }

  std::string to_string() const override {
    return "torus ugal router";
  }

  void route(packet* pkt) override {
    routable* rtbl = pkt->interface<routable>();
    bool eject = route_common(rtbl);
    if (eject) return;

    auto& p = rtbl->current_path();
    torus::route_type_t ty = torus_->torus_route(my_addr_, rtbl->dest_switch(), p);
    auto hdr = p.header<header>();
    int min_vc = 0;
    switch(ty){
      case torus::same_path:
        if (hdr->crossed_timeline) min_vc = 1;
        else min_vc = 0;
        break; //keep the original virtual channel
      case torus::new_dimension:
        min_vc = 0;
        break;
      case torus::wrapped_around:
        min_vc = 1;
        break;
    }
    switch(hdr->stage_number){
      case initial_stage:
      case minimal_only_stage:
        p.vc = min_vc;
        break;
      case valiant_stage:
      case final_stage:
        p.vc = min_vc + 2;
        break;
    }
  }

 private:
  torus* torus_;
};

}
}
