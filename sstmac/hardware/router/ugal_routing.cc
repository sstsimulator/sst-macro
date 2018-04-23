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
#include <sstmac/hardware/switch/network_switch.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

ugal_router::ugal_router(sprockit::sim_parameters *params, topology *top, network_switch *netsw)
  :  router(params, top, netsw)
{
  val_threshold_ = params->get_optional_int_param("ugal_threshold", 0);
  val_preference_factor_ = params->get_optional_int_param("valiant_preference_factor",1);
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
