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
#include <sstmac/hardware/topology/torus.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <cmath>

#define ftree_rter_debug(...) \
  rter_debug("fat tree: %s", sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {


class torus_minimal_router : public router {
 public:
  struct header : public packet::header {
     char crossed_timeline : 1;
  };

  FactoryRegister("torus_minimal", router, torus_minimal_router,
              "a routing algorithm for minimal routing on the torus")

  torus_minimal_router(sprockit::sim_parameters* params,
                          topology* top, network_switch* netsw)
    : router(params, top, netsw)
  {
    torus_ = safe_cast(torus, top);
    inj_port_offset_ = 2*torus_->ndimensions();
  }

  std::string to_string() const override {
    return "torus minimal router";
  }

  int num_vc() const override {
    return 2;
  }

  void route(packet* pkt) override {
    switch_id ej_addr = pkt->toaddr() / torus_->concentration();
    if (ej_addr == my_addr_){
      pkt->current_path().outport() = pkt->toaddr() % torus_->concentration() + inj_port_offset_;
      pkt->current_path().vc = 0;
      return;
    }

    route_to_switch(pkt, ej_addr);
  }

  void route_to_switch(packet* pkt, switch_id ej_addr) {
    packet::path& path = pkt->current_path();
    torus::route_type_t ty = torus_->torus_route(my_addr_, ej_addr, path);
    auto hdr = pkt->get_header<header>();
    switch(ty){
      case torus::same_path:
        if (hdr->crossed_timeline) path.vc = 1;
        else path.vc = 0;
        break; //keep the original virtual channel
      case torus::new_dimension:
        path.vc = 0;
        break;
      case torus::wrapped_around:
        path.vc = 1;
        break;
    }
  }

 protected:
  torus* torus_;
  int inj_port_offset_;
};

class torus_valiant_router : public torus_minimal_router {
 public:
  static const char initial_stage = 0;
  static const char valiant_stage = 1;
  static const char final_stage = 2;

  struct header : public torus_minimal_router::header {
    uint8_t stage_number : 3;
  };

  FactoryRegister("torus_valiant",
              router, torus_valiant_router,
              "router implementing valint routing for torus")

  torus_valiant_router(sprockit::sim_parameters* params, topology *top,
                           network_switch *netsw)
    : torus_minimal_router(params, top, netsw)
  {
  }

  std::string to_string() const override {
    return "torus valiant";
  }

  int num_vc() const override {
    return 4;
  }

  switch_id get_intermediate(packet* pkt, switch_id ej_addr){
    uint32_t seed = netsw_->now().ticks();
    uint32_t attempt = 0;
    switch_id new_sw = ej_addr;
    while (new_sw == ej_addr || new_sw == my_addr_){
      new_sw = random_number(torus_->num_switches(), attempt++, seed);
    }
    return new_sw;
  }

  void route(packet *pkt) override {
    switch_id ej_addr = pkt->toaddr() / torus_->concentration();
    if (ej_addr == my_addr_){
      pkt->current_path().outport() = pkt->toaddr() % torus_->concentration() + inj_port_offset_;
      pkt->current_path().vc = 0;
      return;
    }

    auto hdr = pkt->get_header<header>();
    switch(hdr->stage_number){
      case initial_stage: {
        switch_id inter = get_intermediate(pkt, ej_addr);
        pkt->set_dest_switch(inter);
        route_to_switch(pkt, inter);
        hdr->stage_number = valiant_stage;
        rter_debug("chose intermediate %d for pkt %s on %d:%d",
                   inter, pkt->to_string().c_str(),
                   pkt->current_path().outport(), pkt->current_path().vc);
        break;
      }
      case valiant_stage: {
        if (my_addr_ == pkt->dest_switch()){
          hdr->stage_number = final_stage;
          rter_debug("reached intermediate %d for pkt %s", pkt->dest_switch(), pkt->to_string().c_str());
        } else {
          rter_debug("route to intermediate %d for pkt %s on %d:%d",
                     pkt->dest_switch(), pkt->to_string().c_str(),
                     pkt->current_path().outport(), pkt->current_path().vc);
          route_to_switch(pkt, pkt->dest_switch());
          break;
        }
      }
      case final_stage: {
        route_to_switch(pkt, ej_addr);
        pkt->set_dest_switch(ej_addr);
        pkt->current_path().vc += 2; //final stage vc moves
        rter_debug("route to final %d for pkt %s on %d:%d",
                   pkt->dest_switch(), pkt->to_string().c_str(),
                   pkt->current_path().outport(), pkt->current_path().vc);
        break;
      }
      break;
    }
  }


};

class torus_ugal_router : public torus_valiant_router {
 public:
  static const char minimal_only_stage = 3;

  FactoryRegister("torus_ugal",
              router, torus_ugal_router,
              "router implementing valint routing for torus")

  torus_ugal_router(sprockit::sim_parameters* params, topology *top,
                           network_switch *netsw)
    : torus_valiant_router(params, top, netsw)
  {
  }

  std::string to_string() const override {
    return "torus ugal";
  }

  void route(packet *pkt) override {
    switch_id ej_addr = pkt->toaddr() / torus_->concentration();
    if (ej_addr == my_addr_){
      pkt->current_path().outport() = pkt->toaddr() % torus_->concentration() + inj_port_offset_;
      pkt->current_path().vc = 0;
      return;
    }

    auto hdr = pkt->get_header<header>();
    switch(hdr->stage_number){
      case initial_stage: {
        switch_id inter = get_intermediate(pkt, ej_addr);
        packet::path ugal; torus_->minimal_route_to_switch(my_addr_, inter, ugal);
        packet::path min; torus_->minimal_route_to_switch(my_addr_, ej_addr, min);
        int min_dist = torus_->minimal_distance(my_addr_, ej_addr);
        int ugal_dist = torus_->minimal_distance(my_addr_, inter) +
                        torus_->minimal_distance(inter, ej_addr);

        int ugal_metric = netsw_->queue_length(ugal.outport()) * ugal_dist;
        int min_metric = netsw_->queue_length(min.outport()) * min_dist;

        if (ugal_metric < min_metric){
          pkt->set_dest_switch(inter);
          hdr->stage_number = valiant_stage;
          rter_debug("chose intermediate %d for pkt %s on %d:%d",
                     inter, pkt->to_string().c_str(),
                     pkt->current_path().outport(), pkt->current_path().vc);
        } else {
          pkt->set_dest_switch(ej_addr);
          hdr->stage_number = minimal_only_stage;
          rter_debug("chose minimal %d for pkt %s on %d:%d",
                     ej_addr, pkt->to_string().c_str(),
                     pkt->current_path().outport(), pkt->current_path().vc);
        }
        route_to_switch(pkt, pkt->dest_switch());
        break;
      }
      case valiant_stage: {
        if (my_addr_ == pkt->dest_switch()){
          hdr->stage_number = final_stage;
          rter_debug("reached intermediate %d for pkt %s", pkt->dest_switch(), pkt->to_string().c_str());
        } else {
          rter_debug("route to intermediate %d for pkt %s on %d:%d",
                     pkt->dest_switch(), pkt->to_string().c_str(),
                     pkt->current_path().outport(), pkt->current_path().vc);
          route_to_switch(pkt, pkt->dest_switch());
          break;
        }
      }
      case final_stage: {
        route_to_switch(pkt, ej_addr);
        pkt->set_dest_switch(ej_addr);
        pkt->current_path().vc += 2; //final stage vc moves
        rter_debug("route to final %d for pkt %s on %d:%d",
                   pkt->dest_switch(), pkt->to_string().c_str(),
                   pkt->current_path().outport(), pkt->current_path().vc);
        break;
      }
      case minimal_only_stage: {
        rter_debug("route to minimal %d for pkt %s on %d:%d",
                 pkt->dest_switch(), pkt->to_string().c_str(),
                 pkt->current_path().outport(), pkt->current_path().vc);
        route_to_switch(pkt, ej_addr);
        break;
      }
    }
  }


};


}
}
