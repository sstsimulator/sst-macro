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

  void up_path( int dim, int srcX, int dstX, header* hdr) const
  {
    auto& dimensions_ = torus_->dimensions();
    if ((srcX + 1) % dimensions_[dim] == dstX){
      //move onto next dimension
      hdr->deadlock_vc = 0;
      hdr->crossed_timeline = 0;
    } else if (srcX == (dimensions_[dim]-1)){
      hdr->crossed_timeline = 1;
      hdr->deadlock_vc = 1;
    } else {
      hdr->deadlock_vc = hdr->crossed_timeline ? 1 : 0;
    }
    hdr->edge_port = torus_->convert_to_port(dim, torus::pos);
  }

  void down_path( int dim, int src, int dst, header* hdr) const
  {
    auto& dimensions_ = torus_->dimensions();
    if (src == ((dst + 1) % dimensions_[dim])){
      //move onto next dimension
      hdr->deadlock_vc = 0;
      hdr->crossed_timeline = 0;
    } else if (src == 0){
      hdr->crossed_timeline = 1;
      hdr->deadlock_vc = 1;
    } else {
      hdr->deadlock_vc = hdr->crossed_timeline ? 1 : 0;
    }
    hdr->edge_port = torus_->convert_to_port(dim, torus::neg);
  }

  void minimal_route(switch_id dst, header* hdr){
    switch_id src = my_addr_;
    auto& dimensions_ = torus_->dimensions();
    int div = 1;
    int ndim = dimensions_.size();
    for (int i=0; i < ndim; ++i){
      int srcX = (src / div) % dimensions_[i];
      int dstX = (dst / div) % dimensions_[i];
      if (srcX != dstX){
        if (torus_->shortest_path_positive(i, srcX, dstX)){
          top_debug("torus routing up on dim %d for switch %d to %d on port %d",
                    i, src, dst, int(hdr->edge_port));
          up_path(i, srcX, dstX, hdr);
          return;
        } else {
          top_debug("torus routing down on dim %d for switch %d to %d on port %d",
                    i, src, dst, int(hdr->edge_port));
          down_path(i, srcX, dstX, hdr);
          return;
        }
      }
      div *= dimensions_[i];
    }
    sprockit::abort("torus: failed to route correctly on torus");
  }

  void route(packet* pkt) override {
    auto* hdr = pkt->rtr_header<header>();
    switch_id ej_addr = pkt->toaddr() / torus_->concentration();
    if (ej_addr == my_addr_){
      hdr->edge_port = pkt->toaddr() % torus_->concentration() + inj_port_offset_;
      hdr->deadlock_vc = 0;
      return;
    }

    minimal_route(ej_addr, hdr);
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
    uint32_t dest_switch : 24;
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
    auto* hdr = pkt->rtr_header<header>();
    switch_id ej_addr = pkt->toaddr() / torus_->concentration();
    if (ej_addr == my_addr_){
      hdr->edge_port = pkt->toaddr() % torus_->concentration() + inj_port_offset_;
      hdr->deadlock_vc = 0;
      return;
    }


    switch(hdr->stage_number){
      case initial_stage: {
        switch_id inter = get_intermediate(pkt, ej_addr);
        hdr->dest_switch = inter;
        minimal_route(inter, hdr);
        hdr->stage_number = valiant_stage;
        rter_debug("chose intermediate %d for pkt %s on %d:%d",
                   inter, pkt->to_string().c_str(),
                   int(hdr->edge_port), int(hdr->deadlock_vc));
        break;
      }
      case valiant_stage: {
        if (my_addr_ == hdr->dest_switch){
          hdr->stage_number = final_stage;
          rter_debug("reached intermediate %d for pkt %s",
                     int(hdr->dest_switch), pkt->to_string().c_str());
        } else {
          rter_debug("route to intermediate %d for pkt %s on %d:%d",
                     int(hdr->dest_switch), pkt->to_string().c_str(),
                     int(hdr->edge_port), int(hdr->deadlock_vc));
          minimal_route(hdr->dest_switch, hdr);
          break;
        }
      }
      case final_stage: {
        minimal_route(ej_addr, hdr);
        hdr->dest_switch = ej_addr;
        hdr->deadlock_vc += 2; //final stage vc moves
        rter_debug("route to final %d for pkt %s on %d:%d",
                   int(hdr->dest_switch), pkt->to_string().c_str(),
                   int(hdr->edge_port), int(hdr->deadlock_vc));
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
    auto* hdr = pkt->rtr_header<header>();
    switch_id ej_addr = pkt->toaddr() / torus_->concentration();
    if (ej_addr == my_addr_){
      hdr->edge_port = pkt->toaddr() % torus_->concentration() + inj_port_offset_;
      hdr->deadlock_vc = 0;
      return;
    }

    switch(hdr->stage_number){
      case initial_stage: {
        switch_id inter = get_intermediate(pkt, ej_addr);
        header ugal; minimal_route(inter, &ugal);
        header min; minimal_route(ej_addr, &min);
        int min_dist = torus_->minimal_distance(my_addr_, ej_addr);
        int ugal_dist = torus_->minimal_distance(my_addr_, inter) +
                        torus_->minimal_distance(inter, ej_addr);

        int ugal_metric = netsw_->queue_length(ugal.edge_port) * ugal_dist;
        int min_metric = netsw_->queue_length(min.edge_port) * min_dist;

        if (ugal_metric < min_metric){
          hdr->dest_switch = inter;
          hdr->stage_number = valiant_stage;
          rter_debug("chose intermediate %d for pkt %s on %d:%d",
                     inter, pkt->to_string().c_str(),
                     int(hdr->edge_port), int(hdr->deadlock_vc));
        } else {
          hdr->dest_switch = ej_addr;
          hdr->stage_number = minimal_only_stage;
          rter_debug("chose minimal %d for pkt %s on %d:%d",
                     ej_addr, pkt->to_string().c_str(), int(hdr->edge_port), int(hdr->deadlock_vc));
        }
        minimal_route(hdr->dest_switch, hdr);
        break;
      }
      case valiant_stage: {
        if (my_addr_ == hdr->dest_switch){
          hdr->stage_number = final_stage;
          rter_debug("reached intermediate %d for pkt %s",
                     int(hdr->dest_switch), pkt->to_string().c_str());
        } else {
          rter_debug("route to intermediate %d for pkt %s on %d:%d",
                     int(hdr->dest_switch), pkt->to_string().c_str(),
                     int(hdr->edge_port), int(hdr->deadlock_vc));
          minimal_route(hdr->dest_switch, hdr);
          break;
        }
      }
      case final_stage: {
        minimal_route(ej_addr, hdr);
        hdr->dest_switch = ej_addr;
        hdr->deadlock_vc += 2; //final stage vc moves
        rter_debug("route to final %d for pkt %s on %d:%d",
                   int(hdr->dest_switch), pkt->to_string().c_str(),
                   int(hdr->edge_port), int(hdr->deadlock_vc));
        break;
      }
      case minimal_only_stage: {
        rter_debug("route to minimal %d for pkt %s on %d:%d",
                   int(hdr->dest_switch), pkt->to_string().c_str(),
                   int(hdr->edge_port), int(hdr->deadlock_vc));
        minimal_route(ej_addr, hdr);
        break;
      }
    }
  }


};


}
}
