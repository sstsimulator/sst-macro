/**
Copyright 2009-2021 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2021, NTESS

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
  rter_debug("fat tree: %s", sprockit::sprintf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {


class TorusMinimalRouter : public Router {
 public:
  struct header : public Packet::Header {
     char crossed_timeline : 1;
     char last_dim;
  };

  SST_ELI_REGISTER_DERIVED(
    Router,
    TorusMinimalRouter,
    "macro",
    "torus_minimal",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "a routing algorithm for minimal routing on the torus")

  TorusMinimalRouter(SST::Params& params,
                     Topology* top, NetworkSwitch* netsw)
    : Router(params, top, netsw)
  {
    torus_ = safe_cast(Torus, top);
    inj_port_offset_ = 2*torus_->ndimensions();
  }

  std::string toString() const override {
    return "torus minimal router";
  }

  int numVC() const override {
    return 2;
  }

  void upPath(int dim, int srcX, int dstX, header* hdr) const
  {
    auto& dimensions_ = torus_->dimensions();
    if (srcX == (dimensions_[dim]-1)){
      hdr->crossed_timeline = 1;
      hdr->deadlock_vc = 1;
    } else if (dim != hdr->last_dim){
      hdr->deadlock_vc = 0;
      hdr->crossed_timeline = 0;
    } else {
      hdr->deadlock_vc = hdr->crossed_timeline ? 1 : 0;
    }
    hdr->last_dim = dim;
    hdr->edge_port = torus_->convertToPort(dim, Torus::pos);
  }

  void downPath(int dim, int src, int dst, header* hdr) const
  {
    auto& dimensions_ = torus_->dimensions();
    if (src == 0){
      hdr->crossed_timeline = 1;
      hdr->deadlock_vc = 1;
    } else if (dim != hdr->last_dim){
      hdr->deadlock_vc = 0;
      hdr->crossed_timeline = 0;
    } else {
      hdr->deadlock_vc = hdr->crossed_timeline ? 1 : 0;
    }
    hdr->last_dim = dim;
    hdr->edge_port = torus_->convertToPort(dim, Torus::neg);
  }

  void minimalRoute(SwitchId dst, header* hdr){
    SwitchId src = my_addr_;
    auto& dimensions_ = torus_->dimensions();
    int div = 1;
    int ndim = dimensions_.size();
    for (int i=0; i < ndim; ++i){
      int srcX = (src / div) % dimensions_[i];
      int dstX = (dst / div) % dimensions_[i];
      if (srcX != dstX){
        if (torus_->shortestPathPositive(i, srcX, dstX)){
          top_debug("torus routing up on dim %d for switch %d to %d on port %d",
                    i, src, dst, int(hdr->edge_port));
          upPath(i, srcX, dstX, hdr);
          return;
        } else {
          top_debug("torus routing down on dim %d for switch %d to %d on port %d",
                    i, src, dst, int(hdr->edge_port));
          downPath(i, srcX, dstX, hdr);
          return;
        }
      }
      div *= dimensions_[i];
    }
    sprockit::abort("torus: failed to route correctly on torus");
  }

  void route(Packet* pkt) override {
    auto* hdr = pkt->rtrHeader<header>();
    SwitchId ej_addr = pkt->toaddr() / torus_->concentration();
    if (ej_addr == my_addr_){
      hdr->edge_port = pkt->toaddr() % torus_->concentration() + inj_port_offset_;
      hdr->deadlock_vc = 0;
      return;
    }

    minimalRoute(ej_addr, hdr);
  }

 protected:
  Torus* torus_;
  int inj_port_offset_;
};

class TorusValiantRouter : public TorusMinimalRouter {
 public:
  static const char initial_stage = 0;
  static const char valiant_stage = 1;
  static const char final_stage = 2;

  struct header : public TorusMinimalRouter::header {
    uint8_t stage_number : 3;
    uint32_t dest_switch : 24;
  };

  SST_ELI_REGISTER_DERIVED(
    Router,
    TorusValiantRouter,
    "macro",
    "torus_valiant",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "a routing algorithm for Valiant routing on the torus")

  TorusValiantRouter(SST::Params& params, Topology *top,
                           NetworkSwitch *netsw)
    : TorusMinimalRouter(params, top, netsw)
  {
  }

  std::string toString() const override {
    return "torus valiant";
  }

  int numVC() const override {
    return 4;
  }

  SwitchId getIntermediate(Packet*  /*pkt*/, SwitchId ej_addr){
    uint32_t seed = netsw_->now().time.ticks();
    uint32_t attempt = 0;
    SwitchId new_sw = ej_addr;
    while (new_sw == ej_addr || new_sw == my_addr_){
      new_sw = randomNumber(torus_->numSwitches(), attempt++, seed);
    }
    return new_sw;
  }

  void route(Packet *pkt) override {
    auto* hdr = pkt->rtrHeader<header>();
    SwitchId ej_addr = pkt->toaddr() / torus_->concentration();
    if (ej_addr == my_addr_){
      hdr->edge_port = pkt->toaddr() % torus_->concentration() + inj_port_offset_;
      hdr->deadlock_vc = 0;
      return;
    }


    switch(hdr->stage_number){
      case initial_stage: {
        SwitchId inter = getIntermediate(pkt, ej_addr);
        hdr->dest_switch = inter;
        minimalRoute(inter, hdr);
        hdr->stage_number = valiant_stage;
        rter_debug("chose intermediate %d for pkt %s on %d:%d",
                   inter, pkt->toString().c_str(),
                   int(hdr->edge_port), int(hdr->deadlock_vc));
        break;
      }
      case valiant_stage: {
        if (my_addr_ == hdr->dest_switch){
          hdr->stage_number = final_stage;
          rter_debug("reached intermediate %d for pkt %s",
                     int(hdr->dest_switch), pkt->toString().c_str());
        } else {
          rter_debug("route to intermediate %d for pkt %s on %d:%d",
                     int(hdr->dest_switch), pkt->toString().c_str(),
                     int(hdr->edge_port), int(hdr->deadlock_vc));
          minimalRoute(hdr->dest_switch, hdr);
          break;
        }
      }
      case final_stage: {
        minimalRoute(ej_addr, hdr);
        hdr->dest_switch = ej_addr;
        hdr->deadlock_vc += 2; //final stage vc moves
        rter_debug("route to final %d for pkt %s on %d:%d",
                   int(hdr->dest_switch), pkt->toString().c_str(),
                   int(hdr->edge_port), int(hdr->deadlock_vc));
        break;
      }
      break;
    }
  }


};

class TorusUGALRouter : public TorusValiantRouter {
 public:
  static const char minimal_only_stage = 3;

  SST_ELI_REGISTER_DERIVED(
    Router,
    TorusUGALRouter,
    "macro",
    "torus_ugal",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "a routing algorithm for UGAL routing on the torus")

  TorusUGALRouter(SST::Params& params, Topology *top,
                           NetworkSwitch *netsw)
    : TorusValiantRouter(params, top, netsw)
  {
  }

  std::string toString() const override {
    return "torus ugal";
  }

  void route(Packet *pkt) override {
    auto* hdr = pkt->rtrHeader<header>();
    SwitchId ej_addr = pkt->toaddr() / torus_->concentration();
    if (ej_addr == my_addr_){
      hdr->edge_port = pkt->toaddr() % torus_->concentration() + inj_port_offset_;
      hdr->deadlock_vc = 0;
      return;
    }

    switch(hdr->stage_number){
      case initial_stage: {
        SwitchId inter = getIntermediate(pkt, ej_addr);
        header ugal; minimalRoute(inter, &ugal);
        header min; minimalRoute(ej_addr, &min);
        int min_dist = torus_->minimalDistance(my_addr_, ej_addr);
        int ugal_dist = torus_->minimalDistance(my_addr_, inter) +
                        torus_->minimalDistance(inter, ej_addr);

        int ugal_metric = netsw_->queueLength(ugal.edge_port, all_vcs) * ugal_dist;
        int min_metric = netsw_->queueLength(min.edge_port, all_vcs) * min_dist;

        if (ugal_metric < min_metric){
          hdr->dest_switch = inter;
          hdr->stage_number = valiant_stage;
          rter_debug("chose intermediate %d for pkt %s on %d:%d",
                     inter, pkt->toString().c_str(),
                     int(hdr->edge_port), int(hdr->deadlock_vc));
        } else {
          hdr->dest_switch = ej_addr;
          hdr->stage_number = minimal_only_stage;
          rter_debug("chose minimal %d for pkt %s on %d:%d",
                     ej_addr, pkt->toString().c_str(), int(hdr->edge_port), int(hdr->deadlock_vc));
        }
        minimalRoute(hdr->dest_switch, hdr);
        break;
      }
      case valiant_stage: {
        if (my_addr_ == hdr->dest_switch){
          hdr->stage_number = final_stage;
          rter_debug("reached intermediate %d for pkt %s",
                     int(hdr->dest_switch), pkt->toString().c_str());
        } else {
          rter_debug("route to intermediate %d for pkt %s on %d:%d",
                     int(hdr->dest_switch), pkt->toString().c_str(),
                     int(hdr->edge_port), int(hdr->deadlock_vc));
          minimalRoute(hdr->dest_switch, hdr);
          break;
        }
      }
      case final_stage: {
        minimalRoute(ej_addr, hdr);
        hdr->dest_switch = ej_addr;
        hdr->deadlock_vc += 2; //final stage vc moves
        rter_debug("route to final %d for pkt %s on %d:%d",
                   int(hdr->dest_switch), pkt->toString().c_str(),
                   int(hdr->edge_port), int(hdr->deadlock_vc));
        break;
      }
      case minimal_only_stage: {
        rter_debug("route to minimal %d for pkt %s on %d:%d",
                   int(hdr->dest_switch), pkt->toString().c_str(),
                   int(hdr->edge_port), int(hdr->deadlock_vc));
        minimalRoute(ej_addr, hdr);
        break;
      }
    }
  }


};

/**
 * @brief The TorusPositiveRouter class
 * Simple router that routes all traffic in the same direction, making
 * it easier to create specific congestion scenarios
 */
class TorusPositiveRouter : public Router {
 public:
  struct header : public Packet::Header {
     char wrapped : 1;
     int num_hops;
  };

  SST_ELI_REGISTER_DERIVED(
   Router,
   TorusPositiveRouter,
   "macro",
   "torus_positive",
   SST_ELI_ELEMENT_VERSION(1,0,0),
   "a routing algorithm that routes all traffic in the +direction")

  TorusPositiveRouter(SST::Params& params,
                      Topology* top, NetworkSwitch* netsw)
   : Router(params, top, netsw)
  {
    torus_ = safe_cast(Torus, top);
    inj_port_offset_ = 2*torus_->ndimensions();
  }

  std::string toString() const override {
    return "torus positive router";
  }

  int numVC() const override {
    return 2;
  }

 void minimalRoute(SwitchId dst, header* hdr){
   SwitchId src = my_addr_;
   auto& dimensions_ = torus_->dimensions();
   int div = 1;
   int ndim = dimensions_.size();
   for (int i=0; i < ndim; ++i){
     int srcX = (src / div) % dimensions_[i];
     int dstX = (dst / div) % dimensions_[i];
     int nextX = (srcX + 1) % dimensions_[i];
     int maxX = dimensions_[i] - 1;
     if (srcX != dstX){
       if (srcX == maxX){
         //we are wrapping around
         hdr->wrapped = 1;
         hdr->deadlock_vc = 1;
       } else { //nothing special
         hdr->deadlock_vc = hdr->wrapped ? 1 : 0;
       }
       if (nextX == dstX){
         hdr->wrapped = 0; //reset for next dim
       }
       hdr->edge_port = torus_->convertToPort(i, Torus::pos);
       return;
     }
     div *= dimensions_[i];
   }
   sprockit::abort("torus: failed to route correctly on torus");
 }

  void route(Packet* pkt) override {
    auto* hdr = pkt->rtrHeader<header>();
    SwitchId ej_addr = pkt->toaddr() / torus_->concentration();
    if (ej_addr == my_addr_){
      hdr->edge_port = pkt->toaddr() % torus_->concentration() + inj_port_offset_;
      hdr->deadlock_vc = 0;
      return;
    }
    minimalRoute(ej_addr, hdr);
  }

 protected:
  Torus* torus_;
  int inj_port_offset_;
};

}
}
