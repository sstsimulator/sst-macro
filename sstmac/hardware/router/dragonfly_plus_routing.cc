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
#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/hardware/topology/dragonfly_plus.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <cmath>

#define ftree_rter_debug(...) \
  rter_debug("fat tree: %s", sprockit::sprintf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

class DragonflyPlusAlltoallMinimalRouter : public Router {
 public:
  struct header : public Packet::Header {};

  SST_ELI_REGISTER_DERIVED(
    Router,
    DragonflyPlusAlltoallMinimalRouter,
    "macro",
    "dragonfly_plus_alltoall_minimal",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "router implementing minimal routing for dragonfly+")

  DragonflyPlusAlltoallMinimalRouter(SST::Params& params, Topology *top,
                         NetworkSwitch *netsw)
    : Router(params, top, netsw)
  {
    dfly_ = safe_cast(DragonflyPlus, top);
    num_leaf_switches_ = dfly_->g() * dfly_->a();
    //stagger by switch id
    rotater_ = (my_addr_) % dfly_->a();

    covering_ = dfly_->h() / (dfly_->g() - 1);
    if (covering_ == 0){
      spkt_abort_printf("dragonfly+ minimal router for alltoall wiring does"
                        " not have full covering");
    }

    int mod = dfly_->h() % (dfly_->g() - 1);
    if (mod != 0){
      spkt_abort_printf("dragonfly+ group connections h=%d is not evenly divided by N-1 for N=%d groups",
                        dfly_->h(), dfly_->g());
    }

    grp_rotaters_.resize(dfly_->g());
    for (int i=0; i < dfly_->g(); ++i){
      grp_rotaters_[i] = 0;
    }

    my_g_ = (my_addr_%num_leaf_switches_) / dfly_->a();
    my_row_ = my_addr_ / num_leaf_switches_;


    std::vector<int> connected;
    int my_a = dfly_->computeA(my_addr_);
    dfly_->groupWiring()->connectedRouters(my_a, my_g_, connected);
    for (int p=0; p < connected.size(); ++p){
      int my_expected_g = p / covering_;
      if (my_expected_g >= my_g_){
        ++my_expected_g;
      }
      int dst = connected[p];
      int actual_g = dfly_->computeG(dst);
      if (my_expected_g != actual_g){
        spkt_abort_printf("Router %d expected group %d on port %d, but got group %d",
                          my_addr_, my_expected_g, p, actual_g);
      }
    }

    static_route_ = params.find<bool>("static", false);
  }

  int numVC() const override {
    return 1;
  }

  std::string toString() const override {
    return "dragonfly+ minimal circulant router";
  }

  void route(Packet *pkt) override {
    auto* hdr = pkt->rtrHeader<header>();
    SwitchId ej_addr = pkt->toaddr() / dfly_->concentration();
    if (ej_addr == my_addr_){
      hdr->edge_port = pkt->toaddr() % dfly_->concentration() + dfly_->a();
      hdr->deadlock_vc = 0;
      return;
    }

    int dstG = (ej_addr % num_leaf_switches_) / dfly_->a();
    if (my_row_ == 0){
      if (static_route_){
        hdr->edge_port = ej_addr % dfly_->a();
      } else {
        hdr->edge_port = rotater_;
        rotater_ = (rotater_ + 1) % dfly_->a();
      }
    } else if (my_g_ == dstG){
      int dstA = ej_addr % dfly_->a();
      hdr->edge_port = dstA;
    } else {
      int rotater;
      int grpOffset = my_g_ < dstG ? dstG - 1 : dstG;
      if (static_route_){
        rotater = ej_addr % covering_;
      } else {
        rotater = grp_rotaters_[dstG];
        grp_rotaters_[dstG] = (grp_rotaters_[dstG] + 1) % covering_;
      }
      int port = grpOffset*covering_ + rotater + dfly_->a();
      hdr->edge_port = port;
    }
    hdr->deadlock_vc = 0;
  }

 protected:
  int num_leaf_switches_;
  int rotater_;
  int my_g_;
  int my_row_;
  std::vector<int> grp_rotaters_;
  int covering_;
  DragonflyPlus* dfly_;
  bool static_route_;
};

class DragonflyPlusParRouter : public DragonflyPlusAlltoallMinimalRouter {
  struct header : public DragonflyPlusAlltoallMinimalRouter::header {
    uint8_t stage_number : 4;
  };
 public:
  SST_ELI_REGISTER_DERIVED(
    Router,
    DragonflyPlusParRouter,
    "macro",
    "dragonfly_plus_par",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "router implementing PAR for dragonfly+")

  static const char initial_stage = 0;
  static const char valiant_stage = 1;
  static const char final_stage = 2;

  std::string toString() const override {
    return "dragonfly+ PAR router";
  }

  DragonflyPlusParRouter(SST::Params& params, Topology *top,
                       NetworkSwitch *netsw)
    : DragonflyPlusAlltoallMinimalRouter(params, top, netsw)
  {
    dfly_ = safe_cast(DragonflyPlus, top);
    my_row_ = my_addr_ / dfly_->numLeafSwitches();
    my_g_ = (my_addr_ % dfly_->numLeafSwitches()) / dfly_->a();
    covering_ = dfly_->h() / (dfly_->g() - 1);
    grp_rotaters_.resize(dfly_->g());
    for (int i=0; i < dfly_->g(); ++i){
      grp_rotaters_[i] = 0;
    }
    up_rotater_ = 0;
    num_leaf_switches_ = dfly_->numLeafSwitches();
  }

  void route(Packet *pkt) override {
    SwitchId ej_addr = pkt->toaddr() / dfly_->concentration();
    auto hdr = pkt->rtrHeader<header>();
    if (my_row_ == 0){
      if (ej_addr == my_addr_){
        hdr->edge_port = pkt->toaddr() % dfly_->concentration() + dfly_->a();
        hdr->deadlock_vc = 0;
      } else {
        //gotta route up
        rter_debug("routing up on %d", up_rotater_);
        hdr->edge_port = up_rotater_;
        up_rotater_ = (up_rotater_ + 1) % dfly_->a();
        hdr->deadlock_vc = 0;
      }
    } else {
      int dstG = (ej_addr % num_leaf_switches_) / dfly_->a();
      if (my_g_ == dstG){
        //go down to the eject stage
        int dstA = ej_addr % dfly_->a();
        hdr->edge_port = dstA;
        //don't change the vc
        rter_debug("routing down to %d", int(hdr->edge_port));
      } else if (hdr->stage_number == valiant_stage) {
        int grpOffset = my_g_ < dstG ? dstG - 1 : dstG;
        int port = grpOffset*covering_ + grp_rotaters_[dstG] + dfly_->a();
        grp_rotaters_[dstG] = (grp_rotaters_[dstG] + 1) % covering_;
        hdr->edge_port = port;
        hdr->deadlock_vc = 1; //yep - here now
        rter_debug("continuing non-minimal path on %d", port);
      } else { 
        int minimalPort = grp_rotaters_[dstG] + dfly_->a()
            + ((my_g_ < dstG ? (dstG - 1) : dstG)*covering_);
        if (dfly_->g() > 2){
          //we must have an intermediate group - otherwise we just have minimal
          //we must make a ugal decision here
          int interG = my_g_;
          int valiantPort;
          uint32_t attempt = 0;
          uint32_t seed = netsw_->now().time.ticks();
          int numTestPorts = covering_ * dfly_->g();
          while (interG == my_g_ || interG == dstG){
            valiantPort = randomNumber(numTestPorts, attempt, seed);
            ++attempt;
            interG = valiantPort / covering_;
            //a little weird - we skip ports to ourselves
            if (interG > my_g_) valiantPort -= covering_;
          }
          valiantPort += dfly_->a(); //global ports offset by a


          int valiantMetric = 2*netsw_->queueLength(valiantPort, all_vcs);
          int minimalMetric = netsw_->queueLength(minimalPort, all_vcs);

          rter_debug("comparing minimal(%d) %d against non-minimal(%d) %d",
                     minimalPort, minimalMetric, valiantPort, valiantMetric);

          if (minimalMetric <= valiantMetric){
            hdr->edge_port = minimalPort;
            hdr->stage_number = final_stage;
            grp_rotaters_[dstG] = (grp_rotaters_[dstG] + 1) % covering_;
          } else {
            hdr->edge_port = valiantPort;
            hdr->stage_number = valiant_stage;
          }
        } else { //no intermediate group - must go minimal
          hdr->edge_port = minimalPort;
          hdr->stage_number = final_stage;
          grp_rotaters_[dstG] = (grp_rotaters_[dstG] + 1) % covering_;
        }
        hdr->deadlock_vc = 0;
      } 
    }
  }

  int numVC() const override {
    return 2;
  }

 private:
  DragonflyPlus* dfly_;
  int my_row_;
  int my_g_;
  int up_rotater_;
  int num_leaf_switches_;
  std::vector<int> grp_rotaters_;
  int covering_;

};


}
}
