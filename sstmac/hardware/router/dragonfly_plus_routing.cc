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
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <cmath>

#define ftree_rter_debug(...) \
  rter_debug("fat tree: %s", sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

class dragonfly_plus_alltoall_minimal_router : public router {
 public:
  struct header : public packet::header {

  };

  FactoryRegister("dragonfly_plus_alltoall_minimal",
              router, dragonfly_plus_alltoall_minimal_router,
              "router implementing minimal routing for dragonfly+")

  dragonfly_plus_alltoall_minimal_router(sprockit::sim_parameters* params, topology *top,
                         network_switch *netsw)
    : router(params, top, netsw)
  {
    dfly_ = safe_cast(dragonfly_plus, top);
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
    dfly_->group_wiring()->connected_routers(my_a, my_g_, connected);
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

    static_route_ = params->get_optional_bool_param("static", false);
  }

  int num_vc() const override {
    return 1;
  }

  std::string to_string() const override {
    return "dragonfly+ minimal circulant router";
  }

  void route(packet *pkt) override {
    uint16_t dir;
    switch_id ej_addr = dfly_->node_to_ejection_switch(pkt->toaddr(), dir);
    packet::path& path = pkt->current_path();
    if (ej_addr == my_addr_){
      path.set_outport(dir);
      path.vc = 0;
      return;
    }

    int dstG = (ej_addr % num_leaf_switches_) / dfly_->a();
    if (my_row_ == 0){
      if (static_route_){
        path.set_outport(ej_addr % dfly_->a());
      } else {
        path.set_outport(rotater_);
        rotater_ = (rotater_ + 1) % dfly_->a();
      }
    } else if (my_g_ == dstG){
      int dstA = ej_addr % dfly_->a();
      path.set_outport(dstA);
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
      path.set_outport(port);
    }
    path.vc = 0;
  }

 protected:
  int num_leaf_switches_;
  int rotater_;
  int my_g_;
  int my_row_;
  std::vector<int> grp_rotaters_;
  int covering_;
  dragonfly_plus* dfly_;
  bool static_route_;
};

class dragonfly_plus_par_router : public dragonfly_plus_alltoall_minimal_router {
  struct header : public dragonfly_plus_alltoall_minimal_router::header {
    uint8_t stage_number : 4;
  };
 public:
  static const char initial_stage = 0;
  static const char valiant_stage = 1;
  static const char final_stage = 2;

  FactoryRegister("dragonfly_plus_par",
              router, dragonfly_plus_par_router,
              "router implementing PAR for dragonfly+")

  std::string to_string() const override {
    return "dragonfly+ PAR router";
  }

  dragonfly_plus_par_router(sprockit::sim_parameters* params, topology *top,
                       network_switch *netsw)
    : dragonfly_plus_alltoall_minimal_router(params, top, netsw)
  {
    dfly_ = safe_cast(dragonfly_plus, top);
    my_row_ = my_addr_ / dfly_->num_leaf_switches();
    my_g_ = (my_addr_ % dfly_->num_leaf_switches()) / dfly_->a();
    covering_ = dfly_->h() / (dfly_->g() - 1);
    grp_rotaters_.resize(dfly_->g());
    for (int i=0; i < dfly_->g(); ++i){
      grp_rotaters_[i] = 0;
    }
    up_rotater_ = 0;
    num_leaf_switches_ = dfly_->num_leaf_switches();
  }

  void route(packet *pkt) override {
    uint16_t dir;
    switch_id ej_addr = dfly_->node_to_ejection_switch(pkt->toaddr(), dir);
    packet::path& path = pkt->current_path();
    auto hdr = pkt->get_header<header>();
    if (my_row_ == 0){
      if (ej_addr == my_addr_){
        pkt->current_path().outport() = dir;
        pkt->current_path().vc = 0;
      } else {
        //gotta route up
        path.set_outport(up_rotater_);
        up_rotater_ = (up_rotater_ + 1) % dfly_->a();
        path.vc = 0;
      }
    } else {
      int dstG = (ej_addr % num_leaf_switches_) / dfly_->a();
      if (my_g_ == dstG){
        //go down to the eject stage
        int dstA = ej_addr % dfly_->a();
        path.set_outport(dstA);
        //don't change the vc
      } else if (hdr->stage_number == valiant_stage) {
        int grpOffset = my_g_ < dstG ? dstG - 1 : dstG;
        int port = grpOffset*covering_ + grp_rotaters_[dstG] + dfly_->a();
        grp_rotaters_[dstG] = (grp_rotaters_[dstG] + 1) % covering_;
        path.set_outport(port);
        path.vc = 1; //yep - here now
      } else {
        //we must make a ugal decision here
        int interG = my_g_;
        int valiantPort;
        uint32_t attempt = 0;
        uint32_t seed = netsw_->now().ticks();
        int numTestPorts = covering_ * dfly_->g();
        while (interG == my_g_ || interG == dstG){
          valiantPort = random_number(numTestPorts, attempt, seed);
          ++attempt;
          interG = valiantPort / covering_;
          //a little weird - we skip ports to ourselves
          if (interG > my_g_) valiantPort -= covering_;
        }
        valiantPort += dfly_->a(); //global ports offset by a
        int minimalPort = grp_rotaters_[dstG] + dfly_->a()
            + ((my_g_ < dstG ? (dstG - 1) : dstG)*covering_);


        int valiantMetric = 2*netsw_->queue_length(valiantPort);
        int minimalMetric = netsw_->queue_length(minimalPort);

        if (minimalMetric <= valiantMetric){
          path.set_outport(minimalPort);
          hdr->stage_number = final_stage;
          grp_rotaters_[dstG] = (grp_rotaters_[dstG] + 1) % covering_;
        } else {
          path.set_outport(valiantPort);
          hdr->stage_number = valiant_stage;
        }
        path.vc = 0;
      }
    }
  }

  int num_vc() const override {
    return 2;
  }

 private:
  dragonfly_plus* dfly_;
  int my_row_;
  int my_g_;
  int up_rotater_;
  int num_leaf_switches_;
  std::vector<int> grp_rotaters_;
  int covering_;

};


}
}
