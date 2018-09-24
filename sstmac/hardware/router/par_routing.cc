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

#include <sstmac/hardware/router/par_routing.h>
#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/topology/dragonfly_plus.h>
#include <sstmac/hardware/topology/hypercube.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

par_router::par_router(sprockit::sim_parameters *params, topology *top, network_switch *netsw)
  :  ugal_router(params, top, netsw)
{
}

bool
par_router::route_common(packet* pkt)
{
  uint16_t dir;
  switch_id ej_addr = top_->netlink_to_ejection_switch(pkt->toaddr(), dir);
  if (ej_addr == my_addr_){
    pkt->current_path().outport() = dir;
    pkt->current_path().vc = 0;
    return true;
  }

  auto hdr = pkt->get_header<header>();
  switch(hdr->stage_number){
    case initial_stage: {
      switch_id middle_switch = random_intermediate_switch(addr(), ej_addr, netsw_->now().ticks());
      packet::path orig = output_path(ej_addr);
      packet::path valiant = output_path(middle_switch);
      bool go_valiant = switch_paths(ej_addr, middle_switch, orig.outport(), valiant.outport());
      if (go_valiant){
        pkt->set_dest_switch(middle_switch);
        pkt->current_path() = valiant;
        hdr->stage_number = valiant_stage;
      } else {
        pkt->set_dest_switch(ej_addr);
        pkt->current_path() = orig;
      }
      break;
    }
    case valiant_stage: {
      if (my_addr_ == pkt->dest_switch()){
        pkt->set_dest_switch(ej_addr);
        hdr->stage_number = final_stage;
        break;
      } else {
        break;
      }
    }
    case final_stage: {
      break;
    }
  }
  return false;
}

class dragonfly_par_router : public par_router {
  struct header : public par_router::header {
     uint8_t num_hops : 3;
     uint8_t num_group_hops : 2;
  };
 public:
  FactoryRegister("dragonfly_par",
              router, dragonfly_par_router,
              "router implementing PAR for dragonfly")

  std::string to_string() const override {
    return "dragonfly PAR router";
  }

  dragonfly_par_router(sprockit::sim_parameters* params, topology *top,
                       network_switch *netsw)
    : par_router(params, top, netsw)
  {
    dfly_ = safe_cast(dragonfly, top);
  }

  void route(packet *pkt) override {
    bool eject = route_common(pkt);
    if (eject) return;

    packet::path& path = pkt->current_path();
    dfly_->minimal_route_to_switch(my_addr_, pkt->dest_switch(), path);
    auto hdr = pkt->get_header<header>();
    path.vc = hdr->num_hops;
    ++hdr->num_hops;
  }

  int num_vc() const override {
    return 8;
  }

  packet::path output_path(switch_id sid) const override {
    packet::path p;
    dfly_->minimal_route_to_switch(my_addr_, sid, p);
    return p;
  }

 private:
  dragonfly* dfly_;

};

class dragonfly_plus_par_router : public router {
  struct header : public ugal_router::header {};
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
    : router(params, top, netsw)
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
    switch_id ej_addr = dfly_->netlink_to_ejection_switch(pkt->toaddr(), dir);
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

class hypercube_par_router : public router {
  struct header : public ugal_router::header {
    uint8_t nhops : 3;
    uint8_t dstX : 6;
    uint8_t dstY : 6;
    uint8_t dstZ : 6;
    uint16_t ejPort;
  };
 public:
  static const char initial_stage = 0;
  static const char minimal_stage = 1;
  static const char valiant_stage = 2;
  static const char final_stage = 3;

  FactoryRegister("hypercube_par",
              router, hypercube_par_router,
              "router implementing PAR for hypercube/hyperX")

  std::string to_string() const override {
    return "hypercube PAR router";
  }

  hypercube_par_router(sprockit::sim_parameters* params, topology *top,
                       network_switch *netsw)
    : router(params, top, netsw)
  {
    cube_ = safe_cast(hypercube, top);
    auto coords = cube_->switch_coords(my_addr_);
    if (coords.size() != 3){
      spkt_abort_printf("PAR routing currently only valid for 3D");
    }
    myX_ = coords[0];
    myY_ = coords[1];
    myZ_ = coords[2];
    auto& dims = cube_->dimensions();
    x_ = dims[0];
    y_ = dims[1];
    z_ = dims[2];
  }

  int x_random_number(uint32_t seed){
    int x = myX_;
    uint32_t attempt = 0;
    while (x == myX_){
      x = random_number(x_, attempt, seed);
      ++attempt;
    }
    return x;
  }

  int y_random_number(uint32_t seed){
    int y = myY_;
    uint32_t attempt = 1; //start 1 to better scramble
    while (y == myY_){
      y = random_number(y_, attempt, seed);
      ++attempt;
    }
    return y;
  }

  int z_random_number(uint32_t seed){
    int z = myZ_;
    uint32_t attempt = 2; //start 2 to better scramble
    while (z == myZ_){
      z = random_number(z_, attempt, seed);
      ++attempt;
    }
    return z;
  }

  void route(packet *pkt) override {
    auto hdr = pkt->get_header<header>();
    auto& path = pkt->current_path();

    switch(hdr->stage_number){
    case initial_stage: {
      uint16_t dir;
      switch_id ej_addr = cube_->netlink_to_ejection_switch(pkt->toaddr(), dir);
      auto coords = cube_->switch_coords(ej_addr);
      hdr->dstX = coords[0];
      hdr->dstY = coords[1];
      hdr->dstZ = coords[2];
      hdr->ejPort = dir;
      hdr->stage_number = minimal_stage;
      path.vc = 0;
      if (ej_addr == my_addr_){
        path.set_outport(hdr->ejPort);
        return;
      }
    }
    case minimal_stage: {
     //have to decide if we want to route valiantly
       uint32_t seed = netsw_->now().ticks();
       int minimalPort;
       int valiantPort;
       coordinates coords(3);
       coords[0] = myX_ == hdr->dstX ? myX_ : x_random_number(seed);
       coords[1] = myY_ == hdr->dstY ? myY_ : y_random_number(seed);
       coords[2] = myZ_ == hdr->dstZ ? myZ_ : z_random_number(seed);
       if (myX_ != hdr->dstX){
         minimalPort = hdr->dstX;
         valiantPort = coords[0];
       } else if (myY_ != hdr->dstY){
         minimalPort = hdr->dstY + x_;
         valiantPort = coords[1] + x_;
       } else if (myZ_ != hdr->dstZ){
         minimalPort = hdr->dstZ + x_ + y_;
         valiantPort = coords[2] + x_ + y_;
       } else {
          //oh - um - eject
         path.set_outport(hdr->ejPort);
         path.vc = 0;
         return;
       }
      int minLength = netsw_->queue_length(minimalPort);
      int valLength = netsw_->queue_length(valiantPort) * 2;
      if (minLength <= valLength){
        path.set_outport(minimalPort);
      } else {
        switch_id inter = cube_->switch_addr(coords);
        path.set_outport(valiantPort);
        pkt->set_dest_switch(inter);
        hdr->stage_number = valiant_stage;
      }
      break;
    }
    case valiant_stage: {
      if (my_addr_ != pkt->dest_switch()){
        auto coords = cube_->switch_coords(pkt->dest_switch());
        if (coords[0] != myX_){
          path.set_outport(coords[0]);
        } else if (coords[1] != myY_){
          path.set_outport(coords[1]+x_);
        } else {
          path.set_outport(coords[2]+x_+y_);
        }
        break;
      }
      hdr->stage_number = final_stage;
      //otherwise fall through
    }
    case final_stage: {
      int port;
      if (hdr->dstX != myX_){
        port = hdr->dstX;
      } else if (hdr->dstY != myY_){
        port = hdr->dstY+x_;
      } else if (hdr->dstZ != myZ_) {
        port = hdr->dstZ+x_+y_;
      } else {
        //oh - um - eject
       path.set_outport(hdr->ejPort);
       path.vc = 0;
       return;
      }
      path.set_outport(port);
    }
    }


    path.vc = hdr->nhops;
    hdr->nhops++;
  }

  int num_vc() const override {
    return 6;
  }

 private:
  hypercube* cube_;
  int myX_;
  int myY_;
  int myZ_;
  int x_;
  int y_;
  int z_;

};


}
}
