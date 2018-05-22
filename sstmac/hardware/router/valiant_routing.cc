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

#include <sstmac/hardware/router/valiant_routing.h>
#include <sstmac/hardware/router/multipath_routing.h>
#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/hardware/topology/torus.h>
#include <sstmac/hardware/topology/cascade.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

valiant_router::valiant_router(sprockit::sim_parameters *params, topology *top, network_switch *sw) :
  router(params, top, sw)
{
}

void
valiant_router::route(packet *pkt)
{
  uint16_t dir;
  switch_id ej_addr = top_->netlink_to_ejection_switch(pkt->toaddr(), dir);
  if (ej_addr == my_addr_){
    pkt->current_path().vc = 0;
    pkt->current_path().outport() = dir;
    return;
  }

  auto hdr = pkt->get_header<header>();
  switch(hdr->stage_number){
    case initial_stage: {
      switch_id middle_switch = random_intermediate_switch(addr(), ej_addr, netsw_->now().ticks());
      pkt->set_dest_switch(middle_switch);
      debug_printf(sprockit::dbg::router,
        "Router %s selected random intermediate switch %s for message %s",
          top_->switch_label(my_addr_).c_str(),
          top_->switch_label(pkt->dest_switch()).c_str(),
          pkt->to_string().c_str());
      hdr->stage_number = valiant_stage;
    }
    case valiant_stage: {
      if (pkt->dest_switch() != my_addr_) break;
      else hdr->stage_number = final_stage;
    }
    case final_stage: {
      pkt->set_dest_switch(ej_addr);
      if (ej_addr == my_addr_){
        pkt->current_path().outport() = dir;
        pkt->current_path().vc = 0;
        return;
      }
    }
    break;
  }
  topology_route(pkt);
}

class dragonfly_valiant_router : public valiant_router {
  struct header : public valiant_router::header {
     char num_hops : 3;
     char num_group_hops : 3;
  };
 public:
  FactoryRegister("dragonfly_valiant",
              router, dragonfly_valiant_router,
              "router implementing valint routing for dragonfly")

  dragonfly_valiant_router(sprockit::sim_parameters* params, topology *top,
                           network_switch *netsw)
    : valiant_router(params, top, netsw)
  {
    dfly_ = safe_cast(dragonfly, top);
  }

  std::string to_string() const override {
    return "dragonfly valiant";
  }

  int num_vc() const override {
    return 3;
  }

 private:
  void topology_route(packet* pkt) override {
    packet::path& path = pkt->current_path();
    dfly_->minimal_route_to_switch(my_addr_, pkt->dest_switch(), path);
    auto hdr = pkt->get_header<header>();
    path.vc = hdr->num_group_hops;
    if (dfly_->is_global_port(path.outport())){
      ++hdr->num_group_hops;
    }
    ++hdr->num_hops;
  }

  dragonfly* dfly_;
};

class cascade_valiant_router : public valiant_router {
  struct header : public valiant_router::header {
     char num_hops : 4;
     char num_group_hops : 3;
  };
 public:
  FactoryRegister("cascade_valiant",
              router, cascade_valiant_router,
              "router implementing UGAL routing for cascade")

  cascade_valiant_router(sprockit::sim_parameters* params, topology *top,
                         network_switch *netsw)
    : valiant_router(params, top, netsw)
  {
    cascade_ = safe_cast(cascade, top);
  }

  std::string to_string() const override {
    return "cascade valiant router";
  }

 private:
  int num_vc() const override {
    return 3;
  }

  void topology_route(packet* pkt) override {
    packet::path& path = pkt->current_path();
    cascade_->minimal_route_to_switch(this, my_addr_, pkt->dest_switch(), path);
    auto hdr = pkt->get_header<header>();
    path.vc = hdr->num_group_hops;
    if (cascade_->is_global_port(path.outport())){
      ++hdr->num_group_hops;
    }
    ++hdr->num_hops;
  }

  cascade* cascade_;
};

class torus_valiant_router : public valiant_router {
  struct header : public valiant_router::header {
     char crossed_timeline : 1;
  };
 public:
  FactoryRegister("torus_valiant",
              router, torus_valiant_router,
              "router implementing valiant routing for torus")

  torus_valiant_router(sprockit::sim_parameters* params,
                       topology *top, network_switch *netsw)
    : valiant_router(params, top, netsw)
  {
    torus_ = safe_cast(torus, top);
  }

  std::string to_string() const override {
    return "torus valiant router";
  }

  int num_vc() const override {
    return 4;
  }

 private:
  void topology_route(packet* pkt) override {
    packet::path& path = pkt->current_path();
    auto hdr = pkt->get_header<header>();
    torus::route_type_t ty = torus_->torus_route(my_addr_, pkt->dest_switch(), path);
    if (ty == torus::wrapped_around) hdr->crossed_timeline = 1;
    int min_vc = hdr->crossed_timeline ? 1 : 0;
    switch(hdr->stage_number){
      case initial_stage:
        spkt_abort_printf("valiant routing should not call routing for initial stage");
        break;
      case valiant_stage:
        path.vc = min_vc;
        break;
      case final_stage:
        path.vc = min_vc + 2;
        break;
    }
    if (ty == torus::new_dimension) hdr->crossed_timeline = 0;
  }

  torus* torus_;
};


class multipath_dragonfly_valiant_router : public multipath_router<dragonfly_valiant_router> {
  FactoryRegister("dragonfly_valiant_multipath", router, multipath_dragonfly_valiant_router)
 public:
  multipath_dragonfly_valiant_router(sprockit::sim_parameters* params,
                                     topology* top, network_switch* netsw) :
   multipath_router(params,top,netsw){}
};

class multipath_torus_valiant_router : public multipath_router<torus_valiant_router> {
  FactoryRegister("torus_valiant_multipath", router, multipath_torus_valiant_router)
 public:
  multipath_torus_valiant_router(sprockit::sim_parameters* params,
                                 topology* top, network_switch* netsw) :
   multipath_router(params,top,netsw){}
};

}
}
