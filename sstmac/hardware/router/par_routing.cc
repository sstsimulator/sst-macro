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

#include <sstmac/hardware/router/par_routing.h>
#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

par_router::par_router(sprockit::sim_parameters *params, topology *top, network_switch *netsw)
  :  ugal_router(params, top, netsw)
{
}

void
par_router::route(packet *pkt)
{
  uint16_t dir;
  routable* rtbl = pkt->interface<routable>();
  switch_id ej_addr = top_->netlink_to_ejection_switch(pkt->toaddr(), dir);
  if (ej_addr == my_addr_){
    rtbl->current_path().outport() = dir;
    rtbl->current_path().vc = 0;
    return;
  }

  auto hdr = rtbl->current_path().header<header>();
  switch(hdr->stage_number){
    case initial_stage: {
      switch_id middle_switch = top_->random_intermediate_switch(addr(), ej_addr, netsw_->now().ticks());
      routable::path orig;
      routable::path valiant;
      bool go_valiant = switch_paths(ej_addr, middle_switch, orig, valiant);
      if (go_valiant){
        debug_printf(sprockit::dbg::router,
          "Router %s selected random intermediate switch %s for message %s",
            top_->switch_label(my_addr_).c_str(),
            top_->switch_label(rtbl->dest_switch()).c_str(),
            pkt->to_string().c_str());
        rtbl->set_dest_switch(middle_switch);
        rtbl->current_path() = valiant;
        hdr->stage_number = valiant_stage;
      } else {
        rtbl->set_dest_switch(ej_addr);
        rtbl->current_path() = orig;
      }
      break;
    }
    case valiant_stage: {
      if (my_addr_ != rtbl->dest_switch()){
        top_->minimal_route_to_switch(my_addr_, rtbl->dest_switch(), rtbl->current_path());
        break;
      } //else - fall through to final stage
      hdr->stage_number = final_stage;
    }
    case final_stage: {
      top_->minimal_route_to_switch(my_addr_, ej_addr, rtbl->current_path());
      break;
    }
    break;
  }
  topology_route(rtbl);
}

class dragonfly_par_router : public par_router {
  struct header : public par_router::header {
     char num_hops : 3;
     char num_group_hops : 2;
  };
 public:
  FactoryRegister("par_dragonfly",
              router, dragonfly_par_router,
              "router implementing PAR for dragonfly")

  dragonfly_par_router(sprockit::sim_parameters* params, topology *top,
                       network_switch *netsw)
    : par_router(params, top, netsw)
  {
    dfly_ = safe_cast(dragonfly, top);
  }

 private:
  int num_vc() const override {
    return 5;
  }

  void topology_route(routable* rtbl) override {
    routable::path& path = rtbl->current_path();
    auto hdr = path.header<header>();
    path.vc = hdr->num_hops;
    if (dfly_->is_global_port(path.outport())){
      ++hdr->num_group_hops;
    }
    ++hdr->num_hops;
  }

  dragonfly* dfly_;
};


}
}
