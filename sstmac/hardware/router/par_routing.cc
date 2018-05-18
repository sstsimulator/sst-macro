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


}
}
