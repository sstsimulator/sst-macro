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

#include <sstmac/hardware/router/minimal_routing.h>
#include <sstmac/hardware/router/multipath_routing.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/topology/torus.h>
#include <sstmac/hardware/topology/cascade.h>
#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/hardware/topology/hypercube.h>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sstmac/hardware/topology/butterfly.h>
#include <sstmac/hardware/topology/fully_connected.h>
#include <sstmac/hardware/pisces/pisces_switch.h>
#include <sstmac/hardware/pisces/pisces_stats.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

minimal_router::minimal_router(sprockit::sim_parameters* params, topology* top,
                               network_switch* netsw) :
  router(params, top, netsw)
{
}

void
minimal_router::route(packet *pkt)
{
  packet::path& path = pkt->current_path();
  switch_id sid = find_ejection_site(pkt->toaddr(), path);
  if (sid == my_addr_){
    path.vc = 0;
    rter_debug("Ejecting %s from switch %d on port %d",
               pkt->to_string().c_str(), sid, path.outport());
  } else {
    route_to_switch(sid, pkt);
    rter_debug("Routing %s to switch %d on port %d",
               pkt->to_string().c_str(), sid, path.outport());
  }
}

class torus_minimal_router : public minimal_router {
 public:
  struct header : public packet::header {
     char crossed_timeline : 1;
  };

  FactoryRegister("torus_minimal", router, torus_minimal_router,
              "a routing algorithm for minimal routing on the torus")

  torus_minimal_router(sprockit::sim_parameters* params,
                          topology* top, network_switch* netsw)
    : minimal_router(params, top, netsw)
  {
    torus_ = safe_cast(torus, top);
  }

  std::string to_string() const override {
    return "torus minimal router";
  }

  int num_vc() const override {
    return 2;
  }

  void route_to_switch(switch_id sid, packet* pkt) override {
    packet::path& path = pkt->current_path();
    torus::route_type_t ty = torus_->torus_route(my_addr_, sid, path);
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

 private:
  torus* torus_;
};

class cascade_minimal_router : public minimal_router {
  struct header : public packet::header {
     char num_hops : 3;
     char num_group_hops : 2;
  };
 public:
  FactoryRegister("cascade_minimal",
              router, cascade_minimal_router,
              "router implementing minimal routing for cascade")

  cascade_minimal_router(sprockit::sim_parameters* params, topology *top,
                         network_switch *netsw)
    : minimal_router(params, top, netsw)
  {
    cascade_ = safe_cast(cascade, top);
  }

  std::string to_string() const override {
    return "cascade minimal router";
  }

  int num_vc() const override {
    return 2;
  }

 private:
  void route_to_switch(switch_id sid, packet* pkt) override {
    packet::path& path = pkt->current_path();
    cascade_->minimal_route_to_switch(this, my_addr_, sid, path);
    auto hdr = pkt->get_header<header>();
    path.vc = hdr->num_group_hops;
    if (cascade_->is_global_port(path.outport())){
      ++hdr->num_group_hops;
    }
    ++hdr->num_hops;
  }

  cascade* cascade_;
};

class fat_tree_minimal_router : public minimal_router {
 public:
  FactoryRegister("fat_tree_minimal",
              router, fat_tree_minimal_router,
              "router implementing minimal routing for fat-tree")

  fat_tree_minimal_router(sprockit::sim_parameters* params,
                          topology *top,
                          network_switch *netsw)
    : minimal_router(params, top, netsw)
  {
    tree_ = safe_cast(fat_tree, top);
  }

  std::string to_string() const override {
    return "fat-tree minimal router";
  }

  int num_vc() const override {
    return 1;
  }

 private:
  void route_to_switch(switch_id sid, packet* pkt) override {
    packet::path& path = pkt->current_path();
    tree_->minimal_route_to_switch(my_addr_, sid, path);
    path.vc = 0;
  }

  fat_tree* tree_;

};

class tapered_fat_tree_minimal_router : public minimal_router {
 public:
  FactoryRegister("tapered_fat_tree_minimal",
              router, tapered_fat_tree_minimal_router,
              "router implementing minimal routing for cascade")

  tapered_fat_tree_minimal_router(sprockit::sim_parameters* params, topology *top,
                         network_switch *netsw)
    : minimal_router(params, top, netsw)
  {
    tree_ = safe_cast(tapered_fat_tree, top);
  }

  std::string to_string() const override {
    return "tapered fat tree minimal router";
  }

  int num_vc() const override {
    return 1;
  }

 private:
  void route_to_switch(switch_id sid, packet* pkt) override {
    packet::path& path = pkt->current_path();
    tree_->minimal_route_to_switch(my_addr_, sid, path);
    path.vc = 0;
  }

  tapered_fat_tree* tree_;

};

class hypercube_minimal_router : public minimal_router {
 public:
  FactoryRegister("hypercube_minimal",
              router, hypercube_minimal_router,
              "router implementing minimal routing for hypercube")

  hypercube_minimal_router(sprockit::sim_parameters* params, topology *top,
                         network_switch *netsw)
    : minimal_router(params, top, netsw)
  {
    cube_ = safe_cast(hypercube, top);
  }

  std::string to_string() const override {
    return "hypercube minimal router";
  }

  int num_vc() const override {
    return 1;
  }

 private:
  void route_to_switch(switch_id sid, packet* pkt) override {
    packet::path& path = pkt->current_path();
    cube_->minimal_route_to_switch(my_addr_, sid, path);
    path.vc = 0;
  }

  hypercube* cube_;
};

class fully_connected_minimal_router : public minimal_router {
 public:
  FactoryRegister("fully_connected_minimal",
              router, fully_connected_minimal_router,
              "router implementing minimal routing for fully connected")

  fully_connected_minimal_router(sprockit::sim_parameters* params, topology *top,
                         network_switch *netsw)
    : minimal_router(params, top, netsw)
  {
    full_ = safe_cast(fully_connected, top);
  }

  std::string to_string() const override {
    return "fully connected minimal router";
  }

  int num_vc() const override {
    return 1;
  }

 private:
  void route_to_switch(switch_id sid, packet* pkt) override {
    packet::path& path = pkt->current_path();
    full_->minimal_route_to_switch(my_addr_, sid, path);
    path.vc = 0;
  }

  fully_connected* full_;
};

class butterfly_minimal_router : public minimal_router {
 public:
  FactoryRegister("butterfly_minimal",
              router, butterfly_minimal_router,
              "router implementing minimal routing for fully connected")

  butterfly_minimal_router(sprockit::sim_parameters* params, topology *top,
                         network_switch *netsw)
    : minimal_router(params, top, netsw)
  {
    butt_ = safe_cast(butterfly, top);
  }

  std::string to_string() const override {
    return "butterfly minimal router";
  }

  int num_vc() const override {
    return 1;
  }

 private:
  void route_to_switch(switch_id sid, packet* pkt) override {
    packet::path& path = pkt->current_path();
    butt_->minimal_route_to_switch(my_addr_, sid, path);
    path.vc = 0;
  }

  butterfly* butt_;
};

/**
class multipath_dragonfly_minimal_router : public multipath_router<dragonfly_minimal_router> {
  FactoryRegister("dragonfly_minimal_multipath", router, multipath_dragonfly_minimal_router)
 public:
  multipath_dragonfly_minimal_router(sprockit::sim_parameters* params, topology* top, network_switch* netsw) :
   multipath_router(params,top,netsw){}
};
*/

class multipath_torus_minimal_router : public multipath_router<torus_minimal_router> {
  FactoryRegister("torus_minimal_multipath", router, multipath_torus_minimal_router)
 public:
  multipath_torus_minimal_router(sprockit::sim_parameters* params, topology* top, network_switch* netsw) :
   multipath_router(params,top,netsw){}
};

}
}
