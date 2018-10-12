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
#include <sstmac/hardware/topology/topology.h>
#include <sprockit/util.h>
#include <sprockit/delete.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sstmac/hardware/topology/fully_connected.h>

RegisterDebugSlot(router);
RegisterDebugSlot(routing);

RegisterNamespaces("router");

RegisterKeywords(
{ "router", "the type of routing to perform in the network" },
{ "ugal_threshold", "in UGAL, the min number of hops before allowing MIN-UGAL change"},
);

namespace sstmac {
namespace hw {

router::router(sprockit::sim_parameters* params, topology *top, network_switch *sw)
 : top_(top), netsw_(sw), rng_(nullptr)
{
  my_addr_ = switch_id(params->get_int_param("id"));
  std::vector<RNG::rngint_t> seeds(2);
  seeds[0] = 42;
  if (params->has_param("seed")) {
    seed_ = params->get_long_param("seed");
    seeds[1] = seed_;
    debug_seed_ = true;
  } else {
    seeds[1] = time(NULL);
    debug_seed_ = false;
  }
  rng_ = RNG::MWC::construct(seeds);
}

bool
router::switch_paths(
  int orig_distance,
  int new_distance,
  int orig_port,
  int new_port) const
{
  int orig_queue_length = netsw_->queue_length(orig_port);
  int new_queue_length = netsw_->queue_length(new_port);
  int orig_weight = orig_queue_length * orig_distance;
  int valiant_weight = new_queue_length * new_distance;
  return valiant_weight < orig_weight;
}

uint32_t
router::random_number(uint32_t max, uint32_t attempt, uint32_t seed) const
{
  if (debug_seed_){
    std::vector<RNG::rngint_t> seeds(2);
    uint32_t time = seed;
    seeds[1] = seed_ * (time+31) << (attempt + 5);
    seeds[0] = (time+5)*7 + seeds[0]*attempt*42 + 3;
    rng_->vec_reseed(seeds);
  }
  return rng_->value_in_range(max);
}

router::~router()
{
  if (rng_) delete rng_;
}

void
router::compatibility_check() const
{
}

class fully_connected_minimal_router : public router {
 public:
  FactoryRegister("fully_connected_minimal",
              router, fully_connected_minimal_router,
              "router implementing minimal routing for fully connected")

  fully_connected_minimal_router(sprockit::sim_parameters* params, topology *top,
                         network_switch *netsw)
    : router(params, top, netsw)
  {
    full_ = safe_cast(fully_connected, top);
  }

  std::string to_string() const override {
    return "fully connected minimal router";
  }

  int num_vc() const override {
    return 1;
  }

  void route(packet *pkt) override {
    switch_id ej_addr = pkt->toaddr() / full_->concentration();
    if (ej_addr == my_addr_){
      pkt->current_path().outport() = pkt->toaddr() % full_->concentration();
      pkt->current_path().vc = 0;
      return;
    }

    packet::path& path = pkt->current_path();
    full_->minimal_route_to_switch(my_addr_, ej_addr, path);
    path.vc = 0;
  }

 private:
  fully_connected* full_;
};

}
}
