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

#ifndef sstmac_hardware_network_topology_routing_UGAL_ROUTING_H
#define sstmac_hardware_network_topology_routing_UGAL_ROUTING_H

#include <sstmac/hardware/router/valiant_routing.h>

namespace sstmac {
namespace hw {

/**
 * @brief The ugal_router class
 * Encapsulates a router that performs Univeral Globally Adaptive Load-balanced
 * routing as described in PhD Thesis "Load-balanced in routing in interconnection networks"
 * by A Singh.
 */
class ugal_router :
  public valiant_router
{
  FactoryRegister("ugal", router, ugal_router,
              "router implementing ugal congestion-aware routing")
 public:
  ugal_router(sprockit::sim_parameters* params, topology* top, network_switch* netsw);

  std::string to_string() const override {
    return "ugal";
  }

 protected:
  next_action_t initial_step(
    routable* rtbl, packet* pkt) override;

  /**
    The topology object specifies a virtual channel based purely on geometry.
    However, the final virtual channel depends on both geometry and
    routing algorithm.  In this case, we need a separate set of
    virtual channels depending on whether we are in the first
    stage (routing to the intermediate switch) or the second stage
    (routing to the final switch).
    @param topology_vc The geometry-specific virtual channel
    @return The second stage virtual channel
  */
  int first_stage_vc(int topology_vc) const override {
    return 3 * topology_vc;
  }

  /**
    The topology object specifies a virtual channel based purely on geometry.
    However, the final virtual channel depends on both geometry and
    routing algorithm.  In this case, we need a separate set of
    virtual channels depending on whether we are in the first
    stage (routing to the intermediate switch) or the second stage
    (routing to the final switch).
    @param topology_vc The geometry-specific virtual channel
    @return The second stage virtual channel
  */
  int second_stage_vc(int topology_vc) const override {
    return 3 * topology_vc + 1;
  }

  /**
    In adition to the valiant stages, ugal has a minimal stage.
    This requires another set of virtual channels.
    @param topology_vc The geometry-specific virtual channel
    @return The zero stage virtual channel
  */
  int zero_stage_vc(int topology_vc) const {
    return 3 * topology_vc + 2;
  }

 protected:
  int val_threshold_;
  int val_preference_factor_;


};

}
}


#endif // UGAL_ROUTING_H