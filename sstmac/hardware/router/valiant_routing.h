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

#ifndef sstmac_hardware_network_topology_routing_VALIANT_ROUTER_H
#define sstmac_hardware_network_topology_routing_VALIANT_ROUTER_H

#include <sstmac/hardware/router/minimal_routing.h>

namespace sstmac {
namespace hw {

/**
 * @brief The valiant_router class
 * Enacpsulates a router that performs oblivious, global routing according to
 * "A Scheme for Fast Parallel Computation" by Valiant.
 */
class valiant_router : public minimal_router
{
  FactoryRegister("valiant", router, valiant_router,
              "router implementing valiant routing")
 public:
  valiant_router(sprockit::sim_parameters* params,
                 topology* top, network_switch* netsw,
                 routing::algorithm_t algo = routing::valiant);

  virtual ~valiant_router() { }

  virtual void route(packet* pkt) override;

  virtual std::string to_string() const override {
    return "valiant";
  }

  /**
    The topology object specifies a virtual channel based purely on geometry.
    However, the final virtual channel depends on both geometry and
    routing algorithm.  In this case, we need a separate set of
    virtual channels depending on whether we are in the first
    stage (routing to the intermediate switch) or the second stage
    (routing to the final switch).
    @param topology_vc The geometry-specific virtual channel
    @return The first stage virtual channel
  */
  virtual int first_stage_vc(int topology_vc) const {
    return 2*topology_vc;
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
  virtual int second_stage_vc(int topology_vc) const {
    return 2*topology_vc + 1;
  }

 protected:
  typedef enum {
    eject,
    minimal,
    intermediate_switch,
    final_node
  } next_action_t;

  /**
   * @brief intermediate_step
   * @param rtbl  The routing interface with path information
   * @param pkt   The packet corresponding to the structured_routable
   * @return The next action to take. It may be intermediate_switch or
   * final_node depending on whether the intermediate switch has been
   * reached or not.
   */
  next_action_t intermediate_step(routable* rtbl, packet* pkt);

  /**
   Different for pure valiant and UGAL.
   */
  virtual next_action_t initial_step(routable* rtbl, packet* pkt);

  /**
   * @brief next_routing_stage
   * Figure out which action is required for the packet
   * @param pkt The packet being routed
   * @return It may be intermediate_switch or
   * final_node depending on whether the intermediate switch has been
   * reached or not. Or it may be minimal if the packet is still on
   * the minimal path and has not switched to Valiant.
   */
  next_action_t next_routing_stage(packet* pkt);

  /**
   * @brief configure_intermediate_path
   * Once the packet has switched to Valiant, compute a path to the
   * chosen intermediate switch
   * @param path
   */
  void configure_intermediate_path(routable::path& path);

  /**
   * @brief configure_final_path
   * Once the packet has switched to Valiant and reached the intermediate switch,
   * compute a path to the final ejection switch
   * @param path
   */
  void configure_final_path(routable::path& path);

  /**
   * @brief route_valiant
   * Figure out if in intermediate or final stage and make appropriate routing computation
   * @param pkt
   */
  void route_valiant(packet* pkt);

};

}
}

#endif // VALIANT_ROUTER_H