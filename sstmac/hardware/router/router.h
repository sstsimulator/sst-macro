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

#ifndef SSTMAC_HARDWARE_NETWORK_ROUTING_ROUTER_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_ROUTING_ROUTER_H_INCLUDED

#include <sstmac/common/rng.h>
#include <sstmac/common/node_address.h>
#include <sstmac/common/event_manager_fwd.h>
#include <sstmac/hardware/common/packet.h>

#include <sprockit/debug.h>
#include <sprockit/factories/factory.h>

#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/switch/network_switch_fwd.h>


DeclareDebugSlot(router)

#define rter_debug(...) \
  debug_printf(sprockit::dbg::router, \
    "Router on switch %d: %s", int(my_addr_), \
    sprockit::printf(__VA_ARGS__).c_str())


namespace sstmac {
namespace hw {

/**
  @class router
  Class that computes the next step a message should taken in traversing
  the network.  This performs routing operations only and is not actually
  a 'component' in the network - those are switches.  Switch and router
  are not synonymous in SST/macro.  All switches have routers.
*/
class Router : public sprockit::printable
{
  DeclareFactory(Router, Topology*, NetworkSwitch*)
 public:
  /**
   * @brief route Makes a routing decision for the packet.
   * All routing decisions should be stored on the packet object itself.
   * @param pkt
   */
  virtual void route(Packet* pkt) = 0;

  virtual ~Router();

  NetworkSwitch* get_switch() const {
    return netsw_;
  }

  /**
   * @brief addr
   * @return
   */
  SwitchId addr() const {
    return my_addr_;
  }

  Topology* topology() const {
    return top_;
  }

  /**
   * @brief max_num_vc
   * @return The maximum number of virtual channels the router must maintain
   *         to implement all possible routing algorithms
   */
  virtual int numVC() const = 0;

  /**
   * @brief random_number
   * @param max     Select number [0,max) exclusive
   * @param attempt Optional argument to reseed if calling from a loop
   * @param seed    Optional seed for seeding if in debug mode
   * @return
   */
  uint32_t randomNumber(uint32_t max, uint32_t attempt, uint32_t seed) const;

 protected:
  Router(sprockit::sim_parameters* params, Topology* top, NetworkSwitch* sw);

  /**
   * @brief switch_paths Decide which path is 'shortest' based on
   *  distance and queue lengths
   * @param orig_distance
   * @param new_distance
   * @param orig_port
   * @param new_port
   * @return Whether the original path is estimated to be shorter
   */
  bool switchPaths(int orig_distance, int new_distance,
          int orig_port, int new_port) const;

 protected:
  SwitchId my_addr_;

  Topology* top_;

  NetworkSwitch* netsw_;

  RNG::rngint_t seed_;

  bool debug_seed_;

  RNG::MWC* rng_;

};

}
}
#endif
