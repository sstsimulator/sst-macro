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

#ifndef SSTMAC_HARDWARE_NETWORK_ROUTING_ROUTER_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_ROUTING_ROUTER_H_INCLUDED


#include <sstmac/common/node_address.h>
#include <sstmac/common/event_manager_fwd.h>
#include <sstmac/hardware/router/routable.h>
#include <sstmac/hardware/router/routing_enum.h>
#include <sstmac/hardware/common/packet.h>

#include <sprockit/debug.h>
#include <sprockit/factories/factory.h>

#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/switch/network_switch_fwd.h>


DeclareDebugSlot(router)

#define rter_debug(...) \
  debug_printf(sprockit::dbg::router, "Router on switch %d: %s", int(my_addr_), sprockit::printf(__VA_ARGS__).c_str())


namespace sstmac {
namespace hw {

/**
  @class router
  Class that computes the next step a messag should taken in traversing
  the network.  This performs routing operations only and is not actually
  a 'component' in the network - those are switches.  Switch and router
  are not synonymous in SST/macro.  All switches have routers.
*/
class router : public sprockit::printable
{
  DeclareFactory(router, topology*, network_switch*)
 public:
  /**
   * @brief route Makes a routing decision for the packet.
   * All routing decisions should be stored on the packet object itself.
   * @param pkt
   */
  virtual void
  route(packet* pkt);

  virtual void
  route_to_switch(switch_id sid, routable::path& path) = 0;

  virtual ~router();

  virtual void
  compatibility_check() const;

  network_switch*
  get_switch() const {
    return netsw_;
  }

  /**
   * @brief addr
   * @return
   */
  switch_id
  addr() const {
    return my_addr_;
  }

  topology*
  topol() const {
    return top_;
  }

  /**
   * @brief max_num_vc
   * @return The maximum number of virtual channels the router must maintain
   *         to implement all possible routing algorithms
   */
  int
  max_num_vc() const {
    return max_num_vc_;
  }

 protected:
  router(sprockit::sim_parameters* params,
         topology* top, network_switch* sw, routing::algorithm_t algo);

  routing::algorithm_t
  str_to_algo(const std::string& str);

  switch_id
  find_ejection_site(node_id toaddr, routable::path& path) const;

  inline static void
  configure_ejection_path(routable::path& path) {
    path.vc = 0;
  }

  void init_vc();

 protected:
  switch_id my_addr_;

  topology* top_;

  network_switch* netsw_;

  int max_num_vc_;

  typedef std::map<routing::algorithm_t, int> algo_to_vc_map;
  algo_to_vc_map num_vc_lookup_;

  routing::algorithm_t algo_;

};

}
}
#endif