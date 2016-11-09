/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
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

DeclareFactory(router, topology*, network_switch*);





}
}
#endif

