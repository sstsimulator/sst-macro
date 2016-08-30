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
class router :
  public sprockit::factory_type
{
 public:
  /**
   * @brief The structured_path struct Identifies a (structurally) unique
   * path in the topology. For example, there might be multiple links on a
   * router than connect to the +X router in a torus. However,
   * these links are all considered to be ``structurally'' equivalent.
   */
  struct structured_path {
    /**
     * @brief redundancy How many redundant physical links compose the single structural link
     */
    int redundancy;

    /**
     * @brief path_counter The index of the last redundant path taken
     */
    int path_counter;

    structured_path() : path_counter(0) {}

    /**
     * @brief next_index
     * @return The next redundant path that should be taken based on the previously taken paths
     */
    int next_index() {
      int ret = path_counter;
      path_counter = (path_counter + 1) % redundancy;
      return ret;
    }
  };

 public:
  virtual std::string
  to_string() const {
    return "router";
  }

  virtual ~router();

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  finalize_init();

  /**
   * @brief route Makes a routing decision for the packet.
   * All routing decisions should be stored on the packet object itself.
   * @param pkt
   */
  virtual void
  route(packet* pkt) = 0;

  /**
    Compute the minimal path to a node.
    This method can not be const.  For some cases,
    there are multiple minimal paths that rotate
    in a round-robin fasion to spread load (fat-tree, fbfly).
    The router needs to update its round-robin index
    when computing a path - hence no constness.
    @param node_addr The node routing to
    @param path [inout] The path to route along. This might
      contain information about previous steps take along the path.
  */
  virtual void
  minimal_route_to_node(
    node_id node_addr,
    structured_routable::path& path);

  /**
    Compute the minimal path to a switch.
    This method can not be const.  For some cases,
    there are multiple minimal paths that rotate
    in a round-robin fasion to spread load (fat-tree, fbfly).
    The router needs to update its round-robin index
    when computing a path - hence no constness.
    @param sw_addr The switch routing to
    @param path [inout] The path to route along
  */
  virtual void
  minimal_route_to_switch(
    switch_id sw_addr,
    structured_routable::path& path);

  virtual void
  init_stats(event_manager* m){}

  void init_vc();

  virtual void
  set_switch(network_switch* sw);

  /**
   @return Whether we are ejecting
   @param dst The destination node
   @param ports The ports that make progress towards the final destination.
                If ejecting, this contains only the ejection port.
  */
  bool
  productive_paths_to_node(
    node_id dst,
    structured_routable::path_set& paths);

  /**
   * @brief productive_paths_to_switch  Get the set of all paths
   * that corresponding to ``productive'' steps towards a destination switch
   * @param dst   The ID for the destination switch
   * @param paths [inout] The set of productive paths that move closer to the destination switch
   */
  virtual void
  productive_paths_to_switch(
    switch_id dst,
    structured_routable::path_set& paths) = 0;

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

  virtual void
  set_topology(topology* top) {
    top_ = top;
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
  router(routing::algorithm_t algo);

  routing::algorithm_t
  str_to_algo(const std::string& str);

 protected:
  switch_id my_addr_;

  topology* top_;

  network_switch* netsw_;

  bool hop_count_reporting_;

  int hop_count_delta_;

  int max_num_vc_;

  typedef std::map<routing::algorithm_t, int> algo_to_vc_map;
  algo_to_vc_map num_vc_lookup_;

  routing::algorithm_t algo_;

};

DeclareFactory(router);





}
}
#endif

