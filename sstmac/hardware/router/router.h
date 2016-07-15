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
  struct geometric_path {
    int redundancy;
    int path_counter;

    geometric_path() : path_counter(0) {}

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

  virtual void
  route(packet* pkt) = 0;

  int
  convert_to_port(int dim, int dir);

  /**
    Compute the minimal path to a node.
    This method can not be const.  For some cases,
    there are multiple minimal paths that rotate
    in a round-robin fasion to spread load (fat-tree, fbfly).
    The router needs to update its round-robin index
    when computing a path - hence no constness.
    @param node_addr The node routing to
    @param path [inout] The path to route along
  */
  virtual void
  minimal_route_to_node(
    node_id node_addr,
    geometry_routable::path& path);

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
    geometry_routable::path& path);

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
    geometry_routable::path_set& paths);

  virtual void
  productive_paths_to_switch(
    switch_id dst,
    geometry_routable::path_set& paths) = 0;

  network_switch*
  get_switch() const {
    return netsw_;
  }

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

  int
  max_num_vc() const {
    return max_num_vc_;
  }

 protected:
  router();

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

};

DeclareFactory(router);





}
}
#endif

