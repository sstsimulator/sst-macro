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

#ifndef SSTMAC_HARDWARE_NETWORK_SWITCHES_SWITCHTOPOLOGY_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_SWITCHES_SWITCHTOPOLOGY_H_INCLUDED

#include <sstmac/common/rng.h>
#include <sstmac/hardware/topology/coordinates.h>
#include <sstmac/hardware/topology/traffic/traffic.h>
#include <sstmac/hardware/router/routable.h>
#include <sstmac/hardware/router/routing_enum.h>
#include <sstmac/hardware/router/router_fwd.h>
#include <sstmac/hardware/common/connection.h>
#include <sstmac/backends/common/sim_partition_fwd.h>
#include <sprockit/sim_parameters_fwd.h>
#include <sprockit/debug.h>
#include <sprockit/factories/factory.h>
#include <sprockit/unordered.h>

DeclareDebugSlot(topology)

#define top_debug(...) \
  debug_printf(sprockit::dbg::topology, __VA_ARGS__)

namespace sstmac {
namespace hw {

  template <class InputMap, class OutputMap>
  void
  copy_map(InputMap& input, OutputMap& output){
    typedef typename InputMap::mapped_type input_cls_type;
    typedef typename OutputMap::mapped_type output_cls_type;

    typename InputMap::iterator it, end = input.end();
    for (it = input.begin(); it != end; ++it) {
      output[it->first] = dynamic_cast<output_cls_type>(it->second);
    }
  }

class topology
{
 public:
  static const int eject;

 public:
  typedef spkt_unordered_map<switch_id, connectable*> internal_connectable_map;
  typedef spkt_unordered_map<node_id, connectable*> end_point_connectable_map;

 public:
  virtual ~topology();

  virtual std::string
  to_string() const = 0;

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  finalize_init();

  /**** BEGIN PURE VIRTUAL INTERFACE *****/
  /**
   * Given a switch address, return number of nodes connected to it
   */
  virtual int
  endpoints_per_switch(switch_id addr) const = 0;

  int
  concentration(switch_id addr) const {
    return num_nodes_per_netlink_ * endpoints_per_switch(addr);
  }

  /**
     For indirect networks, this includes all switches -
     those connected directly to nodes and internal
     switches that are only a part of the network
     @return The total number of switches
  */
  virtual int
  num_switches() const = 0;

  virtual int
  num_endpoints() const = 0;

  /**
     @return The total number of nodes
  */
  virtual int
  num_nodes() const = 0;

  /**
   * @brief Given a set of connectables, connect them appropriately
   * @param objects The set of objects to connect
   * @param cloner   If in parallel mode, not all objects may exist.
   *                 The factory creates missing objects.
   * @throws value_error If invalid switchid is passed to cloner
   */
  virtual void
  connect_objects(sprockit::sim_parameters* params,
                  internal_connectable_map& objects) = 0;

  virtual void
  connect_end_point_objects(
    sprockit::sim_parameters* switch_params,
    sprockit::sim_parameters* node_params,
    internal_connectable_map& internal,
    end_point_connectable_map& end_points);

  /**
     For a given node, determine the injection switch
     All messages from this node inject into the network
     through this switch
     @param nodeaddr The node to inject to
     @param switch_port [inout] The port on the switch the node injects on
     @return The switch that injects from the node
  */
  virtual switch_id
  endpoint_to_injection_switch(node_id nodeaddr, int& switch_port) const = 0;

  /**
     For a given node, determine the ejection switch
     All messages to this node eject into the network
     through this switch
     @param nodeaddr The node to eject from
     @param switch_port [inout] The port on the switch the node ejects on
     @return The switch that ejects into the node
  */
  virtual switch_id
  endpoint_to_ejection_switch(node_id nodeaddr, int& switch_port) const = 0;

  virtual void
  configure_vc_routing(std::map<routing::algorithm_t, int>& m) const = 0;

  /**
     For a given node, determine the ejection switch
     All messages to this node eject into the network
     through this switch
     @param nodeaddr The node to eject from
     @param switch_port [inout] The port on the switch the node ejects on
     @return The switch that ejects into the node
  */
  virtual int
  endpoint_to_injection_port(node_id nodeaddr) const = 0;

  /**
     For a given node, determine the ejection switch
     All messages to this node eject into the network
     through this switch
     @param nodeaddr The node to eject from
     @param switch_port [inout] The port on the switch the node ejects on
     @return The switch that ejects into the node
  */
  virtual int
  endpoint_to_ejection_port(node_id nodeaddr) const = 0;

  /**
     For a given input switch, return all nodes connected to it.
     This return vector might be empty if the
     switch is an internal switch not connected to any nodes
     @throw value_error If invalid switch id is given
     @return The nodes connected to switch
  */
  virtual std::vector<node_id>
  nodes_connected_to_injection_switch(switch_id swid) const = 0;

  virtual std::vector<node_id>
  nodes_connected_to_ejection_switch(switch_id swid) const = 0;

  virtual void
  create_partition(
    int* switches_per_lp,
    int *switch_to_lp,
    int *switch_to_thread,
    int& local_num_switches,
    int me,
    int nproc,
    int nthread,
    int noccupied);

  /**
     @param dim The dimension (e.g. x, y, or z) that should be routed on next.
     Integer value has different meaning depending on type of
     router (torus/dragonfly/etc)
     @param dir The direction (e.g. +/-) that should be routed on.
     Integer value has different physical meaning depending on type
     of router (torus/dragonly/etc)
     @return The port number on a switch that the message should be routed through
     for a given dim/dir
  */
  virtual int
  convert_to_port(int dim, int dir) const = 0;

  /**
     Given the current location and a destination,
     compute the minimal path to the destination.
     The path generally consists of a dimension,
     a direction or branch along that dimension,
     a port number for that dim/dir combination,
     and a virtual channel along the dim/dir
     appropriate for avoiding deadlock.
     @param current_sw_addr The addr of the current switch
     @param dest_sw_addr The addr of the destination switch
     @param path [inout] A complete path descriptor to the destination switch
  */
  virtual void
  minimal_route_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    structured_routable::path& path) const = 0;

  /**
     Given a traffic pattern (e.g. bit-complement),
     return the partner nodes a given source node needs to
     send messages to for the given traffic pattern
     @param ty The desired traffic pattern
     @param src_node The source node that sends messages
     @param partners [inout] The list of partner nodes to send to
  */
  virtual void
  send_partners(
    traffic_pattern::type_t ty,
    node_id src_node,
    std::vector<node_id>& partners) const = 0;

  static topology*
  global() {
    return main_top_;
  }

  /**
     Given a traffic pattern (e.g. bit-complement),
     return the partner nodes a given destination node needs to
     receive messages to for the given traffic pattern
     @param ty The desired traffic pattern
     @param src_node The source node that sends messages
     @param partners [inout] The list of partner nodes to send to
  */
  virtual void
  recv_partners(
    traffic_pattern::type_t ty,
    node_id src_node,
    std::vector<node_id>& partners) const = 0;

  /**
     Figure out how many hops we have to go from a source node
     to a destination node
     @param src The source node address
     @param dst The destination node address
     @return The number of hops to destination
  */
  virtual int
  num_hops_to_node(node_id src, node_id dst) const = 0;
  /**** END PURE VIRTUAL INTERFACE *****/

  /**
   Template utility function for allowing any specific map type
   to be connected.  This creates an equivalent map of connectables that
   then calls the actual #connect_objects function
   @param objects A map of connectable* types (map might hold more specific type like networkswitch)
  */
  template <class MapTypeInternal, class MapTypeEndPoint>
  void
  connect_end_points(
   sprockit::sim_parameters* switch_params,
   sprockit::sim_parameters* node_params,
   MapTypeInternal& internal_nodes,
   MapTypeEndPoint& end_points) {
    internal_connectable_map internal_clone_map;
    end_point_connectable_map end_clone_map;
    copy_map(internal_nodes, internal_clone_map);
    copy_map(end_points, end_clone_map);
    connect_end_point_objects(switch_params, node_params, internal_clone_map, end_clone_map);
  }

  /**
   Template utility function for allowing any specific map type
   to be connected.  This creates an equivalent map of connectables that
   then calls the actual #connect_objects function
   @param objects A map of connectable* types (map might hold more specific type like networkswitch)
  */
  template <class MapType>
  void
  connect_topology(sprockit::sim_parameters* switch_params, MapType& objects) {
    typedef typename MapType::mapped_type cls_type;
    typedef typename MapType::value_type val_type;
    internal_connectable_map clone_map;
    copy_map(objects, clone_map);
    connect_objects(switch_params, clone_map);
  }

  virtual coordinates
  switch_coords(switch_id swid) const;

  /**
     Get a random switch from the topoology.
     This is most often used in things like valiant routing.
     The input parameter avoids accidentally returning
     the exact same switch you are currently on.
     @param current_sw The current location switch
     @return A random switch different from current_sw
  */
  virtual switch_id
  random_intermediate_switch(switch_id current_sw, 
                             switch_id dest_sw = switch_id(-1));

  /**
     For a given node, determine the injection switch
     All messages from this node inject into the network
     through this switch
     @param nodeaddr The node to inject to
     @return The switch that injects from the node
  */
  switch_id
  endpoint_to_injection_switch(node_id nodeaddr) const {
    int ignore;
    return endpoint_to_injection_switch(nodeaddr, ignore);
  }

  virtual switch_id
  endpoint_to_injection_switch(
        node_id nodeaddr, int ports[], int& num_ports) const {
    num_ports = 1;
    return endpoint_to_injection_switch(nodeaddr, ports[0]);
  }

  switch_id
  node_to_ejection_switch(node_id addr) const {
    node_id netid(addr / num_nodes_per_netlink_);
    return endpoint_to_ejection_switch(netid);
  }

  /**
     For a given node, determine the ejection switch
     All messages to this node eject into the network
     through this switch
     @param nodeaddr The node to eject from
     @return The switch that ejects into the node
  */
  switch_id
  endpoint_to_ejection_switch(node_id nodeaddr) const {
    int ignore;
    return endpoint_to_ejection_switch(nodeaddr, ignore);
  }

  virtual switch_id
  endpoint_to_ejection_switch(
        node_id nodeaddr, int ports[], int& num_ports) const {
    num_ports = 1;
    return endpoint_to_ejection_switch(nodeaddr, ports[0]);
  }

  int
  max_num_ports() const {
    return max_ports_injection_ + max_ports_intra_network_;
  }

  int
  max_ports_injection() const {
    return max_ports_injection_;
  }

  int
  max_ports_intra_network() const {
    return max_ports_intra_network_;
  }

  bool
  is_injection_port(int port) const {
    return port >= max_ports_intra_network_;
  }

  /**
   * @brief Return the port number for the ith injection/ejection node
   * @param ith The injection/ejection index
   * @return
   */
  int
  eject_port(int ith) const {
    return max_ports_intra_network() + ith;
  }

  virtual void
  sanity_check();

  virtual std::string
  label(node_id nid) const;

  virtual std::string
  label(switch_id sid) const;

  std::string
  label(event_loc_id id) const;

  std::string
  endpoint_label(node_id nid) const;

  /**
     Given the current location and a destination,
     compute the minimal path to the destination.
     In most cases, this function first computes
     the destination switch attached to the node
     and then calls #minimal_route_to_switch.
     The path generally consists of a dimension,
     a direction or branch along that dimension,
     a port number for that dim/dir combination,
     and a virtual channel along the dim/dir
     appropriate for avoiding deadlock.
     @param current_sw_addr The addr of the current switch
     @param dest_sw_addr The addr of the destination switch
     @param path [inout] A complete path descriptor to the destination node
  */
  virtual void
  minimal_route_to_node(
    switch_id current_sw_addr,
    node_id dest_node_addr,
    structured_routable::path& path) const;

  /**
     Informs topology that a new routing stage has begun, allowing any
     topology specific state to be modified.
     @param rinfo Routing info object
  */
  virtual void
  new_routing_stage(structured_routable* rtbl) { }

  int
  num_nodes_per_netlink() const {
    return num_nodes_per_netlink_;
  }

  bool
  netlink_endpoints() const {
    return netlink_endpoints_;
  }

  virtual void
  build_internal_connectables(
    internal_connectable_map& connectables,
    sprockit::factory<connectable>* factory,
    partition *part,
    int my_rank,
    sprockit::sim_parameters *params,
    connectable* dummy) = 0;

  static topology*
  static_topology(sprockit::sim_parameters* params);

  static void
  set_static_topology(topology* top){
    static_topology_ = top;
  }

  static void
  clear_static_topology(){
    if (static_topology_) delete static_topology_;
    static_topology_ = nullptr;
  }

 protected:
  topology();

  uint32_t random_number(uint32_t max, uint32_t attempt) const;

  sprockit::sim_parameters*
  get_port_params(sprockit::sim_parameters* params, int port);

 private:
  void
  configure_injection_params(
    sprockit::sim_parameters* nic_params,
    sprockit::sim_parameters* switch_params);

 protected:
  /**
    Nodes per switch.  The number of nodes connected to a leaf switch.
    In many topologies, there is a 1-1 correspondence. For others,
    you might have many compute nodes connected to a single injection/ejection switch.
  */
  int endpoints_per_switch_;

  bool netlink_endpoints_;

  bool outputgraph_;

  RNG::rngint_t seed_;

  bool debug_seed_;

  RNG::MWC* rng_;

  std::string name_;

  int num_nodes_per_netlink_;

  int max_ports_intra_network_;

  int max_ports_injection_;

  static topology* main_top_;

 private:
  static topology* static_topology_;

};

DeclareFactory(topology);
}
}

#endif

