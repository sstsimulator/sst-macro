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
#include <sstmac/hardware/topology/topology_fwd.h>
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

  /**** BEGIN PURE VIRTUAL INTERFACE *****/
  virtual std::string
  to_string() const = 0;

  /**
   * Given a switch address, return number of nodes connected to it
   */
  int
  endpoints_per_switch() {
    return endpoints_per_switch_;
  }

  int
  concentration() const {
    return concentration_;
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
  num_leaf_switches() const = 0;

  int
  num_nodes() const {
    return concentration_ * num_leaf_switches();
  }

  int
  num_endpoints() const {
    return endpoints_per_switch_ * num_leaf_switches();
  }

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
    sprockit::sim_parameters* ej_params,
    sprockit::sim_parameters* inj_params,
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
  int
  endpoint_to_injection_port(node_id nodeaddr) const {
    int port;
    switch_id sid = endpoint_to_injection_switch(nodeaddr, port);
    return port;
  }

  /**
     For a given node, determine the ejection switch
     All messages to this node eject into the network
     through this switch
     @param nodeaddr The node to eject from
     @param switch_port [inout] The port on the switch the node ejects on
     @return The switch that ejects into the node
  */
  int
  endpoint_to_ejection_port(node_id nodeaddr) const {
    int port;
    switch_id sid = endpoint_to_ejection_switch(nodeaddr, port);
    return port;
  }

  virtual void
  minimal_routes_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    structured_routable::path& current_path,
    structured_routable::path_set& paths) const {
    paths.resize(1);
    minimal_route_to_switch(current_sw_addr, dest_sw_addr, paths[0]);
  }

  /**
    The function accepts either source or node coordinates.
    This gives the minimal distance counting the number of hops between switches.
    If node coordinates are given, the last coordinate is just ignored.
    @param src_coords. The source coordinates. This can be either switch or node coordinates.
    @param dest_coords. The destination coordinates. This can be either switch or node coordinates.
    @return The number of hops to final destination
  */
  virtual int
  minimal_distance(switch_id src, switch_id dst) const = 0;

  /**
     Figure out how many hops we have to go from a source node
     to a destination node
     @param src The source node address
     @param dst The destination node address
     @return The number of hops to destination
  */
  int
  num_hops_to_endpoint(node_id src, node_id dst) const {
    switch_id start = endpoint_to_injection_switch(src);
    switch_id stop = endpoint_to_ejection_switch(dst);
    return minimal_distance(start, stop);
  }

  int
  num_hops_to_node(node_id src, node_id dst) const {
    node_id src_ep = src / num_nodes_per_endpoint_;
    node_id dst_ep = dst / num_nodes_per_endpoint_;
    return num_hops_to_endpoint(src_ep, dst_ep);
  }

  /**
     For a given input switch, return all nodes connected to it.
     This return vector might be empty if the
     switch is an internal switch not connected to any nodes
     @throw value_error If invalid switch id is given
     @return The nodes connected to switch
  */
  virtual void
  nodes_connected_to_injection_switch(switch_id swid, std::vector<node_id>& nodes) const;

  virtual void
  nodes_connected_to_ejection_switch(switch_id swid, std::vector<node_id>& nodes) const;


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

  static topology*
  global() {
    return main_top_;
  }

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
   sprockit::sim_parameters* ej_params,
   sprockit::sim_parameters* inj_params,
   MapTypeInternal& internal_nodes,
   MapTypeEndPoint& end_points) {
    internal_connectable_map internal_clone_map;
    end_point_connectable_map end_clone_map;
    copy_map(internal_nodes, internal_clone_map);
    copy_map(end_points, end_clone_map);
    connect_end_point_objects(ej_params, inj_params, internal_clone_map, end_clone_map);
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

  virtual switch_id
  node_to_ejection_switch(node_id addr, int& port) const = 0;

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
    return num_nodes_per_endpoint_;
  }

  typedef  std::function<connectable*(sprockit::sim_parameters*,uint64_t)> connectable_factory;

  virtual void
  build_internal_connectables(
    internal_connectable_map& connectables,
    connectable_factory factory,
    connectable_factory dummy_factory,
    partition *part,
    int my_rank,
    sprockit::sim_parameters *params);

  static topology*
  static_topology(sprockit::sim_parameters* params);

  static void
  set_static_topology(topology* top){
    static_topology_ = top;
  }

  virtual cartesian_topology*
  cart_topology() const;

  static void
  clear_static_topology(){
    if (static_topology_) delete static_topology_;
    static_topology_ = nullptr;
  }

 protected:
  enum class InitMaxPortsIntra {
    I_Remembered
  };
  enum class InitGeomEjectID {
    I_Remembered
  };

  topology(sprockit::sim_parameters* params,
           InitMaxPortsIntra i1,
           InitGeomEjectID i2);

  uint32_t random_number(uint32_t max, uint32_t attempt) const;

  sprockit::sim_parameters*
  get_port_params(sprockit::sim_parameters* params, int port);

  sprockit::sim_parameters*
  setup_port_params(int port, int credits, double bw,
                    sprockit::sim_parameters* link_params,
                    sprockit::sim_parameters* params);

 protected:
  /**
    Nodes per switch.  The number of nodes connected to a leaf switch.
    In many topologies, there is a 1-1 correspondence. For others,
    you might have many compute nodes connected to a single injection/ejection switch.
  */
  int endpoints_per_switch_;

  int concentration_;

  RNG::rngint_t seed_;

  bool debug_seed_;

  RNG::MWC* rng_;

  std::string name_;

  int num_nodes_per_endpoint_;

  int max_ports_intra_network_;

  int max_ports_injection_;

  int eject_geometric_id_;

  int injection_redundancy_;

  static topology* main_top_;

 private:
  static topology* static_topology_;

};

DeclareFactory(topology);
}
}

#endif

