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

class topology : public sprockit::printable
{
  DeclareFactory(topology)

 public:
  static const int eject;

  struct connection {
    switch_id src;
    switch_id dst;
    int src_outport;
    int dst_inport;
  };

  static const int speedy_port = 1000000;

 public:
  typedef spkt_unordered_map<switch_id, connectable*> internal_connectable_map;
  typedef spkt_unordered_map<node_id, connectable*> end_point_connectable_map;

 public:
  virtual ~topology();

  /**** BEGIN PURE VIRTUAL INTERFACE *****/
  /**
   * @brief Whether all network ports are uniform on all switches,
   *        having exactly the same latency/bandwidth parameters
   * @return
   */
  virtual bool
  uniform_network_ports() const = 0;

  /**
   * @brief Whether all switches are the same, albeit with each port on the switch
   *        having slightly different latency/bandwidth configurations
   * @return
   */
  virtual bool
  uniform_switches_non_uniform_network_ports() const = 0;

  /**
   * @brief Whether all switches are the same and all ports on those switches
   *        have exactly the same configuration
   * @return
   */
  virtual bool
  uniform_switches() const = 0;

  /**
   * @brief connected_outports
   * @param src   Get the source switch in the connection
   * @param conns The set of output connections with dst switch_id
   *              and the port numbers for each connection
   */
  virtual void
  connected_outports(switch_id src, std::vector<topology::connection>& conns) const = 0;

  /**
   * @brief configure_individual_port_params.  The port-specific parameters
   *        will be stored in new namespaces "portX" where X is the port number
   * @param src
   * @param [inout] switch_params
   */
  virtual void
  configure_individual_port_params(switch_id src,
          sprockit::sim_parameters* switch_params) const = 0;

  /**
     For indirect networks, this includes all switches -
     those connected directly to nodes and internal
     switches that are only a part of the network
     @return The total number of switches
  */
  virtual int num_switches() const = 0;

  /**
   * @brief max_switch_id Depending on the node indexing scheme, the maximum switch id
   *  might be larger than the actual number of switches.
   * @return The max switch id
   */
  virtual switch_id max_switch_id() const = 0;

  /**
   * @brief swithc_id_slot_filled
   * @param sid
   * @return Whether a switch object should be built for a given switch_id
   */
  virtual bool switch_id_slot_filled(switch_id sid) const = 0;

  virtual int num_nodes() const = 0;

  /**
   * @brief max_node_id Depending on the node indexing scheme, the maximum node id
   *  might be larger than the actual number of nodes.
   * @return The max node id
   */
  virtual node_id max_node_id() const = 0;

  /**
   * @brief node_id_slot_filled
   * @param nid
   * @return Whether a node object should be built for a given node_id
   */
  virtual bool node_id_slot_filled(node_id nid) const = 0;

  virtual switch_id max_netlink_id() const = 0;

  virtual bool
  netlink_id_slot_filled(node_id nid) const = 0;

  /**
   * @brief num_endpoints To be distinguished slightly from nodes.
   * Multiple nodes can be grouped together with a netlink.  The netlink
   * is then the network endpoint that injects to the switch topology
   * @return
   */
  virtual int
  num_netlinks() const = 0;

  /**
   * @brief Return the maximum number of ports on any switch in the network
   * @return
   */
  virtual int
  max_num_ports() const = 0;

  /**
     For a given node, determine the injection switch
     All messages from this node inject into the network
     through this switch
     @param nodeaddr The node to inject to
     @param switch_port [inout] The port on the switch the node injects on
     @return The switch that injects from the node
  */
  virtual switch_id
  netlink_to_injection_switch(netlink_id nodeaddr, int& switch_port) const = 0;

  /**
     For a given node, determine the ejection switch
     All messages to this node eject into the network
     through this switch
     @param nodeaddr The node to eject from
     @param switch_port [inout] The port on the switch the node ejects on
     @return The switch that ejects into the node
  */
  virtual switch_id
  netlink_to_ejection_switch(netlink_id nodeaddr, int& switch_port) const = 0;

  /**
   * @brief configure_vc_routing  Configure the number of virtual channels
   *        required for all supported routing algorithms
   * @param [inout] m
   */
  virtual void
  configure_vc_routing(std::map<routing::algorithm_t, int>& m) const = 0;

  /**
   * @brief node_to_ejection_switch Given a destination node,
   *        figure out which switch has an ejection connection to it
   * @param addr
   * @param port  The port number on the switch that leads to ejection
   *              to the particular node
   * @return
   */
  virtual switch_id
  node_to_ejection_switch(node_id addr, int& port) const = 0;

  virtual switch_id
  node_to_injection_switch(node_id addr, int& port) const = 0;

  /**
    This gives the minimal distance counting the number of hops between switches.
    @param src. The source switch.
    @param dest. The destination switch.
    @return The number of hops to final destination
  */
  virtual int
  minimal_distance(switch_id src, switch_id dst) const = 0;


  /**
    This gives the minimal distance counting the number of hops between switches.
    @param src. The source node.
    @param dest. The destination node.
    @return The number of hops to final destination
  */
  virtual int
  num_hops_to_node(node_id src, node_id dst) const = 0;

  struct injection_port {
    node_id nid;
    int port;
  };

  /**
     For a given input switch, return all nodes connected to it.
     This return vector might be empty if the
     switch is an internal switch not connected to any nodes
     @return The nodes connected to switch for injection
  */
  virtual void
  nodes_connected_to_injection_switch(switch_id swid,
                          std::vector<injection_port>& nodes) const = 0;

  /**
     For a given input switch, return all nodes connected to it.
     This return vector might be empty if the
     switch is an internal switch not connected to any nodes
     @return The nodes connected to switch for ejection
  */
  virtual void
  nodes_connected_to_ejection_switch(switch_id swid,
                          std::vector<injection_port>& nodes) const = 0;

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
    routable::path& path) const = 0;

  virtual bool
  node_to_netlink(node_id nid, node_id& net_id, int& offset) const = 0;


  /**** END PURE VIRTUAL INTERFACE *****/

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
    switch_id sid = netlink_to_injection_switch(nodeaddr, port);
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
  netlink_to_ejection_port(netlink_id nodeaddr) const {
    int port;
    switch_id sid = netlink_to_ejection_switch(nodeaddr, port);
    return port;
  }

  switch_id
  netlink_to_ejection_switch(netlink_id nodeaddr) const {
    int ignore;
    return netlink_to_ejection_switch(nodeaddr, ignore);
  }

  switch_id
  netlink_to_injection_switch(netlink_id nodeaddr) const {
    int ignore;
    return netlink_to_injection_switch(nodeaddr, ignore);
  }

  virtual void
  minimal_routes_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    routable::path& current_path,
    routable::path_set& paths) const {
    paths.resize(1);
    minimal_route_to_switch(current_sw_addr, dest_sw_addr, paths[0]);
  }

  virtual void create_partition(
    int* switch_to_lp,
    int* switch_to_thread,
    int me,
    int nproc,
    int nthread,
    int noccupied) const;

#if SSTMAC_INTEGRATED_SST_CORE
  switch_id node_to_logp_switch(node_id nid) const;

  static int nproc;
#endif


  static topology*
  global() {
    return main_top_;
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

  virtual switch_id
  node_to_injection_switch(
        node_id nodeaddr, int ports[], int& num_ports) const {
    num_ports = 1;
    return node_to_injection_switch(nodeaddr, ports[0]);
  }

  virtual switch_id
  node_to_ejection_switch(
        node_id nodeaddr, int ports[], int& num_ports) const {
    num_ports = 1;
    return node_to_ejection_switch(nodeaddr, ports[0]);
  }


  virtual switch_id
  netlink_to_injection_switch(
        node_id nodeaddr, int ports[], int& num_ports) const {
    num_ports = 1;
    return netlink_to_injection_switch(nodeaddr, ports[0]);
  }

  virtual switch_id
  netlink_to_ejection_switch(
        node_id nodeaddr, int ports[], int& num_ports) const {
    num_ports = 1;
    return netlink_to_ejection_switch(nodeaddr, ports[0]);
  }

  /**
   * @brief configure_switch_params By default, almost all topologies
   *        have uniform switch parameters.
   * @param src
   * @param switch_params In/out parameter. Input is default set of params.
   *        Output is non-default unique params.
   */
  virtual void
  configure_nonuniform_switch_params(switch_id src,
        sprockit::sim_parameters* switch_params) const
  {
  }

  std::string
  label(device_id id) const;

  virtual std::string
  switch_label(switch_id sid) const;

  virtual std::string
  node_label(node_id nid) const;

  /**
     Informs topology that a new routing stage has begun, allowing any
     topology specific state to be modified.
     @param rinfo Routing info object
  */
  virtual void
  new_routing_stage(routable* rtbl) { }

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

  static sprockit::sim_parameters*
  get_port_params(sprockit::sim_parameters* params, int port);

 protected:
  topology(sprockit::sim_parameters* params);

  uint32_t random_number(uint32_t max, uint32_t attempt) const;

  static sprockit::sim_parameters*
  setup_port_params(int port, int credits, double bw,
                    sprockit::sim_parameters* link_params,
                    sprockit::sim_parameters* params);

  void configure_individual_port_params(int port_offset, int nports,
           sprockit::sim_parameters* params) const;

 protected:
  RNG::rngint_t seed_;

  bool debug_seed_;

  RNG::MWC* rng_;

  std::string name_;

  static topology* main_top_;

 private:
  static topology* static_topology_;

};

}
}

#endif