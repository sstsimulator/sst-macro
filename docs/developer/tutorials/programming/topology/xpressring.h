#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

class xpress_ring :
  public structured_topology
{
 public:
  typedef enum {
    up_port = 0,
    down_port = 1,
    jump_up_port = 2,
    jump_down_port = 3
  } port_t;

 public:
  xpress_ring(sprockit::sim_parameters* params);

  virtual ~xpress_ring() {}

  bool uniform_switches() const override {
    return true;
  }

  bool
  uniform_network_ports() const override {
    return true;
  }

  bool
  uniform_switches_non_uniform_network_ports() const override {
    return true;
  }

  std::string
  to_string() const override {
    return "xpress ring topology";
  }

  void
  configure_individual_port_params(switch_id src,
              sprockit::sim_parameters* switch_params) const override;

  void
  configure_vc_routing(std::map<routing::algorithm_t, int>& m) const override;

  void
  connected_outports(switch_id src, 
        std::vector<topology::connection>& conns) const override;

  /**
  Structured topologies can be direct (torus) or indirect (fat tree).
  We therefore need to distinguish the total number of switches and
  the number of leaf switches - i.e. those directly connected to nodes.
  For direct topologies, num_switches and num_leaf_switches are the same.
  For indirect, num_leaf_switches < num_switches.
  @return The number of leaf switches directly connected to compute nodes
  */
  int num_leaf_switches() const override {
    return ring_size_;
  }

  /**
  Workhorse function for implementing #minimal_route_to_switch
  and #minimal_route_to_node.
  Given source/dest coordinates, find the minimal path.
  @param current_sw_addr The addr of the current switch
  @param dest_sw_addr The addr of the destination switch
  @param path [inout] A complete path descriptor to the destination switch
  */
  void
  minimal_route_to_switch(
    switch_id src,
    switch_id dest,
    routable::path& path) const override;

  /**
  The function accepts either source or node coordinates.
  This gives the minimal distance counting the number of hops between switches.
  If node coordinates are given, the last coordinate is just ignored.
  @param src_coords. The source coordinates. This can be either switch or node coordinates.
  @param dest_coords. The destination coordinates. This can be either switch or node coordinates.
  @return The number of hops to final destination
  */
  int
  minimal_distance(
    switch_id src,
    switch_id dest) const override;

  int
  num_switches() const override {
    return ring_size_;
  }

  int
  diameter() const override;

 private:
  int
  num_hops(int total_distance) const;

 private:
  int ring_size_;

  int jump_size_;

};

}
}

