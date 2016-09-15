#ifndef CARTESIAN_TOPOLOGY_H
#define CARTESIAN_TOPOLOGY_H

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

/**
 * @brief The cartesian_topology class
 * Encapsulates a topology like torus that can be naturally mapped onto
 * an n-dimensional Cartesian (integer) coordinate system
 */
class cartesian_topology :
  public structured_topology
{
 public:
  /**
   * @brief configure_geometric_paths
   * For all possible geometric or structure paths, compute
   * their redundances in terms of number of ports that go
   * in the same geometric or structural direction, e.g.
   * might be 3 ports that all go +X on a router
   * @param [inout] redundancies
   */
  virtual void
  configure_geometric_paths(std::vector<int>& redundancies) = 0;

  virtual void
  minimal_routes_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    structured_routable::path &current_path,
    structured_routable::path_set &paths) const;

 protected:
  virtual void
  init_factory_params(sprockit::sim_parameters *params);

  /**
   * The number of redundant links (ports) comprising a geometric
   * or structure direction in the topology
   */
  std::vector<int> red_;
};

}
}

#endif // CARTESIAN_TOPOLOGY_H
