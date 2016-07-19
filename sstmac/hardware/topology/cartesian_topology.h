#ifndef CARTESIAN_TOPOLOGY_H
#define CARTESIAN_TOPOLOGY_H

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

class cartesian_topology :
  public structured_topology
{
 public:
  const std::vector<int>&
  red() const {
    return red_;
  }

  virtual void
  configure_geometric_paths(std::vector<int>& redundancies) = 0;

  virtual void
  minimal_routes_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    geometry_routable::path &current_path,
    geometry_routable::path_set &paths) const;

 protected:
  virtual void
  init_factory_params(sprockit::sim_parameters *params);

  std::vector<int> red_;
};

}
}

#endif // CARTESIAN_TOPOLOGY_H
