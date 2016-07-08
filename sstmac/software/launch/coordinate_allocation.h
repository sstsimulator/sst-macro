#ifndef COORDINATE_ALLOCATION_H
#define COORDINATE_ALLOCATION_H

#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/software/launch/allocation_strategy.h>

namespace sstmac {
namespace sw {

class coordinate_allocation :
  public allocation_strategy
{

 public:
  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual ~coordinate_allocation() throw() {}

  virtual void
  allocate(int nnode_requested,
    const node_set& available,
    node_set &allocation) const;

  static void
  read_coordinate_file(
    parallel_runtime* rt,
    const std::string& file,
    std::vector<hw::coordinates>& node_list);

 protected:
  std::string coord_file_;

};

}
}

#endif // COORDINATE_ALLOCATION_H
