#ifndef COORDINATE_ALLOCATION_H
#define COORDINATE_ALLOCATION_H

#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/software/launch/node_allocator.h>

namespace sstmac {
namespace sw {

class coordinate_allocation :
  public node_allocator
{
 public:
  coordinate_allocation(sprockit::sim_parameters* params);

  virtual ~coordinate_allocation() throw() {}

  std::string
  to_string() const override {
    return "coordinate allocation";
  }

  virtual void
  allocate(int nnode_requested,
    const ordered_node_set& available,
    ordered_node_set& allocation) const override;

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
