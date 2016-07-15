#ifndef NODE_ID_ALLOCATION_H
#define NODE_ID_ALLOCATION_H

#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/software/launch/node_allocator.h>

namespace sstmac {
namespace sw {

class node_id_allocation :
  public node_allocator
{

 public:
  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual ~node_id_allocation() throw() {}

  virtual void
  allocate(int nnode_requested,
    const ordered_node_set& available,
    ordered_node_set &allocation) const;

  static void
  read_coordinate_file(const std::string& file,
    std::vector<node_id>& node_list,
    hw::topology* top);

 protected:
  std::string coord_file_;

};

}
}


#endif // NODE_ID_ALLOCATION_H
