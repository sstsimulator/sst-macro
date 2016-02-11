#ifndef NODE_ID_ALLOCATION_H
#define NODE_ID_ALLOCATION_H

#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/software/launch/allocation_strategy.h>

namespace sstmac {
namespace sw {

class node_id_allocation :
  public allocation_strategy
{

 public:
  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual ~node_id_allocation() throw() {}

  virtual void
  allocate(int nnode_requested,
           node_set &allocation);

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
