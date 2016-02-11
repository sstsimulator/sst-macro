#include <sstmac/software/launch/first_available_allocation.h>
#include <sstmac/hardware/interconnect/interconnect.h>

namespace sstmac {
namespace sw {

SpktRegister("first_available | first", allocation_strategy,
            first_available_allocation,
            "Allocate the first set of nodes from the list of available nodes. In most cases, allocating from the available node list will give you a regular, contiguous allocation");



first_available_allocation::~first_available_allocation() throw ()
{
}

void
first_available_allocation::allocate(int nnode_requested,
                                     node_set &allocation)
{
  node_set& available = interconn_->available();
  node_set& allocated = interconn_->allocated();

  validate_num_nodes(nnode_requested, "first_available_allocation");


  for (int i = 0; i < nnode_requested; i++) {
    node_set::const_iterator nextnode = available.begin();
    debug_printf(sprockit::dbg::allocation,
        "first_available_allocation: node[%d]=%d",
        i, int((*nextnode)));

    allocation.insert(*nextnode);
    allocated.insert(*nextnode);
    available.erase(nextnode);
  }
}

}
}

