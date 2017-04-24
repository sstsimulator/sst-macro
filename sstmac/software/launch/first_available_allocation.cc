#include <sstmac/software/launch/first_available_allocation.h>
#include <sstmac/hardware/topology/topology.h>

namespace sstmac {
namespace sw {

first_available_allocation::~first_available_allocation() throw ()
{
}

void
first_available_allocation::allocate(
  int nnode_requested,
  const ordered_node_set& available,
  ordered_node_set& allocation) const
{
  if (available.size() < nnode_requested){
    spkt_throw_printf(sprockit::value_error,
      "only %d nodes available, but %d requested",
      available.size(), nnode_requested);
  }

  int nid = 0;
  int num_allocated = 0;
  while (num_allocated < nnode_requested){
    if (available.find(nid) != available.end()){
      allocation.insert(nid);
      debug_printf(sprockit::dbg::allocation,
          "first_available_allocation: node[%d]=%d",
          num_allocated, nid);
      ++num_allocated;
    }
    ++nid;
  }
}

}
}

