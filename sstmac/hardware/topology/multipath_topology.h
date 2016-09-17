#ifndef MULTIPATH_TOPOLOGY_H
#define MULTIPATH_TOPOLOGY_H

#include <sstmac/hardware/router/routable.h>

namespace sstmac {
namespace hw {

class multipath_topology {
 public:
  virtual void
  get_redundant_paths(routable::path& inPath,
                      routable::path_set& outPaths) const  = 0;

  virtual void
  configure_geometric_paths(std::vector<int>& redundancies) const = 0;
};

}
}

#endif // MULTIPATH_TOPOLOGY_H
