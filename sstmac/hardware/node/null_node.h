#ifndef NULLNODE_H
#define NULLNODE_H

#include <sstmac/hardware/node/simple_node.h>

namespace sstmac {
namespace hw {

/**
 * A null node.  All requests just complete immediately.
 */
class null_node :
  public simple_node
{
 public:
  null_node(sprockit::sim_parameters* params, uint64_t id, event_manager* mgr);

  virtual ~null_node();

 private:
  sprockit::sim_parameters* override_params(sprockit::sim_parameters* params);


};

}
} // end of namespace sstmac

#endif // NULLNODE_H

