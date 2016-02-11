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
#if SSTMAC_INTEGRATED_SST_CORE
  null_node(
    SST::ComponentId_t id,
    SST::Params& params
  ) : simple_node(id, params)
  { }
#endif

  virtual ~null_node();

  void
  init_factory_params(sprockit::sim_parameters *params);

 protected:
  void
  null_warning(sprockit::sim_parameters* params);


};

}
} // end of namespace sstmac

#endif // NULLNODE_H

