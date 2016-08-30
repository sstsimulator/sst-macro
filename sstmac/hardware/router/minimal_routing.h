#ifndef sstmac_hardware_network_topology_routing_BASIC_ROUTING_H
#define sstmac_hardware_network_topology_routing_BASIC_ROUTING_H

#include <sstmac/hardware/router/structured_router.h>

namespace sstmac {
namespace hw {

/**
 * @brief The minimal_router class
 * Router that performs
 */
class minimal_router :
  public structured_router
{

 public:
  minimal_router() :
    structured_router(routing::minimal){}

  virtual ~minimal_router() {}

  virtual void
  route(packet* pkt);

  std::string
  to_string() const {
    return "minimal router";
  }

  virtual void
  finalize_init();

  virtual void
  set_topology(topology *top);

 protected:
  minimal_router(routing::algorithm_t algo) :
    structured_router(algo){}

  virtual void
  route(packet* pkt, structured_routable::path_set& paths);

};

}
}


#endif // BASIC_ROUTING_H

