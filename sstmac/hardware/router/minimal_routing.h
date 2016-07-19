#ifndef sstmac_hardware_network_topology_routing_BASIC_ROUTING_H
#define sstmac_hardware_network_topology_routing_BASIC_ROUTING_H

#include <sstmac/hardware/router/structured_router.h>

namespace sstmac {
namespace hw {

class minimal_router :
  public structured_router
{

 public:
  virtual ~minimal_router() {}

  virtual void
  route(packet* pkt);

  std::string
  to_string() const {
    return "minimal router";
  }

  virtual void
  finalize_init();

 protected:
  virtual void
  route(packet* pkt, geometry_routable::path_set& paths);

};

}
}


#endif // BASIC_ROUTING_H

