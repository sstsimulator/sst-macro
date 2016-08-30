#ifndef SSTMAC_HARDWARE_NETWORK_ROUTING_ROUTER_MINAD_ROUTER_H
#define SSTMAC_HARDWARE_NETWORK_ROUTING_ROUTER_MINAD_ROUTER_H

#include <sstmac/hardware/router/minimal_routing.h>

namespace sstmac {
namespace hw {

/**
 * @brief The minimal_adaptive_router class
 * Performs some amount of adaptive routing, choosing between
 * the least congested of all possible minimal paths.
 * Router always takes a minimal path and is not able
 * to discover ``better'' non-minimal paths.
 */
class minimal_adaptive_router :
  public minimal_router
{

 public:
  minimal_adaptive_router() :
    minimal_router(routing::minimal_adaptive){}

  virtual std::string
  to_string() const {
    return "min_ad";
  }

  virtual void
  route(packet* pkt);

  virtual void
  finalize_init();

};


}
}


#endif // MINAD_ROUTER_H

