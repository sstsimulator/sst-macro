#ifndef sstmac_hardware_network_topology_routing_BASIC_ROUTING_H
#define sstmac_hardware_network_topology_routing_BASIC_ROUTING_H

#include <sstmac/hardware/router/router.h>

namespace sstmac {
namespace hw {

/**
 * @brief The minimal_router class
 * Router that performs
 */
class minimal_router :
  public router
{

 public:
  minimal_router(sprockit::sim_parameters* params, topology* top,
                 network_switch* netsw, routing::algorithm_t algo = routing::minimal);

  virtual ~minimal_router() {}

  std::string
  to_string() const override {
    return "minimal router";
  }

 protected:
  void route_to_switch(switch_id sid, routable::path &path) override;

};

}
}


#endif // BASIC_ROUTING_H

