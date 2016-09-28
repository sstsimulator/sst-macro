#include <sstmac/hardware/router/minimal_routing.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sstmac/hardware/pisces/pisces_switch.h>
#include <sstmac/hardware/pisces/pisces_stats.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

SpktRegister("minimal", router, minimal_router,
            "a routing algorithm for minimal routing on regular topologies");

minimal_router::minimal_router(sprockit::sim_parameters* params, topology* top,
                               network_switch* netsw, routing::algorithm_t algo) :
  router(params, top, netsw, algo)
{
  fat_tree* ft = test_cast(fat_tree, top);
  if (ft){
    spkt_throw(sprockit::value_error,
               "minimal_router should not be used with fat tree - set router=fattree in params");
  }
}

void
minimal_router::route_to_switch(switch_id sid, routable::path& path)
{
  top_->minimal_route_to_switch(my_addr_, sid, path);
}

}
}

