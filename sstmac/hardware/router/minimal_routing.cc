#include <sstmac/hardware/router/minimal_routing.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

SpktRegister("minimal", router, minimal_router,
            "a routing algorithm for minimal routing on regular topologies");


void
minimal_router::route(packet* pkt)
{
  structured_routable* rt = pkt->interface<structured_routable>();
  minimal_route_to_node(rt->toaddr(),
    rt->current_path());
  int outport = rt->port();
  debug_printf(sprockit::dbg::router,
    "Routing %p from %ld to %ld on port=%d",
     pkt,
     long(addr()),
     long(pkt->toaddr()),
     outport);
}

void
minimal_router::set_topology(topology *top)
{
  fat_tree* ft = test_cast(fat_tree, top);
  if (ft){
    spkt_throw(sprockit::value_error,
               "minimal_router should not be used with fat tree - set router=fattree in params");
  }
  structured_router::set_topology(top);
}

void
minimal_router::route(packet* pkt, structured_routable::path_set &paths)
{
  structured_routable* rt = pkt->interface<structured_routable>();
  minimal_routes_to_node(rt->toaddr(), rt->current_path(), paths);
}

void
minimal_router::finalize_init()
{
  structured_router::finalize_init();
  max_num_vc_ = num_vc_lookup_[routing::minimal];
}

}
}

