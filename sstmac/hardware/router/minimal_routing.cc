#include <sstmac/hardware/router/minimal_routing.h>
#include <sstmac/hardware/switch/network_switch.h>

namespace sstmac {
namespace hw {

SpktRegister("minimal", router, minimal_router,
            "a routing algorithm for minimal routing on regular topologies");


void
minimal_router::route(packet* pkt)
{
  routable* rt = pkt->interface<routable>();
  minimal_route_to_node(rt->toaddr(),
    rt->rinfo().current_path());
  int outport = rt->rinfo().port();
  debug_printf(sprockit::dbg::router,
    "Routing %p from %ld to %ld on port=%d",
     pkt,
     long(addr()),
     long(pkt->toaddr()),
     outport);
}

void
minimal_router::route(packet* pkt, routing_info::path_set &paths)
{
  routable* rt = pkt->interface<routable>();
  minimal_routes_to_node(rt->toaddr(), rt->rinfo().current_path(), paths);
}

void
minimal_router::finalize_init()
{
  structured_router::finalize_init();
  max_num_vc_ = num_vc_lookup_[routing::minimal];
}

}
}

