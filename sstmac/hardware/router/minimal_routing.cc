#include <sstmac/hardware/router/minimal_routing.h>
#include <sstmac/hardware/switch/network_switch.h>

namespace sstmac {
namespace hw {

SpktRegister("minimal", router, minimal_router,
            "a routing algorithm for minimal routing on regular topologies");


void
minimal_router::route(const sst_message::ptr& msg)
{
  routable* rt = msg->interface<routable>();
  minimal_route_to_node(rt->toaddr(),
    rt->rinfo().current_path());
  int outport = rt->rinfo().port();
  debug_printf(sprockit::dbg::routing,
    "Routing %p from %ld to %ld on port=%d",
     msg.get(),
     long(addr()),
     long(msg->toaddr()),
     outport);
}

void
minimal_router::route(const sst_message::ptr& msg, routing_info::path_set &paths)
{
  routable* rt = msg->interface<routable>();
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

