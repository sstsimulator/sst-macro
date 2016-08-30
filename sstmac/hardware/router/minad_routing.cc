#include <sstmac/hardware/router/minad_routing.h>
#include <sstmac/hardware/switch/network_switch.h>

namespace sstmac {
namespace hw {

SpktRegister("min_ad", router, minimal_adaptive_router,
            "a routing algorithm for minimal adaptive routing on regular topologies");

void
minimal_adaptive_router::route(packet* pkt)
{
  structured_routable* rt = pkt->interface<structured_routable>();
  structured_routable::path_set paths;
  bool eject  = productive_paths_to_node(pkt->toaddr(), paths);
  if (eject) {
    rt->assign_path(paths[0]);
    return;
  }
  //loop through the ports and find the least-congested
  int min_queue_length = netsw_->queue_length(paths[0].outport);
  structured_routable::path& min_path = paths[0];
  debug_printf(sprockit::dbg::router,
    "Routing %p from %ld to %ld: path 0 port=%d queue=%d",
    pkt,
    long(netsw_->addr()),
    long(pkt->toaddr()),
    paths[0].outport,
    min_queue_length);

  for (int i=1; i < paths.size(); ++i) {
    int test_length = netsw_->queue_length(paths[i].outport);
    debug_printf(sprockit::dbg::router,
      "  path %d: port=%d queue=%d",
      i, paths[i].outport, test_length);
    if (test_length < min_queue_length) {
      min_path = paths[i];
      min_queue_length = test_length;
    }
  }
  debug_printf(sprockit::dbg::router,
    "  chose %d", min_path.outport);
  rt->assign_path(min_path);
}

void
minimal_adaptive_router::finalize_init()
{
  router::finalize_init();
  max_num_vc_ = num_vc_lookup_[routing::minimal];
}

}
}

