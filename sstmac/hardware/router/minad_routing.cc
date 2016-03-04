#include <sstmac/hardware/router/minad_routing.h>
#include <sstmac/hardware/switch/network_switch.h>

namespace sstmac {
namespace hw {

SpktRegister("min_ad", router, minimal_adaptive_router,
            "a routing algorithm for minimal adaptive routing on regular topologies");

void
minimal_adaptive_router::route(const sst_message::ptr& msg)
{
  routable* rt = msg->interface<routable>();
  routing_info::path_set paths;
  bool eject  = productive_paths_to_node(msg->toaddr(), paths);
  if (eject) {
    rt->rinfo().assign_path(paths[0]);
    return;
  }
  //loop through the ports and find the least-congested
  int min_queue_length = netsw_->queue_length(paths[0].outport);
  routing_info::path& min_path = paths[0];
  debug_printf(sprockit::dbg::router,
    "Routing %p from %ld to %ld: path 0 port=%d queue=%d",
    msg.get(),
    long(netsw_->addr()),
    long(msg->toaddr()),
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
  rt->rinfo().assign_path(min_path);
}

void
minimal_adaptive_router::finalize_init()
{
  router::finalize_init();
  max_num_vc_ = num_vc_lookup_[routing::minimal];
}

}
}

