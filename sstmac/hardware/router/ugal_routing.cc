#include <sstmac/hardware/router/ugal_routing.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

SpktRegister("ugal", router, ugal_router,
            "router implementing ugal congestion-aware routing");

ugal_router::ugal_router(sprockit::sim_parameters *params, topology *top, network_switch *netsw)
  :  valiant_router(params, top, netsw, routing::ugal)
{
  val_threshold_ = params->get_optional_int_param("ugal_threshold", 0);
  val_preference_factor_ = params->get_optional_int_param("valiant_preference_factor",1);
}

valiant_router::next_action_t
ugal_router::initial_step(
  routable* rtbl,
  packet* pkt)
{
  routable::path& path = rtbl->current_path();
  int pathDir;
  switch_id ej_addr = top_->node_to_ejection_switch(pkt->toaddr(), path.outport);
  if (ej_addr == netsw_->addr()) {
    configure_ejection_path(path);
    return minimal;
  }

  switch_id src = addr();
  switch_id dst = ej_addr;

  int min_dst = top_->minimal_distance(src, dst);
  if (min_dst <= val_threshold_) {
    // Too close - ignore valiant.
    top_->minimal_route_to_switch(my_addr_, ej_addr, path);
    // Still need to set vc - might need to use valiant at some point.
    path.vc = zero_stage_vc(path.vc);
    return minimal;
  }

  // Compute and compare minimal and valiant routes
  switch_id inter = top_->random_intermediate_switch(src,dst);
  int valiant_dst = 
      top_->minimal_distance(src, dst) + top_->minimal_distance(inter, dst);

  // Since min_path might really be used as a path, it needs to be a copy of
  // path so that the routing function will have correct metadata to work with.
  routable::path min_path = path;

  // Conversely, val_path is never used as a real path since
  // intermediate_step() recomputes the path.
  routable::path val_path;

  top_->minimal_route_to_switch(src, dst, min_path);
  top_->minimal_route_to_switch(src, inter, val_path);
  int min_queue_length = netsw_->queue_length(min_path.outport);
  int valiant_queue_length = netsw_->queue_length(val_path.outport);
  int minimal_weight = min_queue_length * min_dst * val_preference_factor_;
  int valiant_weight = valiant_queue_length * valiant_dst;

  debug_printf(sprockit::dbg::router,
    "UGAL routing: min=%d x %d to sw %ld, valiant=%d x %d to sw %ld",
     min_dst, min_queue_length, long(ej_addr),
     valiant_dst, valiant_queue_length, long(inter));

  if (minimal_weight <= valiant_weight) {
    // Keep routing minimally.
    debug_printf(sprockit::dbg::router,
      "UGAL minimal routing to port %d",
      min_path.outport);
    min_path.vc = zero_stage_vc(min_path.vc);
    path = min_path;
    return minimal;
  }
  else {
    // Switch to valiant routing.
    debug_printf(sprockit::dbg::router,
      "UGAL valiant routing to switch %ld, port %d",
      long(inter), val_path.outport);
    rtbl->set_dest_switch(inter);
    rtbl->current_path().set_metadata_bit(routable::valiant_stage);

    // Let topology know we're switching to a new routing stage,
    // metadata may need to be modified.
    top_->new_routing_stage(rtbl);

    // intermediate_step() will handle the remaining path/vc setup.
    // Don't duplicate that here or bad things will happen.
    return intermediate_step(rtbl, pkt);
  }
}

}
}

