#include <sstmac/hardware/router/ugal_routing.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

SpktRegister("ugal", router, ugal_router,
            "router implementing ugal congestion-aware routing");


void
ugal_router::init_factory_params(sprockit::sim_parameters *params)
{
  val_threshold_ = params->get_optional_int_param("ugal_threshold", 0);
  val_preference_factor_ = params->get_optional_int_param("valiant_preference_factor",1);
  valiant_router::init_factory_params(params);
}

void
ugal_router::finalize_init()
{
  valiant_router::finalize_init();
  max_num_vc_ = num_vc_lookup_[routing::ugal];
}

void
ugal_router::route(packet* pkt)
{
  //routing_info& rinfo = pkt->interface<routable>()->rinfo();
  routing::algorithm_t algo = routing::ugal;
  geometry_routable* rtbl = pkt->interface<geometry_routable>();
  switch(algo){
    case routing::minimal:
      minimal_route_to_node(pkt->toaddr(), rtbl->current_path());
      return;
    case routing::valiant:
    case routing::ugal: //virtual methods overridden
      //just run the valiant algorithm
      route_valiant(pkt);
      break;
    default:
      spkt_throw_printf(sprockit::value_error,
        "ugal router got invalid routing type %s",
        routing::tostr(algo));
  }
}

valiant_router::next_action_t
ugal_router::initial_step(
  geometry_routable* rtbl,
  packet* pkt)
{
  geometry_routable::path& path = rtbl->current_path();
  structured_topology* regtop = safe_cast(structured_topology, topol());
  int pathDir;
  switch_id ej_addr = regtop->endpoint_to_ejection_switch(pkt->toaddr(), pathDir);

  if (ej_addr == netsw_->addr()) {
    path.outport = pathDir;
    path.vc = 0;
    return minimal;
  }

  coordinates src = regtop->switch_coords(addr());
  coordinates dst = regtop->switch_coords(ej_addr);

  int min_dst = regtop->minimal_distance(src, dst);
  if (min_dst <= val_threshold_) {
    // Too close - ignore valiant.
    minimal_route_to_switch(ej_addr, path);
    // Still need to set vc - might need to use valiant at some point.
    path.vc = zero_stage_vc(path.vc);
    return minimal;
  }

  // Compute and compare minimal and valiant routes
  switch_id inter_addr = regtop->random_intermediate_switch(addr(),ej_addr);
  coordinates inter = regtop->switch_coords(inter_addr);
  int valiant_dst = 
      regtop->minimal_distance(src, inter) + regtop->minimal_distance(inter, dst);

  // Since min_path might really be used as a path, it needs to be a copy of
  // path so that the routing function will have correct metadata to work with.
  geometry_routable::path min_path = path;

  // Conversely, val_path is never used as a real path since
  // intermediate_step() recomputes the path.
  geometry_routable::path val_path;

  regtop->minimal_route_to_coords(src, dst, min_path);
  regtop->minimal_route_to_coords(src, inter, val_path);
  int min_queue_length = netsw_->queue_length(min_path.outport);
  int valiant_queue_length = netsw_->queue_length(val_path.outport);
  int minimal_weight = min_queue_length * min_dst * val_preference_factor_;
  int valiant_weight = valiant_queue_length * valiant_dst;

  debug_printf(sprockit::dbg::router,
    "UGAL routing: min=%d x %d to sw %ld, valiant=%d x %d to sw %ld",
     min_dst, min_queue_length, long(ej_addr),
     valiant_dst, valiant_queue_length, long(inter_addr));

  if (minimal_weight <= valiant_weight) {
    // Keep routing minimally.
    debug_printf(sprockit::dbg::router,
      "UGAL minimal routing to port %d",
      min_path.outport);
    min_path.vc = zero_stage_vc(min_path.vc);
    rtbl->assign_path(min_path);
    return minimal;
  }
  else {
    // Switch to valiant routing.
    debug_printf(sprockit::dbg::router,
      "UGAL valiant routing to switch %ld, port %d",
      long(inter_addr), val_path.outport);
    rtbl->set_dest_switch(inter_addr);
    rtbl->current_path().set_metadata_bit(geometry_routable::valiant_stage);

    // Let topology know we're switching to a new routing stage,
    // metadata may need to be modified.
    regtop->new_routing_stage(rtbl);

    // intermediate_step() will handle the remaining path/vc setup.
    // Don't duplicate that here or bad things will happen.
    return intermediate_step(rtbl, pkt);
  }
}

}
}

