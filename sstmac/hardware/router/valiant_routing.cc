#include <sstmac/hardware/router/valiant_routing.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

SpktRegister("valiant", router, valiant_router,
            "router implementing valiant routing");


valiant_router::next_action_t
valiant_router::initial_step(
  structured_routable* rtbl,
  packet* pkt)
{
  structured_routable::path unused_path;
  int dir = 0;
  structured_topology* regtop = regtop_;
  switch_id ej_addr = regtop->endpoint_to_ejection_switch(pkt->toaddr(), dir);
  if (ej_addr == my_addr_){
    return final_node;
  }
  topology* top = topol();
  switch_id middle_switch = top->random_intermediate_switch(addr(), ej_addr);
  rtbl->set_dest_switch(middle_switch);
  debug_printf(sprockit::dbg::router,
    "Router %s selected random intermediate switch %s for message %s",
      regtop_->label(my_addr_).c_str(),
      regtop_->label(rtbl->dest_switch()).c_str(),
      pkt->to_string().c_str());
  return intermediate_step(rtbl, pkt);
}

void
valiant_router::finalize_init()
{
  structured_router::finalize_init();
  max_num_vc_ = num_vc_lookup_[routing::valiant];
}

valiant_router::next_action_t
valiant_router::intermediate_step(
  structured_routable* rtbl,
  packet* pkt)
{
  if (rtbl->dest_switch() == addr()) {
    // Let topology know we're switching to a new routing stage,
    // some state may need to be modified
    topology* top = topol();
    top->new_routing_stage(rtbl);
    debug_printf(sprockit::dbg::router,
      "Router %s is intermediate valiant destination for message %s",
        regtop_->label(my_addr_).c_str(),
        pkt->to_string().c_str());
    rtbl->current_path().clear_metadata();
    return final_node; //not to switch
  }
  else {
    return intermediate_switch; //route to switch, not node
  }
}

valiant_router::next_action_t
valiant_router::next_routing_stage(packet* pkt)
{
  structured_routable* rtbl = pkt->interface<structured_routable>();
  if (rtbl->current_path().metadata_bit(structured_routable::final_stage)) {
    return final_node;
  }
  else if (rtbl->current_path().metadata_bit(structured_routable::valiant_stage)) {
    return intermediate_step(rtbl, pkt);
  }
  else {
    return initial_step(rtbl, pkt);
  }
}

void
valiant_router::configure_final_path(structured_routable::path& path)
{
  path.vc = second_stage_vc(path.vc);
  path.set_metadata_bit(structured_routable::final_stage);
}

void
valiant_router::configure_intermediate_path(structured_routable::path& path)
{
  path.vc = first_stage_vc(path.vc);
  path.set_metadata_bit(structured_routable::valiant_stage);
}

void
valiant_router::route_valiant(packet* pkt)
{
  structured_routable* rtbl = pkt->interface<structured_routable>();
  next_action_t ac = next_routing_stage(pkt);
  switch (ac){
    case minimal:
      break; //nothing to do
    case intermediate_switch:
    {
      minimal_route_to_switch(rtbl->dest_switch(), rtbl->current_path());
      configure_intermediate_path(rtbl->current_path());
      break;
    }
    case final_node:
    {
      minimal_route_to_node(pkt->toaddr(), rtbl->current_path());
      configure_final_path(rtbl->current_path());
    }

  }
}

void
valiant_router::route(packet* pkt)
{
  structured_routable* rtbl = pkt->interface<structured_routable>();
  routing::algorithm_t algo = routing::valiant;
  switch(algo){
    case routing::minimal:
      minimal_route_to_node(pkt->toaddr(), rtbl->current_path());
      return;
    case routing::valiant:
      route_valiant(pkt);
      break;
    default:
      spkt_throw_printf(sprockit::value_error,
        "valiant router got invalid routing type %s",
        routing::tostr(algo));
  }
}

void
valiant_router::route(packet* pkt, structured_routable::path_set& paths)
{
  structured_routable* rtbl = pkt->interface<structured_routable>();
  next_action_t ac = next_routing_stage(pkt);
  switch(ac){
    case intermediate_switch:
      regtop_->minimal_routes_to_switch(my_addr_, rtbl->dest_switch(),
                                        rtbl->current_path(), paths);
      for (int i=0; i < paths.size(); ++i){
        configure_intermediate_path(paths[i]);
      }
      debug_printf(sprockit::dbg::router,
        "Router %s routing message to intermediate switch %s on vc %d->%d: %s",
          regtop_->label(my_addr_).c_str(),
          regtop_->label(rtbl->dest_switch()).c_str(),
          rtbl->current_path().vc,
          paths[0].vc,
          pkt->to_string().c_str());
      break;
    case final_node:
      minimal_routes_to_node(pkt->toaddr(), rtbl->current_path(), paths);
      for (int i=0; i < paths.size(); ++i){
        configure_final_path(paths[i]);
      }
      debug_printf(sprockit::dbg::router,
        "Router %s routing message to final node %s on vc %d->%d: %s",
          regtop_->label(my_addr_).c_str(),
          regtop_->label(pkt->toaddr()).c_str(),
          rtbl->current_path().vc,
          paths[0].vc,
          pkt->to_string().c_str());
      break;
    default:
      spkt_throw_printf(sprockit::value_error,
        "valiant_router: cannot handle action %d for multipath routing", ac);
  }
}


}
}


