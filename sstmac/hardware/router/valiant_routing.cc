#include <sstmac/hardware/router/valiant_routing.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

SpktRegister("valiant", router, valiant_router,
            "router implementing valiant routing");


valiant_router::valiant_router(sprockit::sim_parameters* params, topology *top,
                               network_switch *netsw, routing::algorithm_t algo)
  : minimal_router(params, top, netsw, algo)
{
}

valiant_router::next_action_t
valiant_router::initial_step(
  routable* rtbl,
  packet* pkt)
{
  routable::path unused_path;
  int dir = 0;
  switch_id ej_addr = top_->netlink_to_ejection_switch(pkt->toaddr(), dir);
  if (ej_addr == my_addr_){
    return final_node;
  }
  topology* top = topol();
  switch_id middle_switch = top_->random_intermediate_switch(addr(), ej_addr);
  rtbl->set_dest_switch(middle_switch);
  debug_printf(sprockit::dbg::router,
    "Router %s selected random intermediate switch %s for message %s",
      top_->switch_label(my_addr_).c_str(),
      top_->switch_label(rtbl->dest_switch()).c_str(),
      pkt->to_string().c_str());
  return intermediate_step(rtbl, pkt);
}

valiant_router::next_action_t
valiant_router::intermediate_step(
  routable* rtbl,
  packet* pkt)
{
  if (rtbl->dest_switch() == addr()) {
    // Let topology know we're switching to a new routing stage,
    // some state may need to be modified
    topology* top = topol();
    top->new_routing_stage(rtbl);
    debug_printf(sprockit::dbg::router,
      "Router %s is intermediate valiant destination for message %s",
        top_->switch_label(my_addr_).c_str(),
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
  routable* rtbl = pkt->interface<routable>();
  if (rtbl->current_path().metadata_bit(routable::final_stage)) {
    return final_node;
  }
  else if (rtbl->current_path().metadata_bit(routable::valiant_stage)) {
    return intermediate_step(rtbl, pkt);
  }
  else {
    return initial_step(rtbl, pkt);
  }
}

void
valiant_router::configure_final_path(routable::path& path)
{
  path.vc = second_stage_vc(path.vc);
  path.set_metadata_bit(routable::final_stage);
}

void
valiant_router::configure_intermediate_path(routable::path& path)
{
  path.vc = first_stage_vc(path.vc);
  path.set_metadata_bit(routable::valiant_stage);
}

void
valiant_router::route_valiant(packet* pkt)
{
  routable* rtbl = pkt->interface<routable>();
  next_action_t ac = next_routing_stage(pkt);
  auto& path = rtbl->current_path();
  switch (ac){
    case intermediate_switch:
    {
      top_->minimal_route_to_switch(my_addr_, rtbl->dest_switch(), path);
      configure_intermediate_path(path);
      break;
    }
    case final_node:
    {
      switch_id sid = top_->node_to_ejection_switch(pkt->toaddr(), path.outport);
      if (sid == my_addr_){
        configure_ejection_path(path);
      } else {
        top_->minimal_route_to_switch(my_addr_, sid, path);
        configure_final_path(path);
      }
    }
    case minimal: //no extra action required
    case eject:
      break;
  }
}

void
valiant_router::route(packet* pkt)
{
  routable* rtbl = pkt->interface<routable>();
  route_valiant(pkt);
}

}
}


