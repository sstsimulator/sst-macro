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
  routing_info& rinfo,
  sst_message* msg)
{
  routing_info::path unused_path;
  int dir = 0;
  structured_topology* regtop = regtop_;
  switch_id ej_addr = regtop->endpoint_to_ejection_switch(msg->toaddr(), dir);
  if (ej_addr == my_addr_){
    return final_node;
  }
  topology* top = topol();
  switch_id middle_switch = top->random_intermediate_switch(addr(), ej_addr);
  rinfo.set_dest_switch(middle_switch);
  debug_printf(sprockit::dbg::router,
    "Router %s selected random intermediate switch %s for message %s",
      regtop_->label(my_addr_).c_str(),
      regtop_->label(msg->interface<routable>()->rinfo().dest_switch()).c_str(),
      msg->to_string().c_str());
  return intermediate_step(rinfo, msg);
}

void
valiant_router::finalize_init()
{
  structured_router::finalize_init();
  max_num_vc_ = num_vc_lookup_[routing::valiant];
}

valiant_router::next_action_t
valiant_router::intermediate_step(
  routing_info& rinfo,
  sst_message* msg)
{
  if (rinfo.dest_switch() == addr()) {
    // Let topology know we're switching to a new routing stage,
    // some state may need to be modified
    topology* top = topol();
    top->new_routing_stage(rinfo);
    debug_printf(sprockit::dbg::router,
      "Router %s is intermediate valiant destination for message %s",
        regtop_->label(my_addr_).c_str(),
        msg->to_string().c_str());
    rinfo.current_path().clear_metadata();
    return final_node; //not to switch
  }
  else {
    return intermediate_switch; //route to switch, not node
  }
}

valiant_router::next_action_t
valiant_router::next_routing_stage(sst_message* msg)
{
  routing_info& rinfo = msg->interface<routable>()->rinfo();
  if (rinfo.current_path().metadata_bit(routing_info::final_stage)) {
    return final_node;
  }
  else if (rinfo.current_path().metadata_bit(routing_info::valiant_stage)) {
    return intermediate_step(rinfo, msg);
  }
  else {
    return initial_step(rinfo, msg);
  }
}

void
valiant_router::configure_final_path(routing_info::path& path)
{
  path.vc = second_stage_vc(path.vc);
  path.set_metadata_bit(routing_info::final_stage);
}

void
valiant_router::configure_intermediate_path(routing_info::path& path)
{
  path.vc = first_stage_vc(path.vc);
  path.set_metadata_bit(routing_info::valiant_stage);
}

void
valiant_router::route_valiant(sst_message* msg)
{
  routing_info& rinfo = msg->interface<routable>()->rinfo();
  next_action_t ac = next_routing_stage(msg);
  switch (ac){
    case minimal:
      break; //nothing to do
    case intermediate_switch:
    {
      minimal_route_to_switch(rinfo.dest_switch(), rinfo.current_path());
      configure_intermediate_path(rinfo.current_path());
      break;
    }
    case final_node:
    {
      minimal_route_to_node(msg->toaddr(), rinfo.current_path());
      configure_final_path(rinfo.current_path());
    }

  }
}

void
valiant_router::route(sst_message* msg)
{
  routing_info& rinfo = msg->interface<routable>()->rinfo();
  rinfo.init_default_algo(routing::valiant);
  switch(rinfo.route_algo()){
    case routing::minimal:
      minimal_route_to_node(msg->toaddr(), rinfo.current_path());
      return;
    case routing::valiant:
      route_valiant(msg);
      break;
    default:
      spkt_throw_printf(sprockit::value_error,
        "valiant router got invalid routing type %s",
        routing::tostr(rinfo.route_algo()));
  }
}

void
valiant_router::route(sst_message* msg, routing_info::path_set& paths)
{
  routing_info& rinfo = msg->interface<routable>()->rinfo();
  next_action_t ac = next_routing_stage(msg);
  switch(ac){
    case intermediate_switch:
      regtop_->minimal_routes_to_switch(my_addr_, rinfo.dest_switch(), rinfo.current_path(), paths);
      for (int i=0; i < paths.size(); ++i){
        configure_intermediate_path(paths[i]);
      }
      debug_printf(sprockit::dbg::router,
        "Router %s routing message to intermediate switch %s on vc %d->%d: %s",
          regtop_->label(my_addr_).c_str(),
          regtop_->label(rinfo.dest_switch()).c_str(),
          rinfo.current_path().vc,
          paths[0].vc,
          msg->to_string().c_str());
      break;
    case final_node:
      minimal_routes_to_node(msg->toaddr(), rinfo.current_path(), paths);
      for (int i=0; i < paths.size(); ++i){
        configure_final_path(paths[i]);
      }
      debug_printf(sprockit::dbg::router,
        "Router %s routing message to final node %s on vc %d->%d: %s",
          regtop_->label(my_addr_).c_str(),
          regtop_->label(msg->toaddr()).c_str(), 
          rinfo.current_path().vc,
          paths[0].vc,
          msg->to_string().c_str());
      break;
    default:
      spkt_throw_printf(sprockit::value_error,
        "valiant_router: cannot handle action %d for multipath routing", ac);
  }
}


}
}


