#include <sstmac/hardware/router/structured_router.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

void
structured_router::set_topology(topology* top)
{
  regtop_ = safe_cast(structured_topology, top);
  router::set_topology(top);
}

void
structured_router::finalize_init()
{
  //nps_ = regtop_->endpoints_per_switch(me_);
  router::finalize_init();
}

void
structured_router::minimal_routes_to_node(
  node_id dest_node_addr,
  geometry_routable::path& path,
  geometry_routable::path_set& paths)
{
  netlink_id endpoint_id(dest_node_addr / top_->num_nodes_per_netlink());

  int ignore;
  switch_id ej_addr = regtop_->endpoint_to_ejection_switch(endpoint_id, ignore);

  rter_debug("structured router going through switch %d to node %d with eject on %d",
    int(my_addr_), int(dest_node_addr), int(ej_addr));

  if (ej_addr == my_addr_) {
    regtop_->eject_paths_on_switch(endpoint_id, ej_addr, paths);
  }
  else {
    regtop_->minimal_routes_to_switch(my_addr_, ej_addr, path, paths);
  }
}

void
structured_router::minimal_route_to_node(
  node_id dest_node_addr,
  geometry_routable::path& path)
{
  netlink_id endpoint_id(dest_node_addr / top_->num_nodes_per_netlink());
  switch_id ej_addr = regtop_->endpoint_to_ejection_switch(endpoint_id, path.outport);
  rter_debug("structured router going to switch %d to node %d with eject on %d",
    int(my_addr_), int(dest_node_addr), int(ej_addr));
  if (ej_addr == my_addr_) {
    path.vc = 0;
  }
  else {
    minimal_route_to_switch(ej_addr, path);
  }
}

void
structured_router::minimal_route_to_switch(
  switch_id dest_sw_addr,
  geometry_routable::path& path)
{
  regtop_->minimal_route_to_switch(
    my_addr_,
    dest_sw_addr,
    path);
  //path.outport = regtop_->convert_to_port(path.dim, path.dir);
}

void
structured_router::productive_paths_to_switch(
  switch_id dst,
  geometry_routable::path_set& paths)
{
  coordinates my_coords = regtop_->switch_coords(my_addr_);
  coordinates dst_coords = regtop_->switch_coords(dst);
  regtop_->productive_paths(paths, my_coords, dst_coords);
}

void
structured_router::set_switch(network_switch* sw)
{
  router::set_switch(sw);
}

}
}

