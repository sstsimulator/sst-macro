#include <sstmac/hardware/router/structured_router.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

structured_router::structured_router(sprockit::sim_parameters* params, topology* top,
                                     network_switch* netsw,
                                     routing::algorithm_t algo) :
  router(params, top, netsw, algo)
{
  regtop_ = safe_cast(structured_topology, top);
}

void
structured_router::minimal_routes_to_node(
  node_id dest_node_addr,
  structured_routable::path& path,
  structured_routable::path_set& paths)
{
  int ignore;
  switch_id ej_addr = regtop_->node_to_ejection_switch(dest_node_addr, ignore);

  rter_debug("structured router going through switch %d to node %d with eject on %d",
    int(my_addr_), int(dest_node_addr), int(ej_addr));

  if (ej_addr == my_addr_) {
    regtop_->node_eject_paths_on_switch(dest_node_addr, ej_addr, paths);
  }
  else {
    regtop_->minimal_routes_to_switch(my_addr_, ej_addr, path, paths);
  }
}

void
structured_router::minimal_route_to_node(
  node_id dest_node_addr,
  structured_routable::path& path)
{
  switch_id ej_addr = regtop_->node_to_ejection_switch(dest_node_addr, path.outport);
  rter_debug("structured router going from switch %d to node %d with eject on %d",
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
  structured_routable::path& path)
{
  regtop_->minimal_route_to_switch(
    my_addr_,
    dest_sw_addr,
    path);
}

void
structured_router::productive_paths_to_switch(
  switch_id dst,
  structured_routable::path_set& paths)
{
  spkt_throw(sprockit::unimplemented_error,
             "productive_paths_to_switch");
}

}
}

