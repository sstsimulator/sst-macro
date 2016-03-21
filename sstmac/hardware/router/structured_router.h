#ifndef REGULAR_ROUTER_H
#define REGULAR_ROUTER_H


#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/topology/structured_topology_fwd.h>
#include <sstmac/hardware/switch/network_switch_fwd.h>
#include <sstmac/hardware/topology/coordinates.h>

namespace sstmac {
namespace hw {

class structured_router
  : public router
{

 public:
  virtual void
  set_topology(topology* top);

  virtual void
  finalize_init();

  virtual void
  set_switch(network_switch* sw);

  void
  minimal_route_to_node(
    node_id node_addr,
    routing_info::path& path);

  void
  minimal_routes_to_node(
    node_id node_addr,
    routing_info::path& current_path,
    routing_info::path_set& path);

  virtual void
  minimal_route_to_switch(
    switch_id sw_addr,
    routing_info::path& path);

  virtual void
  productive_paths_to_switch(
    switch_id dst,
    routing_info::path_set& paths);

 protected:
  structured_topology* regtop_;

};

}
}

#endif // REGULAR_ROUTER_H

