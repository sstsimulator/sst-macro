#ifndef REGULAR_ROUTER_H
#define REGULAR_ROUTER_H


#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/topology/structured_topology_fwd.h>
#include <sstmac/hardware/switch/network_switch_fwd.h>
#include <sstmac/hardware/topology/coordinates.h>

namespace sstmac {
namespace hw {

/**
 * @brief The structured_router class
 * Implements a routher that computes path on-the-fly using the geometry of the topology
 * rather than using a table-based scheme with a table stored in the router.
 * For example, a computation is performed on 3D torus coordinates to determine if
 * +/- X,Y,Z is the required direction rather than just looking up port numbers in a table.
 */
class structured_router
  : public router
{

 public:
  virtual ~structured_router(){}

  void
  minimal_route_to_node(
    node_id node_addr,
    structured_routable::path& path);

  /**
   * @brief minimal_routes_to_node
   * Special version of routing function that computes all possible
   * minimal paths.
   * @param node_addr
   * @param [inout] current_path Metadata about the path taken so far.
   *    This may contain history information about previous steps
   * @param [inout[ paths The set of all available minimal paths
   */
  void
  minimal_routes_to_node(
    node_id node_addr,
    structured_routable::path& current_path,
    structured_routable::path_set& paths);

  virtual void
  minimal_route_to_switch(
    switch_id sw_addr,
    structured_routable::path& path);

  virtual void
  productive_paths_to_switch(
    switch_id dst,
    structured_routable::path_set& paths);

 protected:
  structured_router(sprockit::sim_parameters* params, topology* top,
                    network_switch* netsw, routing::algorithm_t algo);

  structured_topology* regtop_;

};

}
}

#endif // REGULAR_ROUTER_H

