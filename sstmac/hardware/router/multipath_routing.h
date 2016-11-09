#ifndef MULTIPATH_ROUTER_H
#define MULTIPATH_ROUTER_H

#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/topology/cartesian_topology.h>
#include <sstmac/hardware/topology/multipath_topology.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

template <class ParentRouter>
class multipath_router :
  public ParentRouter
{
  /**
   * @brief The structured_path struct Identifies a (structurally) unique
   * path in the topology. For example, there might be multiple links on a
   * router than connect to the +X router in a torus. However,
   * these links are all considered to be ``structurally'' equivalent.
   */
  struct multipath {
    /**
     * @brief redundancy How many redundant physical links compose the single structural link
     */
    int redundancy;

    /**
     * @brief path_counter The index of the last redundant path taken
     */
    int path_counter;

    multipath() : path_counter(0) {}

    /**
     * @brief next_index
     * @return The next redundant path that should be taken based on the previously taken paths
     */
    int next_index() {
      int ret = path_counter;
      path_counter = (path_counter + 1) % redundancy;
      return ret;
    }
  };

  virtual void
  compatibility_check(){
    //do nothing
  }

 public:
  multipath_router(sprockit::sim_parameters* params, topology* top, network_switch* netsw) :
    ParentRouter(params, top, netsw),
    top_(safe_cast(multipath_topology, top))
  {
    std::vector<int> reds;
    top_ = safe_cast(multipath_topology, top);
    top_->configure_geometric_paths(reds);
    int npaths = reds.size();
    geom_paths_.resize(npaths);
    for (int i=0; i < npaths; ++i){
      geom_paths_[i].redundancy = reds[i];
    }
  }

  virtual void
  route(packet* pkt){
    routable::path_set paths;
    ParentRouter::route(pkt);
    routable::path& path = pkt->interface<routable>()->current_path();
    top_->get_redundant_paths(path, paths);

    int path_id = paths[0].geometric_id;
    int next_index = geom_paths_[path_id].next_index();
    debug_printf(sprockit::dbg::router,
      "multipath routing: using index %d", next_index);
    path = paths[next_index];
  }

 private:
  std::vector<multipath> geom_paths_;
  multipath_topology* top_;

};

}
}

#endif // MULTIPATH_ROUTER_H
