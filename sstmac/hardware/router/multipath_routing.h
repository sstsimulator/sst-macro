#ifndef MULTIPATH_ROUTER_H
#define MULTIPATH_ROUTER_H

#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/topology/cartesian_topology.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

template <class ParentRouter>
class multipath_router :
  public ParentRouter
{
 public:
  virtual void
  route(packet* pkt){
    structured_routable::path_set paths;
    ParentRouter::route(pkt, paths);
    int path_id = paths[0].geometric_id;
    int next_index = geom_paths_[path_id].next_index();
    debug_printf(sprockit::dbg::router,
      "multipath routing: using index %d", next_index);
    pkt->interface<structured_routable>()->assign_path(paths[next_index]);
  }

  virtual void
  set_topology(topology* top){
    std::vector<int> reds;
    ParentRouter::set_topology(top);
    top_ = safe_cast(cartesian_topology, top);
    top_->configure_geometric_paths(reds);
    int npaths = reds.size();
    geom_paths_.resize(npaths);
    for (int i=0; i < npaths; ++i){
      geom_paths_[i].redundancy = reds[i];
    }
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params){
    ParentRouter::init_factory_params(params);
  }



 private:
  std::vector<router::structured_path> geom_paths_;
  cartesian_topology* top_;

};

}
}

#endif // MULTIPATH_ROUTER_H
