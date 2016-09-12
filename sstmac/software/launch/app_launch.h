#ifndef APPMANAGER_H
#define APPMANAGER_H

#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/common/node_address.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/software/launch/node_set.h>

#include <sprockit/factories/factory.h>
#include <sprockit/unordered.h>

#include <sstmac/software/process/app_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/backends/common/parallel_runtime_fwd.h>

#include <sstmac/software/launch/node_allocator.h>
#include <sstmac/software/launch/task_mapper.h>

#include <vector>

namespace sstmac {
namespace sw {

class app_launch
{

 public:
  app_launch(app_id aid) :
    aid_(aid),
    indexed_(false)
  {
  }

  virtual ~app_launch();

  int
  nproc() const {
    return nproc_;
  }

  node_id
  node_assignment(int rank) const {
    return rank_to_node_indexing_[rank];
  }

  const std::list<int>&
  rank_assignment(node_id nid) const {
    return node_to_rank_indexing_[nid];
  }

  app*
  app_template() const {
    return app_template_;
  }

  timestamp
  time() const {
    return time_;
  }

  app_id
  aid() const {
    return aid_;
  }

  std::vector<int>
  core_affinities() const {
    return core_affinities_;
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  hw::topology*
  topol() const {
    return top_;
  }

  /**
   * @brief request_allocation  Request a set of nodes to be used by the job
   * @param available   The set of nodes available
   * @param allocation  Reference return. Will contain all the nodes request for the allocation.
   *                    The allocation is NOT necessarily a subset of availabe. If the allocation fails
   *                    the function can still return an allocation request that might be satisfied
   *                    at a later time.
   */
  void
  request_allocation(const ordered_node_set& available,
                     ordered_node_set& allocation);

  /**
   * @brief index_allocation  Given an allocation of nodes, index or map the job. For MPI,
   *                          this means assigning MPI ranks to nodes (and possibly even cores).
   * @param allocation        The set of nodes returned by the allocation request
   */
  void
  index_allocation(const ordered_node_set& allocation);

  bool
  is_indexed() const {
    return indexed_;
  }

  static app_launch*
  static_app_launch(int aid, sprockit::sim_parameters* params);

  static void
  clear_static_app_launch(){
    for (auto& pair : static_app_launches_){
      delete pair.second;
    }
    static_app_launches_.clear();
  }

  static void
  parse_aprun(const std::string& cmd, int& nproc, int& nproc_per_node,
              std::vector<int>& core_affinities);

  static void
  parse_launch_cmd(
    sprockit::sim_parameters* params,
    int& nproc,
    int& procs_per_node,
    std::vector<int>& affinities);

 private:
  sw::node_allocator* allocator_;
  sw::task_mapper* indexer_;
  std::vector<node_id> rank_to_node_indexing_;
  std::vector<std::list<int> > node_to_rank_indexing_;

  hw::topology* top_;

  sw::app* app_template_;

  sw::app_id aid_;

  std::string appname_;

  bool indexed_;

  std::vector<int> core_affinities_;

  timestamp time_;

  int nproc_;

  int procs_per_node_;

  void parse_launch_cmd(sprockit::sim_parameters* params);

 private:
  static std::map<int, app_launch*> static_app_launches_;

  void init_launch_info();

};

DeclareFactory(app_launch, sw::app_id);

}
}



#endif // APPMANAGER_H

