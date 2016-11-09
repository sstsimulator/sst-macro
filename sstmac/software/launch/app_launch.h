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

class software_launch
{

 public:
  software_launch(sprockit::sim_parameters* params);

  virtual ~software_launch();

  int
  nproc() const {
    return nproc_;
  }

  node_id
  node_assignment(int rank) const {
    return rank_to_node_indexing_[rank];
  }

  const std::vector<node_id>&
  node_assignments() const {
    return rank_to_node_indexing_;
  }

  const std::list<int>&
  rank_assignment(node_id nid) const {
    return node_to_rank_indexing_[nid];
  }

  timestamp
  time() const {
    return time_;
  }

  std::vector<int>
  core_affinities() const {
    return core_affinities_;
  }

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

  static void
  parse_aprun(const std::string& cmd, int& nproc, int& nproc_per_node,
              std::vector<int>& core_affinities);

  static void
  parse_launch_cmd(
    sprockit::sim_parameters* params,
    int& nproc,
    int& procs_per_node,
    std::vector<int>& affinities);

  bool proc_done() {
    ++num_finished_;
    return num_finished_ == nproc_;
  }

 private:
  sw::node_allocator* allocator_;
  sw::task_mapper* indexer_;
  std::vector<node_id> rank_to_node_indexing_;
  std::vector<std::list<int> > node_to_rank_indexing_;

  hw::topology* top_;

  bool indexed_;

  std::vector<int> core_affinities_;

  timestamp time_;

  int nproc_;

  int procs_per_node_;

  int num_finished_;

  void parse_launch_cmd(sprockit::sim_parameters* params);

 private:
  void init_launch_info();

};

class app_launch : public software_launch
{
 public:
  app_launch(sprockit::sim_parameters* params,
             app_id aid,
             const std::string& app_namespace);

  virtual ~app_launch();

  const std::string&
  app_namespace() const {
    return app_namespace_;
  }

  sprockit::sim_parameters*
  app_params() const {
    return app_params_;
  }

  app_id
  aid() const {
    return aid_;
  }

  /**
   * @brief nodes
   * @param name The name and corresponding namespace (usually app1) for the application
   * @return The list of nodes assigned to each rank in the application launch
   */
  static const std::vector<node_id>&
  nodes(const std::string& name);

  static app_launch*
  static_app_launch(int aid, const std::string& name, sprockit::sim_parameters* params);

  static app_launch*
  static_app_launch(int aid, sprockit::sim_parameters* params);

  static app_launch*
  static_app_launch(int aid);

  static app_launch*
  static_app_launch(const std::string& name);

  static app_launch*
  service_info(const std::string& name);

  static void
  clear_static_app_launch(){
    for (auto& pair : static_app_launches_){
      delete pair.second;
    }
    static_app_launches_.clear();
  }

 private:
  sw::app_id aid_;

  std::string app_namespace_;

  sprockit::sim_parameters* app_params_;

  static std::map<std::string, app_launch*> static_app_launches_;

};

}
}



#endif // APPMANAGER_H

