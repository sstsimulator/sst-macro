#ifndef sstmac_sw_launch_task_mapping_h
#define sstmac_sw_launch_task_mapping_h

#include <list>
#include <vector>
#include <map>
#include <memory>
#include <sstmac/software/process/app_id.h>
#include <sstmac/common/node_address.h>
#include <sstmac/common/serializable.h>

namespace sstmac {
namespace sw {

class TaskMapping {
 public:
  TaskMapping(AppId aid) : aid_(aid) {}

  typedef std::shared_ptr<TaskMapping> ptr;

  NodeId rankToNode(int rank) const {
    return rank_to_node_indexing_[rank];
  }

  const std::list<int>& nodeToRanks(int node) const {
    return node_to_rank_indexing_[node];
  }

  AppId aid() const {
    return aid_;
  }

  static TaskMapping::ptr serialize_order(AppId aid, serializer& ser);

  int numRanks() const {
    return rank_to_node_indexing_.size();
  }

  int nproc() const {
    return rank_to_node_indexing_.size();
  }

  const std::vector<NodeId>& rankToNode() const {
    return rank_to_node_indexing_;
  }

  std::vector<NodeId>& rankToNode() {
    return rank_to_node_indexing_;
  }

  const std::vector<std::list<int>>& nodeToRank() const {
    return node_to_rank_indexing_;
  }

  std::vector<std::list<int>>& nodeToRank() {
    return node_to_rank_indexing_;
  }

  static const TaskMapping::ptr& globalMapping(AppId aid);

  static TaskMapping::ptr globalMapping(const std::string& unique_name);

  static void addGlobalMapping(AppId aid, const std::string& unique_name,
                     const TaskMapping::ptr& mapping);

  static void removeGlobalMapping(AppId aid, const std::string& name);

 private:
  AppId aid_;
  std::vector<NodeId> rank_to_node_indexing_;
  std::vector<std::list<int> > node_to_rank_indexing_;
  std::vector<int> core_affinities_;

  static std::vector<int>  local_refcounts_;
  static std::vector<TaskMapping::ptr> app_ids_launched_;
  static std::map<std::string, TaskMapping::ptr> app_names_launched_;

  static void deleteStatics();
};

}
}

#endif
