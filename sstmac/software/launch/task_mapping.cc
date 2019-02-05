#include <sstmac/software/launch/task_mapping.h>
#include <sstmac/common/thread_lock.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

std::vector<TaskMapping::ptr> TaskMapping::app_ids_launched_(1024);
std::map<std::string, TaskMapping::ptr> TaskMapping::app_names_launched_;
std::vector<int> TaskMapping::local_refcounts_(1024);

static thread_lock lock;

TaskMapping::ptr
TaskMapping::serialize_order(AppId aid, serializer &ser)
{
  TaskMapping::ptr mapping;
  if (ser.mode() == ser.UNPACK){
    int num_nodes;
    ser & num_nodes;
    mapping = std::make_shared<TaskMapping>(aid);
    ser & mapping->rank_to_node_indexing_;
    mapping->node_to_rank_indexing_.resize(num_nodes);
    int num_ranks = mapping->rank_to_node_indexing_.size();
    for (int i=0; i < num_ranks; ++i){
      NodeId nid = mapping->rank_to_node_indexing_[i];
      mapping->node_to_rank_indexing_[nid].push_back(i);
    }
    lock.lock();
    auto existing = app_ids_launched_[aid];
    if (!existing){
      app_ids_launched_[aid] = mapping;
    } else {
      mapping = existing;
    } 
    lock.unlock();
  } else {
    //packing or sizing
    mapping = app_ids_launched_[aid];
    if (!mapping) spkt_abort_printf("no task mapping exists for application %d", aid);
    int num_nodes = mapping->node_to_rank_indexing_.size();
    ser & num_nodes;
    ser & mapping->rank_to_node_indexing_;
  }
  return mapping;
}

TaskMapping::ptr
TaskMapping::globalMapping(const std::string& name)
{
  lock.lock();
  auto iter = app_names_launched_.find(name);
  if (iter == app_names_launched_.end()){
    spkt_abort_printf("cannot find global task mapping for %s", name.c_str());
  }
  auto ret = iter->second;
  lock.unlock();
  return ret;
}

const TaskMapping::ptr&
TaskMapping::globalMapping(AppId aid)
{
  auto& mapping = app_ids_launched_[aid];
  if (!mapping){
    spkt_abort_printf("No task mapping exists for app %d\n", aid);
  }
  return mapping;
}

void
TaskMapping::addGlobalMapping(AppId aid, const std::string &unique_name, const TaskMapping::ptr &mapping)
{
  lock.lock();
  app_ids_launched_[aid] = mapping;
  app_names_launched_[unique_name] = mapping;
  local_refcounts_[aid]++;
  lock.unlock();
}

void
TaskMapping::removeGlobalMapping(AppId aid, const std::string& name)
{
  lock.lock();
  local_refcounts_[aid]--;
  if (local_refcounts_[aid] == 0){
    app_ids_launched_[aid] = 0;
    app_names_launched_.erase(name);
  }
  lock.unlock();
}

}
}

