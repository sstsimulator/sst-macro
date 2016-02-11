#ifndef INDEXING_APPMANAGER_H
#define INDEXING_APPMANAGER_H

#include <sstmac/software/process/app_manager.h>
#include <sstmac/software/launch/allocation_strategy.h>
#include <sstmac/software/launch/index_strategy.h>
#include <vector>

namespace sstmac {
namespace native {


class indexing_app_manager :
  public sw::app_manager
{

 public:
  void
  init_factory_params(sprockit::sim_parameters* params);

  virtual ~indexing_app_manager();

  virtual int
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

  sw::launch_info*
  launch_info() const {
    return linfo_;
  }

  void
  allocate_and_index_jobs();

  void
  set_interconnect(hw::interconnect *interconn);

 protected:
  void init_launch_info();

  virtual void
  do_allocate_and_index_jobs() = 0;

 protected:
  sw::allocation_strategy* allocator_;
  sw::index_strategy* indexer_;
  std::vector<node_id> rank_to_node_indexing_;
  std::vector<std::list<int> > node_to_rank_indexing_;
  sw::launch_info* linfo_;

  int nproc_;

};


}
}


#endif // INDEXING_APPMANAGER_H

