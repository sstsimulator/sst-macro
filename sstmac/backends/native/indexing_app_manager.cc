#include <sstmac/backends/native/indexing_app_manager.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/software/launch/launch_info.h>

namespace sstmac {
namespace native {

void
indexing_app_manager::init_launch_info()
{
  linfo_ = new sw::launch_info(
             app_template_,
             aid_,
             nproc_,
             core_affinities_);
}

indexing_app_manager::~indexing_app_manager()
{
  delete allocator_;
  delete indexer_;
}

void
indexing_app_manager::allocate_and_index_jobs()
{
  do_allocate_and_index_jobs();

  int num_nodes = interconn_->topol()->num_nodes();
  node_to_rank_indexing_.resize(num_nodes);

  int num_ranks = rank_to_node_indexing_.size();
  for (int i=0; i < num_ranks; ++i){
    node_id nid = rank_to_node_indexing_[i];
    node_to_rank_indexing_[nid].push_back(i);
  }
}

void
indexing_app_manager::init_factory_params(sprockit::sim_parameters* params)
{
  allocator_ = sw::allocation_strategy_factory::get_optional_param("launch_allocation",
               "first_available", params, rt_);

  indexer_ = sw::index_strategy_factory::get_optional_param("launch_indexing",
               "block", params, rt_);

  app_manager::init_factory_params(params);
}

void
indexing_app_manager::set_interconnect(hw::interconnect *interconn)
{
  app_manager::set_interconnect(interconn);
  allocator_->set_interconnect(interconn_);
  indexer_->set_topology(top_);
}

}
}



