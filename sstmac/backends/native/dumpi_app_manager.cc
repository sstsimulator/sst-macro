#include <sstmac/backends/native/dumpi_app_manager.h>
#include <sstmac/dumpi_util/dumpi_util.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sprockit/stl_string.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace native {

SpktRegister("dumpi", sw::app_manager, dumpi_app_manager);

void
dumpi_app_manager::init_factory_params(sprockit::sim_parameters* params)
{
  indexing_app_manager::init_factory_params(params);

  metafile_ = params->get_param("launch_dumpi_metaname");
  meta_ = new sw::dumpi_meta(metafile_);
  nproc_ = meta_->num_procs();

  init_launch_info();
}

void
dumpi_app_manager::do_allocate_and_index_jobs()
{
  hw::interconnect::node_set allocation;
  int ignore_num_nodes = 0; //dumpi determines this automatically
  int ignore_procs_per_node = 0;
  allocator_->allocate(ignore_num_nodes, allocation);
  indexer_->allocate(aid_, allocation,
                     ignore_procs_per_node, rank_to_node_indexing_, nproc_);

  if (sprockit::debug::slot_active(sprockit::dbg::indexing)){
    cout0 << sprockit::printf("Allocated and indexed %d nodes\n",
                rank_to_node_indexing_.size());
    int num_nodes = rank_to_node_indexing_.size();

    hw::structured_topology* regtop =
        safe_cast(hw::structured_topology, top_);

    for (int i=0; i < num_nodes; ++i){
      node_id nid = rank_to_node_indexing_[i];
      if (top_){
        hw::coordinates coords = regtop->node_coords(nid);
        cout0 << sprockit::printf("Rank %d -> nid%d %s\n",
            i, int(nid), stl_string(coords).c_str());
      } else {
         cout0 << sprockit::printf("Rank %d -> nid%d\n", i, int(nid));
      }
    }
  }

}

}
}


