#ifndef COORDINATE_ALLOCATION_CC
#define COORDINATE_ALLOCATION_CC

#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/software/launch/coordinate_allocation.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sprockit/fileio.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/stl_string.h>

namespace sstmac {
namespace sw {

SpktRegister("coordinate", allocation_strategy, coordinate_allocation);

void
coordinate_allocation::init_factory_params(sprockit::sim_parameters* params)
{
  allocation_strategy::init_factory_params(params);
  coord_file_ = params->get_param("launch_coordinate_file");
}

void
coordinate_allocation::set_topology(hw::topology *top)
{
  allocation_strategy::set_topology(top);
  regtop_ = safe_cast(hw::structured_topology, top);
}

void
coordinate_allocation::read_coordinate_file(
  parallel_runtime* rt,
  const std::string &file,
  std::vector<hw::coordinates> &node_list)
{
  std::istream* instr = rt->bcast_file_stream(file);
  std::istream& in = *instr;

  int num_nodes;
  int num_coords;

  in >> num_nodes;
  in >> num_coords;

  node_list.resize(num_nodes);
  for (int nid=0; nid < num_nodes; ++nid){
    hw::coordinates coords(num_coords);
    for (int idx=0; idx < num_coords; ++idx){
        in >> coords[idx];
    }
    node_list[nid] = coords;
  }

  delete instr;
}

void
coordinate_allocation::allocate(int nnode_requested,
                              node_set &allocation)
{
  std::vector<hw::coordinates> node_list;
  read_coordinate_file(rt_, coord_file_, node_list);

  int num_coords = node_list[0].size();
  int top_ndim = regtop_->ndimensions();
  int nps = regtop_->concentration(switch_id(0));
  if (nps > 1) ++top_ndim;
  if (top_ndim != num_coords){
    spkt_throw_printf(sprockit::value_error,
        "coordinate_allocation::read_coordinate_file: mismatch between topology ndim=%d and file ncoords=%d, concentration=%d",
         top_ndim, num_coords, nps);
  }

  if (!topology_) {
    spkt_throw_printf(sprockit::null_error,
        "coordinate_allocation::allocate: null topology");
  }

  if (node_list.size() < nnode_requested){
    spkt_throw(sprockit::value_error,
        "coordinate_allocation::allocation: requested %d, but only have %d nodes",
        int(node_list.size()), nnode_requested);
  }

  for (int i=0; i < nnode_requested; ++i){
    const hw::coordinates& coords = node_list[i];
    node_id nid = regtop_->node_addr(coords);
    debug_printf(sprockit::dbg::allocation,
        "adding node %d : %s to allocation",
        int(nid), stl_string(coords).c_str());
    allocation.insert(nid);
  }
}

}
}

#endif // COORDINATE_ALLOCATION_CC
