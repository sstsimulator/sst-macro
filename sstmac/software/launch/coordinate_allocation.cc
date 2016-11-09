#ifndef COORDINATE_ALLOCATION_CC
#define COORDINATE_ALLOCATION_CC

#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/software/launch/coordinate_allocation.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/hardware/topology/cartesian_topology.h>
#include <sprockit/fileio.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/stl_string.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
"launch_coordinate_file",
"coordinate_file",
);

namespace sstmac {
namespace sw {

SpktRegister("coordinate", node_allocator, coordinate_allocation);

coordinate_allocation::coordinate_allocation(sprockit::sim_parameters* params) :
  node_allocator(params)
{
  coord_file_ = params->get_param("coordinate_file");
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
coordinate_allocation::allocate(
  int nnode_requested,
  const ordered_node_set& available,
  ordered_node_set& allocation) const
{
  std::vector<hw::coordinates> node_list;
  read_coordinate_file(rt_, coord_file_, node_list);

  hw::cartesian_topology* regtop = topology_->cart_topology();

  int num_coords = node_list[0].size();
  int top_ndim = regtop->ndimensions();
  int nps = regtop->concentration();
  if (nps > 1) ++top_ndim;
  if (top_ndim != num_coords){
    spkt_throw_printf(sprockit::value_error,
        "coordinate_allocation::read_coordinate_file: mismatch between topology ndim=%d and file ncoords=%d, concentration=%d",
         top_ndim, num_coords, nps);
  }

  if (node_list.size() < nnode_requested){
    spkt_throw(sprockit::value_error,
        "coordinate_allocation::allocation: requested %d, but only have %d nodes",
        int(node_list.size()), nnode_requested);
  }

  for (int i=0; i < nnode_requested; ++i){
    const hw::coordinates& coords = node_list[i];
    node_id nid = regtop->node_addr(coords);
    debug_printf(sprockit::dbg::allocation,
        "adding node %d : %s to allocation",
        int(nid), stl_string(coords).c_str());
    allocation.insert(nid);
  }

}

}
}

#endif // COORDINATE_ALLOCATION_CC
