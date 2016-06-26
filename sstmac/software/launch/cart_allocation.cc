#include <sstmac/software/launch/cart_allocation.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/common/cartgrid.h>

#include <sprockit/errors.h>
#include <sprockit/factories/factory.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/stl_string.h>

namespace sstmac {
namespace sw {
SpktRegister("cart | cartesian", allocation_strategy, cart_allocation,
            "Allocate a regular, cartesian volume of nodes.  This is meant mostly for torus topologies, but is also meaningful for dragonfly and hypercube.");

void
cart_allocation::init_factory_params(sprockit::sim_parameters* params)
{
  allocation_strategy::init_factory_params(params);
  if (params->has_param("cart_launch_sizes")){
    /**
      sstkeyword {
          gui=4 4 4;
          docstring=The span of the allocation in each dimension.ENDL
          An allocation [4,4,4] would allocated 64 nodes in a cube of torus.ENDL
          This number of sizes should match the dimensionality of the torus.;
      }
    */
    params->get_vector_param("cart_launch_sizes", sizes_);
    auto_allocate_ = false;
  } else {
    sizes_.resize(3, -1);
    auto_allocate_ = true;
  }

  if (params->has_param("cart_launch_offsets")){
    /**
      sstkeyword {
          gui=0 0 0;
          docstring=The offset of the allocation in each dimension.ENDL
          Nonzero values shift the allocation along a given dimension.ENDL
          This number of offsets should match the dimensionality of the torus.;
      }
    */
    params->get_vector_param("cart_launch_offsets", offsets_);
    if (sizes_.size() != offsets_.size()) {
      spkt_throw_printf(sprockit::value_error,
                     "cartesian allocator: offsets and sizes have different dimensions");
    }
  } else {
    offsets_.resize(sizes_.size(), 0);
  }
}

void
cart_allocation::set_topology(hw::topology *top)
{
  regtop_ = safe_cast(hw::structured_topology, top);
  int ndims = regtop_->ndimensions();
  //add extra dimension for concentration
  if (regtop_->concentration(switch_id(0))) ++ndims;
  if (sizes_.size() != ndims){
    spkt_throw_printf(sprockit::value_error,
       "topology ndims does not match cart_allocation: %d != %d",
       sizes_.size(), ndims);
  }
}

void
cart_allocation::insert(const std::vector<int>& coords, node_set& allocation)
{
  node_id nid = regtop_->node_addr(coords);
  allocation.insert(nid);
  debug_printf(sprockit::dbg::allocation,
      "adding node %d : %s to allocation",
      int(nid), stl_string(coords).c_str());

  interconn_->allocated().insert(nid);
  interconn_->available().erase(nid);
}


void
cart_allocation::allocate_dim(int dim, std::vector<int>& vec,
                              node_set& allocation)
{
  if (dim == sizes_.size()) {
    insert(vec, allocation);
    return;
  }

  int dim_size = sizes_[dim];
  int dim_offset = offsets_[dim];
  for (int i = 0; i < dim_size; ++i) {
    vec[dim] = dim_offset + i;
    allocate_dim(dim + 1, vec, allocation);
  }
}

void
cart_allocation::allocate(int nnode, node_set &allocation)
{
  validate_num_nodes(nnode, "cart_allocation");
  if (auto_allocate_){
    std::vector<int> coords; coords.resize(3);
    int nx, ny, nz; gen_cart_grid(nnode, nx, ny, nz);
    for (int x=0; x < nx; ++x){
      coords[0] = x;
      for (int y=0; y < ny; ++y){
        coords[y] = y;
        for (int z=0; z < nz; ++z){
          coords[z] = z;
          node_id nid = regtop_->node_addr(coords);
          allocation.insert(nid);
        }
      }
    }
  } else {
    std::vector<int> vec(sizes_.size(), 0);
    allocate_dim(0, vec, allocation);
  }

}

} // end namespace sw
} // end namespace sstmac

