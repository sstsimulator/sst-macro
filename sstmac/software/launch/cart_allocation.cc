/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sstmac/software/launch/cart_allocation.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/cartesian_topology.h>
#include <sstmac/common/cartgrid.h>

#include <sprockit/errors.h>
#include <sprockit/factories/factory.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/stl_string.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
"cart_launch_sizes",
"cart_launch_offsets",
"cart_sizes",
"cart_offsets");

namespace sstmac {
namespace sw {

cart_allocation::cart_allocation(sprockit::sim_parameters* params) :
  node_allocator(params)
{
  if (params->has_param("cart_sizes")){
    params->get_vector_param("cart_sizes", sizes_);
    auto_allocate_ = false;
  } else {
    sizes_.resize(3, -1);
    auto_allocate_ = true;
  }

  if (params->has_param("cart_offsets")){
    params->get_vector_param("cart_offsets", offsets_);
    if (sizes_.size() != offsets_.size()) {
      spkt_throw_printf(sprockit::value_error,
                     "cartesian allocator: offsets and sizes have different dimensions");
    }
  } else {
    offsets_.resize(sizes_.size(), 0);
  }
}

void
cart_allocation::insert(
  hw::cartesian_topology* regtop,
  const std::vector<int>& coords,
  const ordered_node_set& available,
  ordered_node_set& allocation) const
{
  node_id nid = regtop->node_addr(coords);
  debug_printf(sprockit::dbg::allocation,
      "adding node %d : %s to allocation",
      int(nid), stl_string(coords).c_str());
  allocation.insert(nid);
}


void
cart_allocation::allocate_dim(
  hw::cartesian_topology* regtop,
  int dim,
  std::vector<int>& vec,
  const ordered_node_set& available,
  ordered_node_set& allocation) const
{
  if (dim == sizes_.size()) {
    insert(regtop, vec, available, allocation);
    return;
  }

  int dim_size = sizes_[dim];
  int dim_offset = offsets_[dim];
  for (int i = 0; i < dim_size; ++i) {
    vec[dim] = dim_offset + i;
    allocate_dim(regtop, dim + 1, vec, available, allocation);
  }
}

void
cart_allocation::allocate(
  int nnode,
  const ordered_node_set& available,
  ordered_node_set& allocation) const
{
  hw::cartesian_topology* regtop = topology_->cart_topology();

  int ndims = regtop->ndimensions();
  //add extra dimension for concentration
  if (regtop->concentration() > 1) ++ndims;
  if (sizes_.size() != ndims){
    spkt_throw_printf(sprockit::value_error,
       "topology ndims does not match cart_allocation: %d != %d",
       sizes_.size(), ndims);
  }

  if (auto_allocate_){
    std::vector<int> coords; coords.resize(3);
    int nx, ny, nz; gen_cart_grid(nnode, nx, ny, nz);
    for (int x=0; x < nx; ++x){
      coords[0] = x;
      for (int y=0; y < ny; ++y){
        coords[y] = y;
        for (int z=0; z < nz; ++z){
          coords[z] = z;
          insert(regtop, coords, available, allocation);
        }
      }
    }
  } else {
    std::vector<int> vec(sizes_.size(), 0);
    allocate_dim(regtop, 0, vec, available, allocation);
  }
}

} // end namespace sw
} // end namespace sstmac