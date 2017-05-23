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

#ifndef SSTMAC_SOFTWARE_SERVICES_LAUNCH_ALLOCATION_CARTALLOCATION_H_INCLUDED
#define SSTMAC_SOFTWARE_SERVICES_LAUNCH_ALLOCATION_CARTALLOCATION_H_INCLUDED

#include <vector>
#include <sstmac/hardware/topology/cartesian_topology.h>
#include <sstmac/software/launch/node_allocator.h>

namespace sstmac {
namespace sw {

class cart_allocation :
  public node_allocator
{
  FactoryRegister("cart | cartesian", node_allocator, cart_allocation,
              "Allocate a regular, cartesian volume of nodes."
              "This is meant mostly for torus topologies, "
              "but is also meaningful for dragonfly and hypercube")
 public:
  cart_allocation(sprockit::sim_parameters* params);

  virtual ~cart_allocation() throw () {}

  std::string to_string() const override {
    return "cart allocation";
  }

  /**
   * @brief allocate
   * @param nnode
   * @param available
   * @param allocation
   * @return  Whether the allocation succeeded based on the available nodes
   */
  void allocate(int nnode,
   const ordered_node_set& available,
   ordered_node_set& allocation) const override;

 private:
  /**
   * @brief insert
   * @param coords     The cartesian coordinates defining the node
   * @param available  The set of available nodes
   * @param allocation The set of nodes storing the current allocation
   * @return Whether the insertion succeeded based on the available nodes
   */
  void insert(
    hw::cartesian_topology* regtop,
    const std::vector<int>& coords,
    const ordered_node_set& available,
    ordered_node_set& allocation) const;

  /**
   * @brief allocate_dim  Recursive method for looping dimensions in the block
   *                      and adding them to the allocation
   * @param dim
   * @param vec
   * @param available
   * @param allocation
   * @return Whether the allocation succeeded based on the available nodes
   */
  void allocate_dim(
   hw::cartesian_topology* regtop,
   int dim,
   std::vector<int>& vec,
   const ordered_node_set& available,
   ordered_node_set& allocation) const;

  std::vector<int> sizes_;
  std::vector<int> offsets_;

  bool auto_allocate_;

};

}
}

#endif