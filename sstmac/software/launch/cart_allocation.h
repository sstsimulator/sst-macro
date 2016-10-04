/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
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

 public:
  cart_allocation(sprockit::sim_parameters* params);

  virtual
  ~cart_allocation() throw () {}

  std::string
  to_string() const override {
    return "cart allocation";
  }

  /**
   * @brief allocate
   * @param nnode
   * @param available
   * @param allocation
   * @return  Whether the allocation succeeded based on the available nodes
   */
  void
  allocate(int nnode,
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
  void
  insert(
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
  void
  allocate_dim(
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

