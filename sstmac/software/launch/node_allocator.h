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

#ifndef SSTMAC_BACKENDS_NATIVE_LAUNCH_ALLOCATIONSTRATEGY_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_LAUNCH_ALLOCATIONSTRATEGY_H_INCLUDED

#include <sstmac/common/rng.h>
#include <sstmac/common/node_address.h>
#include <sstmac/backends/common/parallel_runtime_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/software/launch/node_set.h>

#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>
#include <sprockit/sim_parameters_fwd.h>
#include <sprockit/unordered.h>
#include <sprockit/printable.h>

DeclareDebugSlot(allocation);

namespace sstmac {
namespace sw {

/**
 * Strategy type for assigning processes to nodes in a parallel run.
 *
 */
class node_allocator :
  public sprockit::printable
{
 public:
  virtual
  ~node_allocator() throw ();

  /** Get nodes.
    @param nnode number of nodes requested
    @param available the set of nodes that can be given
    @param allocation returns the nodes that have been allocated
    @return Whether the allocation succeeded based on available nodes
  */
  virtual void
  allocate(int nnode,
   const ordered_node_set& available,
   ordered_node_set& allocation) const = 0;

 protected:
  node_allocator(sprockit::sim_parameters* params);

 protected:
  hw::topology* topology_;
  parallel_runtime* rt_;

};


DeclareFactory(node_allocator);

}
} // end of namespace sstmac

#endif

