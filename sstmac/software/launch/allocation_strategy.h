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

#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>
#include <sprockit/sim_parameters_fwd.h>

#include <set>

DeclareDebugSlot(allocation);

namespace sstmac {
namespace sw {

/**
 * Strategy type for assigning processes to nodes in a parallel run.
 *
 */
class allocation_strategy : public sprockit::factory_type
{
 protected:
  typedef std::set<node_id> node_set;

 public:
  virtual void
  init_param1(parallel_runtime* rt){
    rt_ = rt;
  }

  /**
    @return The topology being allocated from
  */
  hw::topology*
  topol() const {
    return topology_;
  }

  void
  set_interconnect(hw::interconnect* ic);

  virtual void
  set_topology(hw::topology* top) {
    topology_ = top;
  }

  virtual void
  init_factory_params(sprockit::sim_parameters *params);

  virtual std::string
  to_string() const {
    return "allocation strategy";
  }

  virtual
  ~allocation_strategy() throw ();

  /** Get nodes.
    @param nnode number of nodes requested
    @param allocation returns the nodes that have been allocated
    @throw value_error If not enough nodes are available, then this will throw.
    Nodes are removed from available and put into allocated.
  */
  virtual void
  allocate(int nnode, node_set& allocation) = 0;

  /**
    Release a set of nodes back into the allocation
    @param alloc The nodes to deallocate
  */
  virtual void
  release(const node_set& alloc);

 protected:
  allocation_strategy() :
    rt_(0), interconn_(0), topology_(0) {}

  void validate_num_nodes(int num_nodes, const char* type);

 protected:
  hw::topology* topology_;
  hw::interconnect* interconn_;
  parallel_runtime* rt_;

};



DeclareFactory1InitParam(allocation_strategy, parallel_runtime*);

}
} // end of namespace sstmac

#endif

