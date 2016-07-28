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

#ifndef SSTMAC_BACKENDS_NATIVE_LAUNCH_INDEXSTRATEGY_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_LAUNCH_INDEXSTRATEGY_H_INCLUDED

#include <sstmac/common/node_address.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/backends/common/parallel_runtime_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sprockit/debug.h>
#include <sprockit/factories/factory.h>
#include <vector>
#include <sstmac/software/launch/node_set.h>

DeclareDebugSlot(indexing);

namespace sstmac {
namespace sw {

/**
 * Base class for strategies regarding how to sequentially number nodes
 * in a parallel simulation.
 */
class task_mapper :
  public sprockit::factory_type
{

 public:
  virtual std::string
  to_string() const {
    return "task mapper";
  }

  virtual void
  set_topology(hw::topology* top){
    topology_ = top;
  }

  virtual void
  init_factory_params(sprockit::sim_parameters *params);

  virtual
  ~task_mapper() throw ();

  /** Assign processes to nodes.
   @param aid The application ID for the application whose processes are being indexed
   @param nodes is the set of unique nodes to be used for the allocation
   @param ppn is the nominal number of processes allocated on each node.
   @param result is the resulting vector of length nodes (size nproc)
   @param nproc the total number of processes to allocate
   @throw value_error if nodes.empty()
   @throw value_error if ppn <= 0
   @throw value_error if nodes.size()*ppn < nproc
  */
  virtual void
  map_ranks(
    const app_id& aid,
    const ordered_node_set& allocation,
    int ppn,
    std::vector<node_id>& result,
    int nproc) = 0;

 protected:
  task_mapper(parallel_runtime* rt) :
    rt_(rt), topology_(0) {}

  int
  validate_nproc(int ppn, int num_nodes, int nproc, const char* name) const;

 protected:
  hw::topology* topology_;
  parallel_runtime* rt_;

};

DeclareFactory(task_mapper, parallel_runtime*);

}
} // end of namespace sstmac

#endif

