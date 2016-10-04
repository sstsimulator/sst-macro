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

#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/software/launch/task_mapper.h>
#include <sstmac/backends/common/parallel_runtime.h>

RegisterDebugSlot(indexing);

ImplementFactory(sstmac::sw::task_mapper);

namespace sstmac {
namespace sw {

task_mapper::~task_mapper() throw()
{
}

task_mapper::task_mapper(sprockit::sim_parameters* params) :
  rt_(nullptr), topology_(nullptr)
{
  rt_ = parallel_runtime::static_runtime(params);
  topology_ = sstmac::hw::topology::static_topology(params);
}

int
task_mapper::validate_nproc(int ppn, int num_nodes, int nproc,
                               const char *name) const
{
  if(nproc < 0) {
    nproc = num_nodes * ppn;
  }

  if(num_nodes == 0) {
    spkt_throw_printf(sprockit::value_error,
                     "%s::allocate:  empty node vector",
                     name);
  }

  if(ppn <= 0) {
    spkt_throw_printf(sprockit::value_error,
                     "%s::allocate:  ppn must be > 0. "
                     "If you're running DUMPI or another tracer, launch_indexing parameter "
                     "needs to be hostname or dumpi. Alternatively you can specify a app1.launch_cmd.",
                     name);
  }

  int max_nproc = ppn * num_nodes;
  if (max_nproc < nproc) {
    spkt_throw_printf(sprockit::value_error,
                     "%s::allocate: require ppn * num_nodes >= nproc: ppn=%d num_nodes=%d nproc=%d",
                     name, ppn, num_nodes, nproc);
  }

  return nproc;
}

}
}

