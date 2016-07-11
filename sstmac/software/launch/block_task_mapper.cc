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

#include <sstmac/software/launch/block_task_mapper.h>
#include <sstmac/software/launch/launcher.h>
#include <sstmac/software/launch/app_launch.h>
#include <sstmac/common/runtime.h>
#include <sprockit/errors.h>
#include <math.h>
#include <sstream>


namespace sstmac  {
namespace sw {

SpktRegister("block", task_mapper, block_task_mapper,
   "Tries to group consecutive MPI ranks on the same node (i.e. in a block)."
   "Otherwise, indexes in the same order as the allocation list.");


block_task_mapper::~block_task_mapper() throw()
{
}

void
block_task_mapper::map_ranks(
  const app_id& aid,
  const ordered_node_set& nodes,
  int ppn,
  std::vector<node_id> &result,
  int nproc)
{
  nproc = validate_nproc(ppn, nodes.size(), nproc, "blockindexing");

  result.clear();
  ordered_node_set::const_iterator it, end = nodes.end();
  long i = 0;

  int num_physical_nodes = nproc / ppn;
  if (nproc % ppn){
    ++num_physical_nodes;
  }
  for(it = nodes.begin(); it != end && i < num_physical_nodes; ++it) {
    node_id addr(*it);

    debug_printf(sprockit::dbg::indexing,
        "block indexing: considering nodeaddr[%d]=%d",
        i, int(addr));
    int64_t this_ppn;

    int next_nproc = result.size() + ppn;
    if (next_nproc > nproc) {
      this_ppn = nproc - result.size();
    }
    else {
      this_ppn = ppn;
    }

    result.insert(result.end(), this_ppn, addr);
    i++;
  }
}

}
} // end of namespace sstmac

