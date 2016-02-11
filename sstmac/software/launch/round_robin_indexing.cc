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

#include <sstmac/software/launch/round_robin_indexing.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {
SpktRegister("round | round_robin", index_strategy, round_robin_indexing,
            "Tries to spread out ranks across the nodes. Ranks 0,1,2,3 are on different nodes. Ranks 0,4,8 are on the same node. Round robin and block indexing are equivalent if there is one process per node."
           );


round_robin_indexing::~round_robin_indexing() throw ()
{
}

void
round_robin_indexing::allocate(
  const app_id& aid,
  const node_set &nodes,
  int ppn,
  std::vector<node_id> &result,
  int nproc)
{
  nproc = validate_nproc(ppn, nodes.size(), nproc, "blockindexing");

  node_set::iterator iter = nodes.begin();
  for (long i = 0; i < nproc / ppn; i++) {
    iter++;
  }

  for (int i = 0; i < ppn; ++i) {
    result.insert(result.end(), nodes.begin(), iter);
  }
}

}
} // end of namespace sstmac

