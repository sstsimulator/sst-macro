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

#include <sstmac/software/launch/random_task_mapper.h>
#include <sprockit/errors.h>
#include <sprockit/sim_parameters.h>
#include <algorithm>
#include <stdlib.h>

namespace sstmac {
namespace sw {

SpktRegister("random", task_mapper, random_task_mapper,
            "randomly assigns tasks to nodes");

random_task_mapper::random_task_mapper(sprockit::sim_parameters *params) :
  task_mapper(params)
{
  if(params->has_param("random_indexer_seed")) {
    long seed = params->get_long_param("random_indexer_seed");
    rng_ = RNG::SimpleCombo::construct(seed);
  }
  else {
    rng_ = RNG::SimpleCombo::construct();
  }
}

random_task_mapper::~random_task_mapper() throw ()
{
}

void
random_task_mapper::map_ranks(
  const ordered_node_set& nodes,
  int ppn,
  std::vector<node_id> &result,
  int nproc)
{
  nproc = validate_nproc(ppn, nodes.size(), nproc, "randomindexing");

  ordered_node_set::iterator iter = nodes.begin();
  for(long i = 0; i < nproc / ppn; i++) {
    iter++;
  }

  // result = nodes;
  for (int i = 0; i < ppn; ++i) {
    result.insert(result.end(), nodes.begin(), iter);
  }

  RNG::UniformInteger_functor rngf(rng_);
  std::random_shuffle(result.begin(), result.end(), rngf);

  result.resize(nproc);
}

}
} // end of namespace sstmac

