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

#ifndef SSTMAC_BACKENDS_NATIVE_LAUNCH_RANDOMINDEXING_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_LAUNCH_RANDOMINDEXING_H_INCLUDED

#include <sstmac/software/launch/index_strategy.h>
#include <sstmac/common/rng.h>

namespace sstmac {
namespace sw {

/**
 * An index strategy that allocates indices using a round robin.
 */
class random_indexing : public index_strategy
{

 public:
  virtual void
  init_factory_params(sprockit::sim_parameters *params);

  virtual
  ~random_indexing() throw ();

  virtual void
  allocate(
    const app_id& aid,
    const node_set &nodes,
    int ppn,
    std::vector<node_id> &result,
    int nproc);

 protected:
  RNG::UniformInteger* rng_;

};

}
} // end of namespace sstmac.

#endif

