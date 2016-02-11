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

#ifndef SSTMAC_COMMON_SSTMAC_ENV_H_INCLUDED
#define SSTMAC_COMMON_SSTMAC_ENV_H_INCLUDED

#include <sprockit/sim_parameters_fwd.h>
#include <sstmac/backends/common/parallel_runtime_fwd.h>

namespace sstmac {

/**
 * This class holds some global information about the simulation
 */
class sstmac_env
{
 public:
  static sprockit::sim_parameters* params;
  static parallel_runtime* rt;
};

}
#endif

