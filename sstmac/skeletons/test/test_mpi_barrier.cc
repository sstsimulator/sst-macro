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

#include <sstmac/skeletons/test/test_mpi_barrier.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/errors.h>

#include <cstring>
#include <stdio.h>

namespace sstmac {
namespace sw {

SpktRegister("test_mpi_barrier", app, test_mpi_barrier);

test_mpi_barrier::~test_mpi_barrier() throw ()
{
}


void
test_mpi_barrier::consume_params(sprockit::sim_parameters* params)
{
  iterations_ = params->get_optional_int_param("test_mpi_barrier_iterations", 10);
}

//
// Go.
//
void
test_mpi_barrier::skeleton_main()
{
  if (mpi() == 0) {
    spkt_throw(sprockit::null_error, "test_mpi_barrier::run:  mpiapi pointer is null");
  }

  timestamp start = mpi()->init();
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();

  for(int i = 0; i < iterations_; i++) {
    mpi()->barrier(world);
  }

  timestamp done = mpi()->finalize();


}

}
} // end of namespace sstmac.

