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

#include <sstmac/skeletons/test/mpi_ping_pong.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sprockit/errors.h>
#include <sprockit/sim_parameters.h>

#include <cstring>
#include <stdio.h>

RegisterDebugSlot(mpi_ping_pong)

namespace sstmac {
namespace sw {

SpktRegister("MPI_pingpong | mpi_pingpong", app, mpi_ping_pong);


mpi_ping_pong::~mpi_ping_pong() throw ()
{
}

void
mpi_ping_pong::consume_params(sprockit::sim_parameters* params)
{
  iterations_ = params->get_int_param("mpipingpong_iterations");
  count_ = params->get_int_param("mpipingpong_count");
}

void
mpi_ping_pong::skeleton_main()
{
  if (mpi() == 0) {
    spkt_throw(sprockit::null_error, "mpi_ping_pong::run:  mpi_api pointer is null");
  }

  timestamp start = mpi()->init();
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();
  ping_pong_debug("rank = %d, size = %d", int(rank), int(size));

  mpi_tag tag(0);
  bool participant = true;
  if ((size % 2) && (int(rank)+ 1 >= size)) {
    // This is the odd-node-out -- communicating with no-one.
    participant = false;
  }
  if (participant) {
    mpi_id buddy(int(rank)^ 1); // 0<=>1, 2<=>3, etc.
    ping_pong_debug("rank = %d is participating", int(rank));
    mpi_status stat;
    for (int half_cycle = 0; half_cycle < 2 * iterations_; ++half_cycle) {
      ping_pong_debug("rank = %d starting half_cycle %d", int(rank), half_cycle);
      if ((half_cycle + rank) & 1) {
        // even values of half-cycle plus rank.
        timestamp t(1e-8);
        ping_pong_debug("rank = %d about to compute", int(rank), half_cycle);
        compute(t);
        ping_pong_debug("rank = %d finished computing", int(rank), half_cycle);
        ping_pong_debug("rank = %d about to send", int(rank), half_cycle);
        timestamp st = mpi()->send(count_, mpi_type::mpi_double->id, buddy,
                                   tag, world);
        ping_pong_debug("rank = %d finished sending", int(rank), half_cycle);
      }
      else {
        ping_pong_debug("rank = %d about to receive", int(rank), half_cycle);
        timestamp st = mpi()->recv(count_, mpi_type::mpi_double->id, buddy,
                                   tag, world, &stat);
        ping_pong_debug("rank = %d finished receiving", int(rank), half_cycle);
      }
      ping_pong_debug("rank = %d finished half_cycle %d", int(rank), half_cycle);
    }
  }
  timestamp done = mpi()->finalize();
}

}
} // end of namespace sstmac.

