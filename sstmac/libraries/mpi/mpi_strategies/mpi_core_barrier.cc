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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_barrier.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_barrier_engine.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_linear_barrier_engine.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_ring_barrier_engine.h>

#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/hfm_iterator.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/rd_iterator.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/bt_scatter_iterator.h>
#include <sstmac/common/event_handler.h>

namespace sstmac {
namespace sw {

//
// Hello.
//
mpi_core_barrier::mpi_core_barrier(BARRIER_IMPL i) :
  impl_(i)
{
}


//
// Goodbye.
//
mpi_core_barrier::~mpi_core_barrier() throw ()
{
}

//
// Build a kernel to synchronize nodes.
//
mpi_collective*
mpi_core_barrier::execute(mpi_request* req,
                          mpi_queue* queue, mpi_tag tag, mpi_comm* comm,
                          operating_system* os) const
{
  event_handler* completion = queue->progress_done_handler(os, req);

  // Here we might want to pick different iterators depending on message size.
  topology_iterator* iter;

  switch (impl_) {
    case HFM: {
      iter = new hfm_iterator(comm->rank(), comm->size());
      mpi_barrier_engine* engine = new mpi_barrier_engine(
        iter, req, queue, tag, comm, completion);
      engine->start();
      return engine;
    }

    case LINEAR: {
      mpi_linear_barrier_engine* engine = new mpi_linear_barrier_engine(
        iter, req, queue, tag, comm, mpi_id(0), completion);
      engine->start();
      return engine;
    }

    case RING:
      mpi_ring_barrier_engine* engine = new mpi_ring_barrier_engine(
        iter, req, queue, tag, comm, completion);
      engine->start();
      return engine;
  }
  return 0;
}

}
} // end of namespace sstmac

