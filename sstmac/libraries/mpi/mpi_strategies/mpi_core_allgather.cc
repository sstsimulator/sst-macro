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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_allgather.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_bruck_allgather.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_ring_allgather.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>

ImplementFactory(sstmac::sw::mpi_allgather_strategy);

namespace sstmac {
namespace sw {

SpktRegister("ring", mpi_allgather_strategy, mpi_core_allgather);
SpktRegister("bruck", mpi_allgather_strategy, mpi_bruck_allgather);

//
// Hi.
//
mpi_core_allgather::mpi_core_allgather()
{
}

//
// Goodbye.
//
mpi_core_allgather::~mpi_core_allgather() throw ()
{
}

//
// Build a kernel for an allgather operation
//
mpi_collective*
mpi_core_allgather::execute(mpi_request* req, mpi_queue* queue,
                            int sendcount, mpi_type_id sendtype,
                            int recvcount, mpi_type_id recvtype,
                            mpi_tag tag, mpi_comm* comm,
                            const payload::const_ptr& content, operating_system* os) const
{
  event_handler* completion = queue->progress_done_handler(os, req);
  mpi_ring_allgather* engine = new mpi_ring_allgather(
                                     req, queue, sendcount, sendtype, recvcount, recvtype, tag,
                                     comm, content, completion);

  engine->start();
  return engine;
}

mpi_collective*
mpi_bruck_allgather::execute(mpi_request* req, mpi_queue* queue,
                            int sendcount, mpi_type_id sendtype,
                            int recvcount, mpi_type_id recvtype,
                            mpi_tag tag, mpi_comm* comm,
                            const payload::const_ptr& content, operating_system* os) const
{
  event_handler* completion = queue->progress_done_handler(os, req);
  mpi_bruck_allgather_engine* engine = new mpi_bruck_allgather_engine(
                                     req, queue, sendcount, sendtype, recvcount, recvtype, tag,
                                     comm, content, completion);

  engine->start();
  return engine;
}


}
} // end of namespace sstmac

