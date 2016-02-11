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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_alltoall.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_direct_alltoall.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/libraries/unblock_handler.h>

namespace sstmac {
namespace sw {

//
// Must be virtual.
//
mpi_core_alltoall::~mpi_core_alltoall() throw ()
{
}

//
// Build a kernel.
//
mpi_collective*
mpi_core_alltoall::execute(mpi_request* req, mpi_queue* queue,
                           int sendcnt, mpi_type_id sendtype,
                           int recvcnt, mpi_type_id recvtype,
                           mpi_tag tag, mpi_comm* comm,
                           const std::vector<payload::const_ptr >& content,
                           operating_system* os) const
{
  std::vector<int> sendvec(comm->size(), sendcnt);
  std::vector<int> recvvec(comm->size(), recvcnt);

  event_handler* completion = queue->progress_done_handler(os, req);
  mpi_direct_alltoall* engine = new mpi_direct_alltoall(
                                      req, queue, sendvec, sendtype, recvvec, recvtype, tag, comm,
                                      content, completion);

  engine->start();
  return engine;
}

}
} // end of namespace sstmac

