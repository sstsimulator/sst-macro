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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_allgatherv.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_ring_allgatherv.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/libraries/unblock_handler.h>

namespace sstmac {
namespace sw {


//
// Goodbye.
//
mpi_core_allgatherv::~mpi_core_allgatherv() throw()
{
}

//
// Build a kernel for an allgatherv operation
//
mpi_collective*
mpi_core_allgatherv::execute(mpi_request* req,
                             mpi_queue* queue,
                             int sendcount, mpi_type_id sendtype,
                             const std::vector<int> &recvcount,
                             mpi_type_id recvtype,
                             mpi_tag tag, mpi_comm* comm,
                             const payload::const_ptr& content,
                             operating_system* os
                            ) const
{
  event_handler* completion = queue->progress_done_handler(os, req);
  mpi_ring_allgatherv* engine = new mpi_ring_allgatherv(
                                  req, queue, sendcount, sendtype, recvcount, recvtype, tag,
                                  comm, content, completion);

  engine->start();
  return engine;
}

}
} // end of namespace sstmac

