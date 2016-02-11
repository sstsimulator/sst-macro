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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_gatherv.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_direct_gatherv.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/libraries/unblock_handler.h>

namespace sstmac {
namespace sw {

//
// Perform a fused reduce-gatherv operation
//
mpi_collective*
mpi_core_gatherv::execute(mpi_request* req,
                          mpi_queue* queue,
                          int sendcount, mpi_type_id sendtype,
                          const std::vector<int> &recvcount,
                          mpi_type_id recvtype,
                          mpi_id root, mpi_tag tag,
                          mpi_comm* comm,
                          const payload::const_ptr& content,
                          operating_system* os) const
{
  event_handler* completion = queue->progress_done_handler(os, req);
  mpi_direct_gatherv* engine = new mpi_direct_gatherv(
                                     req, queue, sendcount, sendtype, recvcount, recvtype, root, tag,
                                     comm, content, completion);
  engine->start();
  return engine;
}

}
} // end of namespace sstmac

