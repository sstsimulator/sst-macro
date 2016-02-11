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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_reduce_scatter.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_halving_reduce_scatter.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>

namespace sstmac {
namespace sw {


//
// Goodbye
//
mpi_core_reduce_scatter::~mpi_core_reduce_scatter() throw()
{
}

//
// Perform a fused reduce-scatter operation
//
mpi_collective*
mpi_core_reduce_scatter::execute(mpi_request* req,
                                 mpi_queue* queue,
                                 const std::vector<int> &recvcnts,
                                 mpi_type_id type, mpi_op* op,
                                 mpi_tag tag, mpi_comm* comm,
                                 const payload::const_ptr& content,
                                 operating_system* os) const
{
  event_handler* completion = queue->progress_done_handler(os, req);
  mpi_halving_reduce_scatter* engine = new mpi_halving_reduce_scatter(
        req, queue, recvcnts, type, op, tag, comm, content, completion);

  engine->start();
  return engine;
}

}
} // end of namespace sstmac

