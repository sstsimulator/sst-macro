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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_bcast.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/bt_scatter_iterator.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_bcast_engine.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/libraries/unblock_handler.h>

namespace sstmac {
namespace sw {


//
// Goodbye.
//
mpi_core_bcast::~mpi_core_bcast() throw ()
{
}


//
// Build a kernel to perform a broadcast.
//
mpi_collective*
mpi_core_bcast::execute(mpi_request* req,
                        mpi_queue* queue,
                        int count, mpi_type_id type,
                        mpi_id root, mpi_tag tag,
                        mpi_comm* comm,
                        const payload::const_ptr& content,
                        operating_system* os) const
{
  topology_iterator* iter = new bt_scatter_iterator(comm->rank(), comm->size(), root);
  event_handler* completion = queue->progress_done_handler(os, req);
  mpi_bcast_engine* engine = new mpi_bcast_engine(iter, req, queue,
                                 count, type, root,
                                 tag, comm, content, completion);
  engine->start();

  return engine;
}

}
} // end of namespace sstmac

