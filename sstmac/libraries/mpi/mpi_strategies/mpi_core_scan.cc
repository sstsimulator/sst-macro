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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_scan.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_simultaneous_btree_scan.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>

namespace sstmac {
namespace sw {


//
// Destructor.
//
mpi_core_scan::~mpi_core_scan() throw()
{
}

//
// Build a kernel to perform a scan operation
//
mpi_collective*
mpi_core_scan::execute(mpi_request* req,
                       mpi_queue* queue,
                       int count, mpi_type_id type, mpi_op* op,
                       mpi_tag tag, mpi_comm* comm,
                       const payload::const_ptr& content,
                       operating_system* os) const
{
  event_handler* completion = queue->progress_done_handler(os, req);
  mpi_simultaneous_btree_scan_engine* engine = new mpi_simultaneous_btree_scan_engine(
      req, queue, count, type, op, tag, comm, content, completion);

  engine->start();
  return engine;
}

}
} // end of namespace sstmac


