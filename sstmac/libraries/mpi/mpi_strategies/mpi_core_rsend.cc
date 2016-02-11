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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_rsend.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/libraries/unblock_handler.h>

namespace sstmac {
namespace sw {


//
// Goodbye.
//
mpi_core_rsend::~mpi_core_rsend() throw ()
{
}

//
// Basic nonblocking send.
//
void
mpi_core_rsend::execute(mpi_request* req,
                        mpi_queue* queue,
                        int count, mpi_type_id type,
                        mpi_id dest, mpi_tag tag, mpi_comm* comm,
                        const payload::const_ptr& content,
                        operating_system* os) const
{
  bool rsend = true, ssend = false;
  event_handler* completion = queue->progress_done_handler(os, req);
  mpi_queue::sendinfo sinfo;
  sinfo.rsend = true;
  queue->send(req, count, type, dest, tag, comm, sinfo,
              mpi_message::user, completion, content);
}

}
} // end of namespace sstmac.

