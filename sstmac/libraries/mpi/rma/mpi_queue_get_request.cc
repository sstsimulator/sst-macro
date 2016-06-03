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

#include <sstmac/libraries/mpi/rma/mpi_queue_get_request.h>
#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>

namespace sstmac {
namespace sw {

//
// Hello.
//
mpi_queue_get_request::mpi_queue_get_request(mpi_request* key,
                                    mpi_queue* queue, event_handler* completion) :
  queue_(queue), key_(key), completion_(completion)
{
}

//
// We be done.
//
void
mpi_queue_get_request::handle(mpi_message* mess)
{
  key_->complete(mess);
  completion_->handle(mess);
}

}
} // end of namespace sstmac

