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

#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_probe_request.h>
#include <sstmac/libraries/mpi/mpi_request.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/common/event_handler.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

//
// Hi there.
//
mpi_queue_probe_request::mpi_queue_probe_request(mpi_request* key,
                                        mpi_comm* comm,
                                        mpi_id source, mpi_tag tag,
                                        event_handler* completion) :
  myid_(comm->id()), source_(source), tag_(tag), completion_(completion),
  key_(key)
{}

//
// Test whether we match a given message.
//
bool
mpi_queue_probe_request::matches(mpi_message* message) const
{
  mpi_comm_id inid = message->commid();
  bool sameid = (myid_ == inid);
  bool src_matches = source_ == message->source() || source_ == mpi::any_source;
  bool tag_matches = tag_ == message->tag() || tag_ == mpi::any_tag;
  return sameid && src_matches && tag_matches;
}

//
// Consider this request complete.
//
void
mpi_queue_probe_request::complete(mpi_message* message)
{
  if (key_) key_->complete(message);
  if (completion_) completion_->handle(message);
}

}
} // end of namespace sstmac.

