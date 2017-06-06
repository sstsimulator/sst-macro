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

#include <sumi-mpi/mpi_queue/mpi_queue_probe_request.h>
#include <sumi-mpi/mpi_request.h>
#include <sumi-mpi/mpi_status.h>
#include <sprockit/errors.h>

namespace sumi {


//
// Hi there.
//
mpi_queue_probe_request::mpi_queue_probe_request(mpi_request* key,
                                        MPI_Comm comm,
                                        int source, int tag) :
  myid_(comm),
  source_(source),
  tag_(tag),
  key_(key)
{}

//
// Test whether we match a given message.
//
bool
mpi_queue_probe_request::matches(const mpi_message::ptr& message) const
{
  MPI_Comm inid = message->comm();
  bool sameid = (myid_ == inid);
  bool src_matches = source_ == message->src_rank() || source_ == MPI_ANY_SOURCE;
  bool tag_matches = tag_ == message->tag() || tag_ == MPI_ANY_TAG;
  return sameid && src_matches && tag_matches;
}

//
// Consider this request complete.
//
void
mpi_queue_probe_request::complete(const mpi_message::ptr& message)
{
  key_->complete(message);
}

}

