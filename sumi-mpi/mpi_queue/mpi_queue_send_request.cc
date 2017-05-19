/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sumi-mpi/mpi_protocol/mpi_protocol.h>
#include <sumi-mpi/mpi_queue/mpi_queue_send_request.h>
#include <sumi-mpi/mpi_message.h>
#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>

namespace sumi {


mpi_queue_send_request::mpi_queue_send_request(
    const mpi_message::ptr& mess,
    mpi_request* key,
    mpi_queue* queue) :
  queue_(queue),
  key_(key),
  dest_(mess->dst_rank()),
  src_(mess->src_rank()),
  seqnum_(mess->seqnum()),
  tag_(mess->tag())
{
}

//
// Goodbye.
//
mpi_queue_send_request::~mpi_queue_send_request() throw ()
{
}

//
// Test whether we match a given message, for nic send acks.
//
bool
mpi_queue_send_request::matches(const mpi_message::ptr& message) const
{
  bool same_id = seqnum_ == message->seqnum();
  bool tag_matches = tag_ == message->tag();
  bool dest_matches = dest_ == message->dst_rank();
  bool src_matches = src_ == message->src_rank();
  return same_id && dest_matches && src_matches && tag_matches;
}

void
mpi_queue_send_request::wait_for_buffer()
{
  /**
    CHANGE! At some point, we may want to track buffer
    resources for MPI. For now, let's not worry about buffers.
    Just clear the completion handlers and use this as a
    placeholder for clearing nic acks
  */
  key_ = nullptr;
}

void
mpi_queue_send_request::complete(const mpi_message::ptr& msg)
{
  mpi_queue_action_debug(queue_->taskid_,
    "completing send_request dst=%d, tag=%d, count=%d, type=%s, seqnum=%d",
    dest_, msg->tag(),
    msg->count(),
    queue_->api()->type_from_id(msg->type())->label.c_str(),
    seqnum_);
  key_->complete();
}

}