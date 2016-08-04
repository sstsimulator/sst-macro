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
  key_ = 0;
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

  if (key_) {
    //don't build a status - send request
    key_->complete();
  }

}

}

