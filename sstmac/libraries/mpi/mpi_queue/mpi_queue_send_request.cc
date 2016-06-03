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

#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_send_request.h>
#include <sstmac/libraries/mpi/mpi_server.h>
#include <sstmac/libraries/mpi/mpi_message.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/common/messages/payload.h>

namespace sstmac {
namespace sw {

mpi_queue_send_request::mpi_queue_send_request(mpi_message* mess,
                                    mpi_request* key,
                                    mpi_queue* queue,
                                    event_handler* completion) :
  queue_(queue),
  completion_(completion),
  key_(key),
  dest_(mess->dest()),
  src_(mess->source()),
  seqnum_(mess->seqnum()),
  tag_(mess->tag())
{
}

//
// Goodbye.
//
mpi_queue_send_request::~mpi_queue_send_request() throw ()
{
  delete completion_;
}

//
// Test whether we match a given message, for nic send acks.
//
bool
mpi_queue_send_request::matches(mpi_message* message) const
{
  bool same_id = seqnum_ == message->seqnum();
  bool tag_matches = tag_ == message->tag();

  //if this is an ack, it will be reversed - a bit weird
  bool dest_matches, src_matches;
  switch (message->hw::network_message::type())
  {
  case hw::network_message::rdma_get_sent_ack:
  case hw::network_message::rdma_put_sent_ack:
  case hw::network_message::payload_sent_ack:
    dest_matches = dest_ == message->source();
    src_matches = src_ == message->dest();
    break;
  default:
    dest_matches = dest_ == message->dest();
    src_matches = src_ == message->source();
    break;
  }
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
  completion_ = 0;
}

void
mpi_queue_send_request::complete(mpi_message* msg)
{
  mpi_queue_action_debug(queue_->taskid_,
    "completing send_request dst=%d, tag=%d, cat=%s, count=%d, type=%s, seqnum=%d",
    int(dest_), int(msg->tag()), mpi_message::str(msg->cat()),
    msg->count(), queue_->api()->type_from_id(msg->type())->label.c_str(),
    seqnum_);

  if (key_) {
    //don't build a status - send request
    key_->complete(NULL);
  }

  if (completion_) {
    completion_->handle(msg);
  }

}

}
} // end of namespace sstmac.

