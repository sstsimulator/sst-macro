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
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sstmac/software/process/backtrace.h>

namespace sumi {

void
eager1::configure_send_buffer(mpi_queue* queue, const mpi_message::ptr& msg, void *buffer, mpi_type* typeobj)
{
  if (buffer){
    void* eager_buf = fill_send_buffer(msg, buffer, typeobj);
    msg->remote_buffer().ptr = eager_buf;
  }
}

void
eager1::send_header(mpi_queue* queue,
                    const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Eager 1 Protocol: Send RDMA Header");
  msg->set_content_type(mpi_message::header);
  queue->memcopy(msg->byte_length());

  queue->post_header(msg, sumi::message::header, false/*the send is "done" - no need to ack*/);

  /** the send will have copied into a temp buffer so we can 'ack' the buffer for now */
  mpi_queue::send_needs_ack_t::iterator it, end =
    queue->send_needs_eager_ack_.end();
  for (it = queue->send_needs_eager_ack_.begin(); it != end; ++it) {
    if ((*it)->matches(msg)) {
      // Match.
      mpi_queue_send_request* sreq = *it;
      sreq->complete(msg);
      queue->send_needs_eager_ack_.erase(it);
      delete sreq;
      return;
    }
  }
  spkt_throw_printf(sprockit::illformed_error,
        "eager1 protocol could not find request to complete");
}

void
eager1::incoming_header(mpi_queue *queue,
                        const mpi_message::ptr &msg)
{
  mpi_queue_recv_request* req =
    queue->pop_pending_request(msg, false);
  incoming_header(queue, msg, req);
}

void
eager1::incoming_header(mpi_queue* queue,
                        const mpi_message::ptr& msg,
                        mpi_queue_recv_request* req)
{
  SSTMACBacktrace("MPI Eager 1 Protocol: Handle RDMA Header");
  if (req) {
    //we can post an RDMA get request direct to the buffer
    //make sure to put the request back in, but alert it
    //that it should expect a data payload next time
    req->set_seqnum(msg->seqnum()); //set seqnum to avoid accidental matches
    queue->waiting_message_.push_front(req);
    req->set_seqnum(msg->seqnum()); //associate the messages
    msg->local_buffer().ptr = req->recv_buffer_;
  }
  else {
    msg->set_protocol(mpi_protocol::eager1_doublecpy_protocol);
    //this has to go in now
    //the need recv buffer has to push back messages in the order they are received
    //in order to preserve message order semantics
    queue->need_recv_.push_back(msg);
  }
  queue->notify_probes(msg);

  // this has already been received by mpi in sequence
  // make sure mpi still handles this since it won't match
  // the current sequence number
  msg->set_in_flight(true);
  msg->set_content_type(mpi_message::data);
  // generate an ack ONLY on the recv end
  queue->post_rdma(msg, false, true);
}

void
eager1_singlecpy::incoming_payload(mpi_queue *queue,
                         const mpi_message::ptr &msg)
{
  mpi_queue_recv_request* req = queue->pop_waiting_request(msg);
  //guaranteed that req is posted before payload arrives
  incoming_payload(queue, msg, req);
}

void
eager1_singlecpy::incoming_payload(mpi_queue *queue,
                                   const mpi_message::ptr &msg,
                                   mpi_queue_recv_request *req)
{
  if (!req){
    spkt_throw(sprockit::value_error,
               "eager1_singlecpy::incoming_payload: null recv request");
  }
  SSTMACBacktrace("MPI Eager 1 Protocol: Handle RDMA Payload");
  //already RDMA'd correctly - just finish
  queue->finalize_recv(msg, req);
}

void
eager1_doublecpy::incoming_payload(mpi_queue *queue,
                         const mpi_message::ptr &msg)
{
  mpi_queue_recv_request* req = queue->pop_matching_request(queue->in_flight_messages_, msg);
  //guaranteed that msg arrived before recv was posted
  incoming_payload(queue, msg, req);
}

void
eager1_doublecpy::incoming_payload(mpi_queue* queue,
                                   const mpi_message::ptr& msg,
                                   mpi_queue_recv_request* req)
{
  SSTMACBacktrace("MPI Eager 1 Protocol: Handle RDMA Payload");
  //We did not RDMA get directly into the buffer
  //finish the transfer
  msg->set_in_flight(false);
  if (req){
    if (req->recv_buffer_){
      msg->local_buffer().ptr = req->recv_buffer_;
      //bypass mpi-message - actually do the move
      msg->message::move_remote_to_local();
    }
    queue->finalize_recv(msg, req);
  }
}


}