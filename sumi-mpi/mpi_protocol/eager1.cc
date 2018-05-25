/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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
#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/null_buffer.h>

namespace sumi {

void
eager1::configure_send_buffer(mpi_queue* queue, mpi_message* msg, void *buffer, mpi_type* typeobj)
{
  if (isNonNullBuffer(buffer)){
    void* eager_buf = fill_send_buffer(msg, buffer, typeobj);
    msg->remote_buffer().ptr = eager_buf;
    msg->set_owns_remote_buffer(true);
  }
  queue->memcopy(msg->payload_bytes());
}

void
eager1::send_header(mpi_queue* queue,
                    mpi_message* msg)
{
  SSTMACBacktrace(MPIEager1Protocol_Send_RDMA_Header);
  msg->set_content_type(mpi_message::header);

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
                        mpi_message*msg)
{
  mpi_queue_recv_request* req =
    queue->pop_pending_request(msg, false);
  incoming_header(queue, msg, req);
}

void
eager1::incoming_header(mpi_queue* queue,
                        mpi_message* msg,
                        mpi_queue_recv_request* req)
{
  SSTMACBacktrace(MPIEager1Protocol_Handle_RDMA_Header);
  if (req) {
    //we can post an RDMA get request direct to the buffer
    //make sure to put the request back in, but alert it
    //that it should expect a data payload next time
    req->set_seqnum(msg->seqnum()); //set seqnum to avoid accidental matches
    queue->waiting_message_.push_front(req);
    req->set_seqnum(msg->seqnum()); //associate the messages
    msg->local_buffer().ptr = req->recv_buffer_;
  } else {
    auto& rbuf = msg->remote_buffer();
    if (rbuf.ptr){
      auto& lbuf = msg->local_buffer();
      lbuf.ptr = new char[msg->payload_bytes()];
    }
    msg->set_protocol(mpi_protocol::eager1_doublecpy_protocol);
    //this has to go in now
    //the need recv buffer has to push back messages in the order they are received
    //in order to preserve message order semantics
    mpi_message* cln = msg->clone_me();
    cln->local_buffer().ptr = msg->local_buffer().ptr;
    cln->set_in_flight(true);
    queue->need_recv_.push_back(cln);
  }
  msg->set_in_flight(true);
  queue->notify_probes(msg);

  // this has already been received by mpi in sequence
  // make sure mpi still handles this since it won't match
  // the current sequence number
  msg->set_content_type(mpi_message::data);
  // generate an ack ONLY on the recv end
  queue->post_rdma(msg, false, true);
}

void
eager1_singlecpy::incoming_payload(mpi_queue *queue,
                         mpi_message*msg)
{
  mpi_queue_recv_request* req = queue->pop_waiting_request(msg);
  //guaranteed that req is posted before payload arrives
  incoming_payload(queue, msg, req);
  delete msg;
}

void
eager1_singlecpy::incoming_payload(mpi_queue *queue,
                                   mpi_message*msg,
                                   mpi_queue_recv_request *req)
{
  if (!req){
    sprockit::abort("eager1_singlecpy::incoming_payload: null recv request");
  }
  SSTMACBacktrace(MPIEager1Protocol_Handle_RDMA_Payload);
  //already RDMA'd correctly - just finish
  queue->memcopy(msg->payload_bytes()); //simulate
  queue->finalize_recv(msg, req);
}

void
eager1_doublecpy::incoming_payload(mpi_queue* queue, mpi_message* msg)
{
  auto iter = queue->in_flight_messages_.find(msg->unique_int());
  mpi_queue_recv_request* req = nullptr;
  int taskid_ = queue->api()->rank();
  if (iter != queue->in_flight_messages_.end()){
    mpi_queue_debug("matched request to message %s", msg->to_string().c_str());
    req = iter->second;
    queue->in_flight_messages_.erase(iter);
  } else {
    mpi_queue_debug("did not match request to message %s", msg->to_string().c_str());
  }
  incoming_payload(queue, msg, req);
}

void
eager1_doublecpy::incoming_payload(mpi_queue* queue, mpi_message* msg,
                                   mpi_queue_recv_request* req)
{
  SSTMACBacktrace(MPIEager1Protocol_Handle_RDMA_Payload);
  //We did not RDMA get directly into the buffer
  //finish the transfer
  msg->set_in_flight(false);
  if (req){
    if (req->recv_buffer_){
      char* temp_buf = (char*) msg->local_buffer().ptr;
      ::memcpy(req->recv_buffer_, temp_buf, msg->payload_bytes());
      delete[] temp_buf;
      msg->local_buffer().ptr = temp_buf;
    }
    queue->memcopy(msg->payload_bytes());
    queue->finalize_recv(msg, req);
    fflush(stdout);
  } else {
    //drop a sentinel value to indicate the payload is here
    queue->in_flight_messages_[msg->unique_int()] = nullptr;
  }
  delete msg;
}


}
