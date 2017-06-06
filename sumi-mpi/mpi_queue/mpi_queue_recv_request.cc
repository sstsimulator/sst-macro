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

#include <sumi-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sumi-mpi/mpi_protocol/mpi_protocol.h>
#include <sumi-mpi/mpi_message.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sumi-mpi/mpi_debug.h>
#include <sumi-mpi/mpi_api.h>
#include <sprockit/debug.h>
#include <sprockit/errors.h>

namespace sumi {

mpi_queue_recv_request::mpi_queue_recv_request(
  mpi_request* key,
  mpi_queue* queue,
  int count,
  MPI_Datatype type,
  int source, int tag, MPI_Comm comm, void* buffer) :
  queue_(queue), source_(source), tag_(tag), comm_(comm),
  seqnum_(0), count_(count), type_(queue->api()->type_from_id(type)),
  key_(key), final_buffer_(buffer), recv_buffer_(nullptr)
{
  if (buffer && !type_->contiguous()){
    recv_buffer_ = new char[count*type_->packed_size()];
  } else {
    recv_buffer_ = (char*) final_buffer_;
  }
}

mpi_queue_recv_request::~mpi_queue_recv_request()
{
}

bool
mpi_queue_recv_request::is_cancelled() const {
  return key_ && key_->is_cancelled();
}

bool
mpi_queue_recv_request::matches(const mpi_message::ptr& msg)
{
  bool count_equals = true; //count_ == msg->count();
  bool comm_equals = comm_ == msg->comm();
  bool seq_equals = true; //seqnum_ == msg->seqnum();
  bool src_equals = source_ == msg->src_rank() || source_ == MPI_ANY_SOURCE;
  bool tag_equals = tag_ == msg->tag() || tag_ == MPI_ANY_TAG;
  bool match = comm_equals && seq_equals && src_equals && tag_equals && count_equals;

  if (match){
    int incoming_bytes = msg->payload_bytes();
    mpi_api* api = queue_->api();
    int recv_buffer_size = count_ * type_->packed_size();
    if (incoming_bytes > recv_buffer_size){
      spkt_abort_printf("MPI matching error: incoming message has %d bytes, but matches buffer of too small size %d:\n"
                        "MPI_Recv(%d,%s,%s,%s,%s) matches\n"
                        "MPI_Send(%d,%s,%d,%s,%s)",
                        incoming_bytes, recv_buffer_size,
                        count_, api->type_str(type_->id).c_str(),
                        api->src_str(source_).c_str(), api->tag_str(tag_).c_str(),
                        api->comm_str(comm_).c_str(),
                        msg->count(), api->type_str(msg->type()).c_str(), msg->dst_rank(),
                        api->tag_str(msg->tag()).c_str(), api->comm_str(msg->comm()).c_str());
    }
  }
  return match;
}

}