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
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/null_buffer.h>

namespace sumi {

void
eager0::configure_send_buffer(mpi_queue* queue, mpi_message* msg,
                              void *buffer, mpi_type* type)
{
  if (isNonNullBuffer(buffer)){
    void* eager_buf = fill_send_buffer(msg, buffer, type);
    msg->eager_buffer() = eager_buf;
    msg->set_owns_local_buffer(true);
  }
  queue->memcopy(msg->payload_bytes());
}

void
eager0::send_header(mpi_queue* queue,
                    mpi_message* msg)
{
  SSTMACBacktrace(MPIEager0Protocol_Send_Header);
  msg->set_content_type(mpi_message::eager_payload);
  queue->post_header(msg, sumi::message::eager_payload, true/*do need an ack*/);
}

void
eager0::incoming_payload(mpi_queue* queue,
                        mpi_message* msg)
{
  mpi_queue_recv_request* req = queue->pop_pending_request(msg);
  incoming_payload(queue, msg, req);
}

void
eager0::incoming_payload(mpi_queue *queue,
                  mpi_message* msg,
                  mpi_queue_recv_request* req)
{
  SSTMACBacktrace(MPIEager0Protocol_Handle_Header);
  if (req) {
    if (msg->local_buffer().ptr && req->recv_buffer_){
      msg->remote_buffer().ptr = req->recv_buffer_;
      msg->inject_local_to_remote();
    }
#if SSTMAC_COMM_SYNC_STATS
    msg->set_time_synced(queue->now());
#endif
    queue->notify_probes(msg);
    queue->memcopy(msg->payload_bytes());
    queue->finalize_recv(msg, req);
    delete msg;
  }
  else {
    queue->notify_probes(msg);
    queue->buffer_unexpected(msg);
  }
}


}
