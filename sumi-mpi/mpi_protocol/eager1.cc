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
#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/null_buffer.h>

namespace sumi {

void
eager1::start(void* buffer, int src_rank, int dst_rank, sstmac::sw::task_id tid, int count,
              mpi_type* typeobj, int tag, MPI_Comm comm, int seq_id, mpi_request* key)
{
  void* eager_buf = nullptr;
  if (isNonNullBuffer(buffer)){
    eager_buf = fill_send_buffer(count, buffer, typeobj);
  }

  uint64_t flow_id = mpi_->smsg_send<mpi_message>(tid, sizeof(mpi_message)/*metadata size*/, nullptr,
                                     sumi::message::no_ack, mpi_->pt2pt_cq_id(), sumi::message::pt2pt,
                                     src_rank, dst_rank, count, typeobj->id, typeobj->packed_size(),
                                     tag, comm, seq_id, EAGER1, eager_buf);

  key->complete();
}

void
eager1::incoming_header(mpi_message* msg)
{
  char* send_buf = (char*) msg->send_buffer();
  char* recv_buf = nullptr;
  if (send_buf){
    recv_buf = new char[msg->payload_size()];
  }
  msg->advance_stage();
  mpi_->rdma_get_request_response(msg, msg->payload_size(), recv_buf, send_buf,
                                  mpi_->pt2pt_cq_id(), mpi_->pt2pt_cq_id());
}

void
eager1::incoming_payload(mpi_message* msg)
{
  SSTMACBacktrace(MPIEager1Protocol_Handle_RDMA_Payload);
  mpi_queue_recv_request* req = queue_->find_matching_recv(msg);
  if (req) incoming(msg, req);
}

void
eager1::incoming(mpi_message *msg, mpi_queue_recv_request* req)
{
  if (req->recv_buffer_){
    char* temp_recv_buf = (char*) msg->local_buffer();
#if SSTMAC_SANITY_CHECK
    if (!temp_recv_buf){
      spkt_abort_printf("have receiver buffer but no local buffer on %s", msg->to_string().c_str());
    }
#endif
    ::memcpy(req->recv_buffer_, temp_recv_buf, msg->payload_bytes());
    delete[] temp_recv_buf;
  }
  queue_->memcopy(msg->payload_bytes());
  queue_->finalize_recv(msg, req);
  delete msg;
}

void
eager1::incoming_ack(mpi_message *msg)
{
  if (msg->remote_buffer()){
    char* temp_send_buf = (char*) msg->remote_buffer();
    delete[] temp_send_buf;
  }
  delete msg;
}

void
eager1::incoming(mpi_message* msg)
{
  switch(msg->network_message::type()){
  case sstmac::hw::network_message::payload:
    incoming_header(msg);
    break;
  case sstmac::hw::network_message::rdma_get_sent_ack:
    incoming_ack(msg);
    break;
  case sstmac::hw::network_message::rdma_get_payload:
    incoming_payload(msg);
    break;
  default:
    spkt_abort_printf("Got bad message type %s for eager1::incoming",
                      sstmac::hw::network_message::tostr(msg->network_message::type()));
  }
}


}
