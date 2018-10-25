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
#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/null_buffer.h>

namespace sumi {

void
eager0::start(void* buffer, int src_rank, int dst_rank, sstmac::sw::task_id tid, int count, mpi_type* typeobj,
              int tag, MPI_Comm comm, int seq_id, mpi_request* req)
{
  SSTMACBacktrace(MPIEager0Protocol_Send_Header);
  void* temp_buf = nullptr;
  if (isNonNullBuffer(buffer)){
    temp_buf = fill_send_buffer(count, buffer, typeobj);
  }
  uint64_t flow_id = mpi_->smsg_send<mpi_message>(tid, count*typeobj->packed_size(), temp_buf,
                              queue_->pt2pt_cq_id(), queue_->pt2pt_cq_id(), sumi::message::pt2pt,
                              src_rank, dst_rank, typeobj->id,  tag, comm, seq_id,
                              count, typeobj->packed_size(), nullptr, EAGER0);
  send_flows_[flow_id] = temp_buf;
  req->complete();
}

void
eager0::incoming(mpi_message *msg, mpi_queue_recv_request *req)
{
  if (req->recv_buffer_){
#if SSTMAC_SANITY_CHECK
    if (!msg->smsg_buffer()){
      spkt_abort_printf("have receive buffer, but no send buffer on %s", msg->to_string().c_str());
    }
#endif
    ::memcpy(req->recv_buffer_, msg->smsg_buffer(), msg->byte_length());
  }
#if SSTMAC_COMM_SYNC_STATS
  msg->set_time_synced(queue->now());
#endif
  queue_->notify_probes(msg);
  queue_->memcopy(msg->payload_bytes());
  queue_->finalize_recv(msg, req);
  delete msg;
}

void
eager0::incoming(mpi_message* msg)
{
  SSTMACBacktrace(MPIEager0Protocol_Handle_Header);
  if (msg->sstmac::hw::network_message::type() == mpi_message::payload_sent_ack){
    char* temp_buf = (char*) msg->smsg_buffer();
    if (temp_buf) delete[] temp_buf;
    delete msg;
  } else {
    //I recv this
    mpi_queue_recv_request* req = queue_->find_matching_recv(msg);
    if (req){
      incoming(msg, req);
    } else {
      queue_->buffer_unexpected(msg);
    }
    queue_->notify_probes(msg);
  }
}

}
