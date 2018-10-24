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
#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_debug.h>
#include <sstmac/software/process/backtrace.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/null_buffer.h>

namespace sumi {

rendezvous_protocol::rendezvous_protocol(sprockit::sim_parameters* params, mpi_queue* queue) :
  mpi_protocol(queue)
{
  software_ack_ = params->get_optional_bool_param("software_ack", true);
}

rendezvous_get::~rendezvous_get()
{
}

void*
rendezvous_get::configure_send_buffer(int count, void *buffer, mpi_type* type)
{
  SSTMACBacktrace(MPIRendezvousProtocol_RDMA_Configure_Buffer);
  mpi_->pin_rdma(count*type->packed_size());

  if (isNonNullBuffer(buffer)){
    if (type->contiguous()){
      return buffer;
    } else {
      void* eager_buf = fill_send_buffer(count, buffer, type);
      return eager_buf;
    }
  }//
  return buffer;
}

void
rendezvous_get::start(void* buffer, int src_rank, int dst_rank, sstmac::sw::task_id tid, int count, mpi_type* type,
                      int tag, MPI_Comm comm, int seq_id, mpi_request* req)
{
  void* send_buf = configure_send_buffer(count, buffer, type);
  uint64_t flow_id = mpi_->smsg_send<mpi_message>(tid, sizeof(mpi_message), nullptr,
                             sumi::message::no_ack, mpi_->pt2pt_cq_id(), sumi::message::pt2pt,
                             src_rank, dst_rank, count, type->id, type->packed_size(),
                             tag, comm, seq_id, RENDEZVOUS_GET, send_buf);
  send_flows_.emplace(std::piecewise_construct,
                      std::forward_as_tuple(flow_id),
                      std::forward_as_tuple(req, buffer, send_buf));
}

void
rendezvous_get::incoming_ack(mpi_message *msg)
{
  mpi_queue_protocol_debug("RDMA get incoming ack %s", msg->to_string().c_str());
  auto iter = send_flows_.find(msg->flow_id());
  if (iter == send_flows_.end()){
    spkt_abort_printf("could not find matching ack for %s", msg->to_string().c_str());
    incoming_header(msg);
  }

  auto& s = iter->second;
  if (s.original && s.original != s.temporary){
    //temp got allocated for some reason
    delete[] (char*) s.temporary;
  }
  s.req->complete();
  send_flows_.erase(iter);
}

void
rendezvous_get::incoming(mpi_message* msg)
{
  mpi_queue_protocol_debug("RDMA get incoming %s", msg->to_string().c_str());
  switch(msg->sstmac::hw::network_message::type()){
  case sstmac::hw::network_message::payload: {
    if (msg->stage() == 0){
      incoming_header(msg);
    } else {
      incoming_ack(msg);
    }
    break;
  }
  case sstmac::hw::network_message::rdma_get_payload:
    incoming_payload(msg);
    break;
  case sstmac::hw::network_message::rdma_get_sent_ack: {
    incoming_ack(msg);
    break;
  }
  default:
    spkt_abort_printf("Invalid message type %s to rendezvous protocol",
                      sstmac::hw::network_message::tostr(msg->sstmac::hw::network_message::type()));
  }
}

void
rendezvous_get::incoming_header(mpi_message* msg)
{
  mpi_queue_protocol_debug("RDMA get incoming header %s", msg->to_string().c_str());
  SSTMACBacktrace(MPIRendezvousProtocol_RDMA_Handle_Header);
  mpi_queue_recv_request* req = queue_->find_matching_recv(msg);
  if (req) incoming(msg, req);

  queue_->notify_probes(msg);
}

void
rendezvous_get::incoming(mpi_message *msg, mpi_queue_recv_request* req)
{
  mpi_queue_protocol_debug("RDMA get matched payload %s", msg->to_string().c_str());
#if SSTMAC_COMM_SYNC_STATS
  //this is a bit of a hack
  msg->set_time_synced(queue->now());
#endif
  mpi_->pin_rdma(msg->payload_bytes());
  mpi_queue_action_debug(
    queue_->api()->comm_world()->rank(),
    "found matching request for %s",
    msg->to_string().c_str());

  recv_flows_[msg->flow_id()] = req;
  msg->advance_stage();
  mpi_->rdma_get_request_response(msg, msg->payload_size(), req->recv_buffer_, msg->send_buffer(),
                   mpi_->pt2pt_cq_id(), software_ack_ ? sumi::message::no_ack : mpi_->pt2pt_cq_id());

}

void
rendezvous_get::incoming_payload(mpi_message* msg)
{
  auto iter = recv_flows_.find(msg->flow_id());
  if (iter == recv_flows_.end()){
    spkt_abort_printf("RDMA get protocol has no matching receive for %s", msg->to_string().c_str());
  }

  mpi_queue_recv_request* req = iter->second;
  recv_flows_.erase(iter);

  queue_->finalize_recv(msg, req);
  if (software_ack_){
    msg->advance_stage();
    mpi_->smsg_send_response(msg, sizeof(mpi_message), nullptr,
                             sumi::message::no_ack, mpi_->pt2pt_cq_id());
  } else {
    delete msg;
  }
}

}
