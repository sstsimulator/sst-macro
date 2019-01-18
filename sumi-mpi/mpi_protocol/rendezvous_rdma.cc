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

RendezvousProtocol::RendezvousProtocol(SST::Params& params, MpiQueue* queue) :
  MpiProtocol(queue)
{
  software_ack_ = params->get_optional_bool_param("software_ack", true);
}

RendezvousGet::~RendezvousGet()
{
}

void*
RendezvousGet::configure_send_buffer(int count, void *buffer, MpiType* type)
{
  SSTMACBacktrace(MPIRendezvousProtocol_RDMA_Configure_Buffer);
  mpi_->pinRdma(count*type->packed_size());

  if (isNonNullBuffer(buffer)){
    if (type->contiguous()){
      return buffer;
    } else {
      void* eager_buf = fillSendBuffer(count, buffer, type);
      return eager_buf;
    }
  }//
  return buffer;
}

void
RendezvousGet::start(void* buffer, int src_rank, int dst_rank, sstmac::sw::TaskId tid, int count, MpiType* type,
                      int tag, MPI_Comm comm, int seq_id, MpiRequest* req)
{
  void* send_buf = configure_send_buffer(count, buffer, type);
  uint64_t flow_id = mpi_->smsgSend<MpiMessage>(tid, 64/*fixed size, not sizeof()*/, nullptr,
                             sumi::Message::no_ack, queue_->pt2ptCqId(), sumi::Message::pt2pt,
                             src_rank, dst_rank, type->id,  tag, comm, seq_id,
                             count, type->packed_size(), send_buf, RENDEZVOUS_GET);
  send_flows_.emplace(std::piecewise_construct,
                      std::forward_as_tuple(flow_id),
                      std::forward_as_tuple(req, buffer, send_buf));
}

void
RendezvousGet::incoming_ack(MpiMessage *msg)
{
  mpi_queue_protocol_debug("RDMA get incoming ack %s", msg->toString().c_str());
  auto iter = send_flows_.find(msg->flowId());
  if (iter == send_flows_.end()){
    spkt_abort_printf("could not find matching ack for %s", msg->toString().c_str());
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
RendezvousGet::incoming(MpiMessage* msg)
{
  mpi_queue_protocol_debug("RDMA get incoming %s", msg->toString().c_str());
  switch(msg->sstmac::hw::NetworkMessage::type()){
  case sstmac::hw::NetworkMessage::payload: {
    if (msg->stage() == 0){
      incoming_header(msg);
    } else {
      incoming_ack(msg);
    }
    break;
  }
  case sstmac::hw::NetworkMessage::rdma_get_payload:
    incoming_payload(msg);
    break;
  case sstmac::hw::NetworkMessage::rdma_get_sent_ack: {
    incoming_ack(msg);
    break;
  }
  default:
    spkt_abort_printf("Invalid message type %s to rendezvous protocol",
                      sstmac::hw::NetworkMessage::tostr(msg->sstmac::hw::NetworkMessage::type()));
  }
}

void
RendezvousGet::incoming_header(MpiMessage* msg)
{
  mpi_queue_protocol_debug("RDMA get incoming header %s", msg->toString().c_str());
  SSTMACBacktrace(MPIRendezvousProtocol_RDMA_Handle_Header);
  MpiQueueRecvRequest* req = queue_->findMatchingRecv(msg);
  if (req) incoming(msg, req);

  queue_->notifyProbes(msg);
}

void
RendezvousGet::incoming(MpiMessage *msg, MpiQueueRecvRequest* req)
{
  mpi_queue_protocol_debug("RDMA get matched payload %s", msg->toString().c_str());
#if SSTMAC_COMM_SYNC_STATS
  //this is a bit of a hack
  msg->setTimeSynced(queue->now());
#endif
  mpi_->pinRdma(msg->payloadBytes());
  mpi_queue_action_debug(
    queue_->api()->commWorld()->rank(),
    "found matching request for %s",
    msg->toString().c_str());

  recv_flows_[msg->flowId()] = req;
  msg->advanceStage();
  mpi_->rdmaGetRequestResponse(msg, msg->payloadSize(), req->recv_buffer_, msg->partnerBuffer(),
                   queue_->pt2ptCqId(), software_ack_ ? sumi::Message::no_ack : queue_->pt2ptCqId());

}

void
RendezvousGet::incoming_payload(MpiMessage* msg)
{
  auto iter = recv_flows_.find(msg->flowId());
  if (iter == recv_flows_.end()){
    spkt_abort_printf("RDMA get protocol has no matching receive for %s", msg->toString().c_str());
  }

  MpiQueueRecvRequest* req = iter->second;
  recv_flows_.erase(iter);

  queue_->finalizeRecv(msg, req);
  if (software_ack_){
    msg->advanceStage();
    mpi_->smsgSendResponse(msg, 64/*more sizeof(...) fixes*/, nullptr,
                             sumi::Message::no_ack, queue_->pt2ptCqId());
  } else {
    delete msg;
  }
}

}
