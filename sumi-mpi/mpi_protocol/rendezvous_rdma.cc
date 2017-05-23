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
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_debug.h>
#include <sstmac/software/process/backtrace.h>
#include <sprockit/sim_parameters.h>

namespace sumi {

rendezvous_protocol::rendezvous_protocol(sprockit::sim_parameters* params)
{
  rdma_pin_delay_ = params->get_optional_time_param("rdma_pin_delay", 0);
  software_ack_ = params->get_optional_bool_param("software_ack", true);
}

rendezvous_get::~rendezvous_get()
{
}

void
rendezvous_get::configure_send_buffer(mpi_queue* queue, const mpi_message::ptr& msg,
                                      void *buffer, mpi_type* type)
{
  if (rdma_pin_delay_.ticks_int64()){
    queue->api()->compute(rdma_pin_delay_); 
  }
  if (buffer){
    if (type->contiguous()){
      msg->remote_buffer().ptr = buffer;
    } else {
      void* eager_buf = fill_send_buffer(msg, buffer, type);
      msg->remote_buffer().ptr = eager_buf;
    }
  }
}

void
rendezvous_get::send_header(mpi_queue* queue,
                            const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: RDMA Send Header");
  msg->set_content_type(mpi_message::header);
  queue->post_header(msg, sumi::message::header, false); //don't need the nic ack
}

void
rendezvous_get::incoming_header(mpi_queue* queue,
                               const mpi_message::ptr& msg)
{
  mpi_queue_recv_request* req = queue->pop_pending_request(msg);
  incoming_header(queue, msg, req);
}

void
rendezvous_get::incoming_header(mpi_queue *queue,
                                const mpi_message::ptr &msg,
                                mpi_queue_recv_request *req)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: RDMA Handle Header");
  if (req) {
#if SSTMAC_COMM_SYNC_STATS
    //this is a bit of a hack
    msg->set_time_synced(queue->now());
#endif
    mpi_queue_action_debug(
      queue->api()->comm_world()->rank(),
      "found matching request for %s",
      msg->to_string().c_str());
    msg->set_needs_send_ack(false); //TODO do I need this?
    msg->set_content_type(mpi_message::data);
    msg->local_buffer().ptr = req->recv_buffer_;
    queue->recv_needs_payload_[msg->unique_int()] = req;
    //generate both a send and recv ack
    //but the send ack might be hardware or software level
    bool hardware_send_ack = !software_ack_;
    queue->post_rdma(msg, hardware_send_ack, true);
  } else {
    mpi_queue_action_debug(
      queue->api()->comm_world()->rank(),
      "no matching requests for %s",
      msg->to_string().c_str());
  }
  queue->notify_probes(msg);
}

void
rendezvous_get::incoming_payload(mpi_queue* queue,
                                const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: RDMA Handle Payload");
  if (rdma_pin_delay_.ticks_int64()){
    queue->api()->compute(rdma_pin_delay_);
  }

  mpi_queue::pending_req_map::iterator it = queue->recv_needs_payload_.find(
        msg->unique_int());
  if (it == queue->recv_needs_payload_.end()) {
    if (queue->recv_needs_payload_.empty()){
      std::cerr << "No recv requests waiting" << std::endl;
    }
    for (auto& p : queue->recv_needs_payload_){
      mpi_message::id id = p.first;
      mpi_queue_recv_request* req = p.second;
      std::cerr << sprockit::printf("Waiting request: count=%d tag=%s comm=%s source=%s",
                    req->count_, 
                    queue->api()->tag_str(req->tag_).c_str(),
                    queue->api()->comm_str(req->comm_).c_str(),
                    queue->api()->src_str(req->source_).c_str()) 
                << std::endl;
    }
    int rank; queue->api_->comm_rank(MPI_COMM_WORLD, &rank);
    spkt_throw_printf(sprockit::illformed_error,
     "mpi_queue[%d]: rendezvous_get::handle_payload: "
     "queue %p data message %lu without a matching ack on %s",
      rank, queue, msg->unique_int(), msg->to_string().c_str());
  }
  mpi_queue_recv_request* recver = it->second;
  queue->recv_needs_payload_.erase(it);
  queue->finalize_recv(msg, recver);
  if (software_ack_){
    queue->send_completion_ack(msg);
  }
}

}