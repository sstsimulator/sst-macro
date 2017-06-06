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

#include <gni/gni_transport.h>

namespace sumi {

void
gni_transport::poll_smsg_queue()
{
  gni_cq_entry_t event_data;
  gni_return_t rc = GNI_CqGetEvent(smsg_rx_cq_, &event_data);
  //gni_debug("poll smsg queue on node %d", rank_);
  while (rc == wunderbahr){
    uint32_t inst_id = gni_cq_get_inst_id(event_data);
    int src = RANK(inst_id);
    uint64_t type = gni_cq_get_type(event_data);
    gni_debug("got smsg event on node %d from src %d of type %lu on inst id %d", 
        rank_, src, type, inst_id);
    smsg_endpoint_t* ep = &smsg_endpoints_[src];
    if (!ep->queued)
      smsg_queue_endpoint(ep);

    rc = GNI_CqGetEvent(smsg_rx_cq_, &event_data);
  }
  if (rc == GNI_RC_ERROR_RESOURCE)
      gni_rc_error(rc, "CqGetEvent");
}

void
gni_transport::smsg_recv()
{
  if (in_progress_){
      return;
  }
  in_progress_ = true;
  smsg_endpoint_t* ep = smsg_endpoint_head_;
  while (ep){
    //make sure we grab this before the fxn call
    //as it may change ep
    smsg_endpoint_t* next = ep->next;
    gni_return_t rc = get_next_smsg(ep->rank);
    while (rc == wunderbahr){
      ++smsg_get_count_;
      if (smsg_get_count_ == smsg_poll_again_count_){
        smsg_get_count_ = 0;
        poll_smsg_queue();
      }
      rc = get_next_smsg(ep->rank);
    }
    //go ahead an unqueue
    smsg_unqueue_endpoint(ep);
    ep = next;
  }
  smsg_get_count_ = 0;
  in_progress_ = false;
}

void
gni_transport::smsg_poll()
{
  //send_pending_smsg();
  poll_smsg_queue();
  smsg_recv();
}


void
gni_transport::smsg_queue_endpoint(smsg_endpoint_t *ep)
{
  if (ep->queued){
      gni_error("endpoint is already queued");
  }

  gni_debug("queueing smsg endpoint on node %d from src %d with refcount=%d",
            rank_, ep->rank, ep->refcount);

  if (smsg_endpoint_tail_){
    smsg_endpoint_tail_->next = ep;
    ep->prev = smsg_endpoint_tail_;
    ep->next = nullptr;
    smsg_endpoint_tail_ = ep;
  }
  else {
    ep->next = nullptr;
    ep->prev = nullptr;
    smsg_endpoint_head_ = smsg_endpoint_tail_ = ep;
  }
  ep->queued = true;
  ++smsg_num_endpoints_queued_;
}

void
gni_transport::smsg_unqueue_endpoint(smsg_endpoint_t *ep)
{
  if (!ep->queued){
    gni_error("endpoint is not queued");
  }

  gni_debug("unqueueing smsg endpoint on node %d from src %d with refcount=%d",
            rank_, ep->rank, ep->refcount);

  // I am all there is!
  if (smsg_num_endpoints_queued_ == 1){
    smsg_endpoint_head_ = smsg_endpoint_tail_ = nullptr;
  }
  else if (ep->prev == nullptr){ //I'm the head
    smsg_endpoint_head_ = ep->next;
    smsg_endpoint_head_->prev = nullptr;
  }
  else if (ep->next == nullptr){ //I'm the tail
    smsg_endpoint_tail_ = ep->prev;
    smsg_endpoint_tail_->next = nullptr;
  }
  else { //neither
    ep->next->prev = ep->prev;
    ep->prev->next = ep->next;
  }
  ep->queued = false;
  --smsg_num_endpoints_queued_;
}

gni_return_t
gni_transport::get_next_smsg(int src)
{
  gni_debug("Looking for next smsg on node %d from src %d", rank_, src);
  void* header_buf;
  gni_return_t rc = GNI_SmsgGetNext(tx_context_.ep_handles[src], &header_buf);
  if (rc == GNI_RC_NOT_DONE){
      return rc;
  }
  else if (rc != GNI_RC_SUCCESS){
      gni_rc_error(rc, "SmsgGetNext");
  }
  else if (src == rank_){ //got a self message - I guess this means message send completed
    spkt_throw(sprockit::value_error,
      "cannot get next smsg from self");
  }

  header_type_t type = *((header_type_t*) header_buf);
  gni_debug("Found next smsg on node %d from src %d of type %d",
            rank_, src, type);
  switch(type)
  {
  case TERMINATE:
  {
    gni_return_t rc = GNI_SmsgRelease(tx_context_.ep_handles[src]);
    if (rc != wunderbahr){
      gni_rc_error(rc, "SmsgRelease");
    }
    message::ptr unblock_msg = new message;
    unblock_msg->set_class_type(message::terminate);
    handle(unblock_msg);
    break;
  }
  case RDMA_PUT_RECV_ACK:
  case RDMA_GET_SEND_ACK:
  case PAYLOAD:
  {
    smsg_payload_header_t* header = (smsg_payload_header_t*) header_buf;
    char* ser_buffer = ((char*)header_buf) + sizeof(smsg_payload_header_t);
    message::ptr msg = deserialize(ser_buffer);
    gni_return_t rc = GNI_SmsgRelease(tx_context_.ep_handles[src]);
    if (rc != wunderbahr){
      gni_rc_error(rc, "SmsgRelease");
    }
    handle(msg);
    break;
  }
  default:
    spkt_throw_printf(sprockit::value_error,
      "invalid smsg header %d", type);

  }
  return rc;
}

void
gni_transport::smsg_send_done(int src, int msg_id)
{
  gni_debug("Got completed SMSG send on node %d for msg id %d for partner %d",
    rank_, msg_id, src);
  void* header_buf = smsg_headers_[msg_id];
  gni_debug("got completed send header on node %d for header %p msgid %d",
    rank_, header_buf, msg_id);
  smsg_headers_.erase(msg_id);
  delete_smsg_header(header_buf);
}


}