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
gni_transport::send_pending_smsg()
{
  if (pending_smsg_.empty())
      return;

  gni_debug("trying to send pending");

  std::list<pending_smsg_t*> complete;

  {
  std::list<pending_smsg_t*>::iterator it = pending_smsg_.begin(), end = pending_smsg_.end();
  while (it != end)
  {
      std::list<pending_smsg_t*>::iterator tmp = it++;
      pending_smsg_t* pending = *tmp;
      gni_return_t rc = GNI_SmsgSend(
                          tx_context_.ep_handles[pending->dst],
                          pending->header,
                          pending->header_length,
                          pending->payload,
                          pending->payload_length,
                          0);

      if (rc == GNI_RC_SUCCESS){
          gni_debug("Sending a pending SMSG payload to node %d on node %d \n", pending->dst, rank_);
          complete.push_back(pending);
          pending_smsg_.erase(tmp);
      }
  }
  }

  {
  std::list<pending_smsg_t*>::iterator it = complete.begin(), end = complete.end();
  while (it != end)
  {
    pending_smsg_t* pending = *it;
    delete_smsg_header(pending->header);
    delete pending;
    ++it;
  }
  }
}

void
gni_transport::smsg_send(int dst, const message::ptr &msg, header_type_t type)
{
  gni_debug("sending header of type %d to %d", rank_, type, dst);
  if (dst == rank_){
    gni_debug("sending self smsg %s", rank_, msg->to_string().c_str());
    handle(msg->clone_msg());
    return;
  }
  smsg_payload_header_t* header = new smsg_payload_header_t;
  gni_debug("allocating message buffer");
  char* buffer = allocate_message_buffer(msg, header->length);
  header->type = type;
  header->buffer = buffer;
  gni_debug("smsg send");
  smsg_send(dst, header, sizeof(smsg_payload_header_t), buffer, header->length);
}

void
gni_transport::do_smsg_send(int dst, const message::ptr &msg)
{
  smsg_send(dst, msg, PAYLOAD);
}

#define ALLOW_SMSG_SEND_FAIL false

void
gni_transport::smsg_send(
  int dst,
  void* header,
  uint32_t header_length,
  void* payload,
  uint32_t payload_length
)
{
  send_pending_smsg();

  uint32_t msg_id = allocate_smsg_id();

  gni_debug("completed send header on node %d for header %p msgid %d", rank_, header, msg_id);

  gni_ep_handle_t ep_handle = tx_context_.ep_handles[dst];
  gni_return_t rc = GNI_SmsgSend(
    ep_handle,
    header,
    header_length,
    payload,
    payload_length,
    msg_id);

  if (ALLOW_SMSG_SEND_FAIL && rc == GNI_RC_NOT_DONE){
    pending_smsg_t* pending = new pending_smsg_t;
    pending->dst = dst;
    pending->header = header;
    pending->header_length = header_length;
    pending->payload = payload;
    pending->payload_length = payload_length;
    pending->done = false;
    pending_smsg_.push_back(pending);
  }
  else if (rc == GNI_RC_SUCCESS){
    gni_debug("Sending a SMSG payload to node %d on node %d", dst, rank_);
    smsg_headers_[msg_id] = header;
  }
  else {
    gni_rc_error(rc, "gni smsg send to dst %d, header=%p length=%lu, payload=%p, length=%lu",
                 dst, header, header_length, payload, payload_length);
  }
}

}