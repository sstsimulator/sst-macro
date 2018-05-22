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

#include <sumi/message.h>


namespace sumi {

const int message::ack_size = 16;
const int message::header_size = 64;

message::~message()
{
  if (owns_remote_buffer_){
    char* buf = (char*) remote_buffer_.ptr;
    delete[] buf;
  }
  if (owns_local_buffer_){
    char* buf = (char*) local_buffer_.ptr;
    delete[] buf;
  }
}

bool
message::is_nic_ack() const
{
  switch(payload_type_)
  {
  case rdma_put_ack:
  case rdma_get_ack:
  case eager_payload_ack:
    return true;
  default:
    return false;
  }
}

message*
message::clone_ack() const
{
  switch (payload_type()){
    case message::eager_payload: {
      message* ack = clone(message::eager_payload_ack);
      ack->set_payload_type(message::eager_payload_ack);
      return ack;
    }
    case message::rdma_get: {
      message* ack = clone(message::rdma_get_ack);
      ack->set_payload_type(message::rdma_get_ack);
      return ack;
    }
    case message::rdma_put: {
      message* ack = clone(message::rdma_put_ack);
      ack->set_payload_type(message::rdma_put_ack);
      return ack;
    }
    default:
      spkt_abort_printf("message::clone_ack: invalid payload type %s for message %s",
        message::tostr(payload_type()), to_string().c_str());
  }
  return nullptr;
}

void
message::reverse()
{
  int tmp = sender_;
  sender_ = recver_;
  recver_ = tmp;
}

message*
message::clone(payload_type_t ty) const
{
  message* cln = new message;
  clone_into(cln);
  return cln;
}

std::string
message::to_string() const
{
  return sprockit::printf("message %s %s %d->%d",
            tostr(payload_type_), tostr(class_),
            sender_, recver_);
}

#define enumcase(x) case x: return #x
const char*
message::tostr(payload_type_t ty)
{
  switch(ty) {
    enumcase(header);
    enumcase(eager_payload);
    enumcase(eager_payload_ack);
    enumcase(rdma_put);
    enumcase(rdma_get);
    enumcase(rdma_put_ack);
    enumcase(rdma_get_ack);
    enumcase(rdma_get_nack);
    enumcase(nvram_get);
    enumcase(failure);
    enumcase(none);
    enumcase(software_ack);
  }
  spkt_throw_printf(sprockit::value_error,
    "message::tostr: invalid payload type %d", ty);
}

const char*
message::tostr(class_t ty)
{
  switch(ty)
  {
    enumcase(pt2pt);
    enumcase(collective);
    enumcase(collective_done);
    enumcase(ping);
    enumcase(bcast);
    enumcase(terminate);
    enumcase(no_class);
    enumcase(fake);
  }
  spkt_throw_printf(sprockit::value_error,
    "message::tostr: invalid message type %d", ty);
}

void
message::clone_into(message* cln) const
{
  cln->payload_type_ = payload_type_;
  cln->owns_remote_buffer_ = owns_remote_buffer_;
  cln->owns_local_buffer_ = owns_local_buffer_;
  cln->class_ = class_;
  cln->sender_ = sender_;
  cln->recver_ = recver_;
  cln->send_cq_ = send_cq_;
  cln->recv_cq_ = recv_cq_;
#if SSTMAC_COMM_SYNC_STATS
  cln->sent_ = sent_;
  cln->header_arrived_ = header_arrived_;
  cln->payload_arrived_ = payload_arrived_;
  cln->synced_ = synced_;
#endif
}

void
message::buffer_send(public_buffer& buf, long num_bytes)
{
  void* new_buf = new char[num_bytes];
  void* old_buf = buf.ptr;
  ::memcpy(new_buf, old_buf, num_bytes);
  buf.ptr = new_buf;
}

void
message::inject_local_to_remote()
{
  if (local_buffer_.ptr){ //might be null
    ::memcpy(remote_buffer_.ptr, local_buffer_.ptr, num_bytes_);
    delete[] (char*) local_buffer_.ptr;
    local_buffer_.ptr = nullptr;
    owns_local_buffer_ = false;
  }
}

void
message::memmove_local_to_remote()
{
  if (local_buffer_.ptr){ //might be null
    ::memcpy(remote_buffer_.ptr, local_buffer_.ptr, num_bytes_);
  }
}

void
message::inject_remote_to_local()
{
  if (remote_buffer_.ptr){
    ::memcpy(local_buffer_.ptr, remote_buffer_.ptr, num_bytes_);
    delete[] (char*) remote_buffer_.ptr;
    remote_buffer_.ptr = nullptr;
    owns_remote_buffer_ = false;
  }
}

void
message::memmove_remote_to_local()
{
  if (remote_buffer_.ptr){
    ::memcpy(local_buffer_.ptr, remote_buffer_.ptr, num_bytes_);
  }
}

void
message::buffer_local()
{
  if (!owns_local_buffer_ && local_buffer_.ptr){
    buffer_send(local_buffer_, num_bytes_);
    owns_local_buffer_ = true;
  }
}

void
message::buffer_remote()
{
  if (!owns_remote_buffer_ && remote_buffer_.ptr){
    buffer_send(remote_buffer_, num_bytes_);
    owns_remote_buffer_ = true;
  }
}
void
message::serialize_buffers(sstmac::serializer& ser)
{
  switch (payload_type_)
  {
    case rdma_get:
      if (remote_buffer_.ptr){
        ser & sstmac::array(remote_buffer_.ptr, num_bytes_);
      }
      break;
    case rdma_put:
    case eager_payload:
      if (local_buffer_.ptr){
        ser & sstmac::array(local_buffer_.ptr, num_bytes_);
      }
      break;
    default:
      break;
  }
}

void
message::serialize_order(sstmac::serializer &ser)
{
#if SSTMAC_COMM_SYNC_STATS
  ser & sent_;
  ser & header_arrived_;
  ser & payload_arrived_;
  ser & synced_;
#endif
  ser & sender_;
  ser & recver_;
  ser & class_;
  ser & payload_type_;
  ser & num_bytes_;
  ser & send_cq_;
  ser & recv_cq_;
  ser & local_buffer_;
  ser & remote_buffer_;
  ser & owns_remote_buffer_;
  ser & owns_local_buffer_;
}

void
system_bcast_message::serialize_order(sstmac::serializer& ser)
{
  message::serialize_order(ser);
  ser & root_;
  ser & action_;
}

}
