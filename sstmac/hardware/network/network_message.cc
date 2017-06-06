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

#include <sstmac/hardware/network/network_message.h>

#define enumcase(x) case x: return #x;

namespace sstmac {
namespace hw {

bool
network_message::is_nic_ack() const
{
  switch(type_)
  {
  case payload_sent_ack:
  case rdma_put_sent_ack:
  case rdma_get_sent_ack:
    return true;
  default:
    return false;
  }
}

void
network_message::convert_to_ack()
{
  reverse();
  switch(type_)
  {
    case rdma_get_payload:
      type_ = rdma_get_sent_ack;
      break;
    case rdma_put_payload:
      type_ = rdma_put_sent_ack;
      break;
    case payload:
      type_ = payload_sent_ack;
      break;
    default:
      spkt_throw_printf(sprockit::value_error,
        "network_message::clone_injection_ack: cannot ack msg type %s",
        tostr(type_));
      break;
  }
}

void
network_message::reverse()
{
  //also flip the addresses
  node_id dst = fromaddr_;
  node_id src = toaddr_;
  toaddr_ = dst;
  fromaddr_ = src;
}

const char*
network_message::tostr(nic_event_t mut)
{
  switch (mut) {
      enumcase(RDMA_GET_REQ_TO_RSP);
      enumcase(RDMA_GET_FAILED);
      enumcase(NVRAM_GET_REQ_TO_RSP);
  }
  spkt_throw_printf(sprockit::value_error,
       "network_message: invalid nic event %d", mut);
}

const char*
network_message::tostr(type_t ty)
{
  switch(ty)
  {
      enumcase(payload);
      enumcase(payload_sent_ack);
      enumcase(rdma_get_request);
      enumcase(rdma_get_payload);
      enumcase(rdma_get_sent_ack);
      enumcase(rdma_get_nack);
      enumcase(rdma_put_payload);
      enumcase(rdma_put_sent_ack);
      enumcase(rdma_put_nack);
      enumcase(null_netmsg_type);
      enumcase(nvram_get_request);
      enumcase(nvram_get_payload);
      enumcase(failure_notification);
  }
  spkt_throw_printf(sprockit::value_error,
    "network_message::tostr: unknown type_t %d",
    ty);
}

void
network_message::serialize_order(serializer& ser)
{
  message::serialize_order(ser);
  ser & needs_ack_;
  ser & toaddr_;
  ser & fromaddr_;
  ser & flow_id_;
  ser & bytes_;
  ser & type_;
  ser & aid_;
}

bool
network_message::is_metadata() const
{
  switch(type_)
  {
    case nvram_get_request:
    case rdma_get_request:
    case rdma_get_sent_ack:
    case rdma_put_sent_ack:
    case payload_sent_ack:
    case rdma_get_nack:
    case rdma_put_nack:
    case failure_notification:
    case null_netmsg_type:
      return true;
    case payload:
    case rdma_get_payload:
    case nvram_get_payload:
    case rdma_put_payload:
      return false;
  }
}

void
network_message::nic_reverse(type_t newtype)
{
  reverse();
  type_ = newtype;
}

long
network_message::byte_length() const
{
  switch (type_)
  {
    case rdma_get_request:
    case rdma_get_sent_ack:
    case rdma_put_sent_ack:
    case payload_sent_ack:
    case rdma_get_nack:
    case rdma_put_nack:
      return 32; //hack for now CHANGE
    default:
      //never return less than 8 bytes - every message has to carry somethng
      return std::max(8L, bytes_);
  }
}

void
network_message::clone_into(network_message* cln) const
{
  cln->needs_ack_ = needs_ack_;
  cln->toaddr_ = toaddr_;
  cln->fromaddr_ = fromaddr_;
  cln->flow_id_ = flow_id_;
  cln->bytes_ = bytes_;
  cln->type_ = type_;
}

void
network_message::put_on_wire()
{
 //do nothing by default
}

}
}