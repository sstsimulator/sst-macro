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

#include <sumi-mpi/mpi_message.h>
#include <sumi-mpi/mpi_status.h>
#include <sumi-mpi/mpi_protocol/mpi_protocol.h>
#include <sprockit/debug.h>
#include <sprockit/errors.h>

#include <stdlib.h>
#include <sstream>

#define enumcase(x) case x: return #x

namespace sumi {


mpi_message::mpi_message(int src, int dst, int count,
   MPI_Datatype type, int type_packed_size,
   int tag,
   MPI_Comm commid, int seqnum, mpi_message::id msgid,
   mpi_protocol* protocol) :
  src_rank_(src),
  dst_rank_(dst),
  count_(count),
  type_(type), type_packed_size_(type_packed_size),
  tag_(tag), commid_(commid),
  seqnum_(seqnum), msgid_(msgid),
  content_type_(null_content),
  in_flight_(false),
  already_buffered_(false),
  protocol_(protocol->get_prot_id())
{
}

void
mpi_message::recompute_bytes()
{
  switch (content_type_)
  {
  case eager_payload:
  case data: {
    num_bytes_ = count_ * type_packed_size_;
    break;
  }
  case header:
  case completion_ack:
  case fake:
    num_bytes_ = 32; //just hard code this for now
    break;
  default:
    spkt_throw_printf(sprockit::value_error,
        "mpi_message::recompute_bytes: invalid type %s",
        str(content_type_));
  }
}

sumi::message*
mpi_message::clone() const
{
  mpi_message* cln = new mpi_message;
  clone_into(cln);
  return cln;
}

void
mpi_message::serialize_order(serializer& ser)
{
  message::serialize_order(ser);
  ser & (src_rank_);
  ser & (dst_rank_);
  ser & (count_);
  ser & type_;
  ser & type_packed_size_;
  ser & (tag_);
  ser & (commid_);
  ser & (seqnum_);
  ser & (msgid_);
  ser & (content_type_);
  ser & (in_flight_);
  ser & (protocol_);
  ser & (already_buffered_);
}

void
mpi_message::clone_into(mpi_message* cln) const
{
  sumi::message::clone_into(cln);
  cln->count_ = count_;
  cln->type_ = type_;
  cln->tag_ = tag_;
  cln->commid_ = commid_;
  cln->seqnum_ = seqnum_;
  cln->msgid_ = msgid_;
  cln->src_rank_ = src_rank_;
  cln->dst_rank_ = dst_rank_;
  cln->content_type_ = content_type_;
  cln->protocol_ = protocol_;
  cln->in_flight_ = in_flight_;
  cln->type_packed_size_ = type_packed_size_;
  cln->already_buffered_ = already_buffered_;
}

void
mpi_message::buffer_send()
{
  if (!already_buffered_){
    message::buffer_send();
  }
}

void
mpi_message::move_remote_to_local()
{
  if (protocol_ == mpi_protocol::EAGER1_DOUBLECPY){
    //do nothing - we do not have the remote buffer yet
  } else {
    message::move_remote_to_local();
  }
}

void
mpi_message::move_local_to_remote()
{
  message::move_local_to_remote();
}

//
// Goodbye.
//
mpi_message::~mpi_message() throw ()
{
}

//
// Create a status object.
//
void
mpi_message::build_status(MPI_Status* stat) const
{
  stat->MPI_SOURCE = src_rank_;
  stat->MPI_TAG = tag_;
  stat->count = count_;
  stat->bytes_received = count_ * type_packed_size_;
}

const char*
mpi_message::str(content_type_t content_type)
{
  switch (content_type) {
      enumcase(null_content);
      enumcase(data);
      enumcase(eager_payload);
      enumcase(header);
      enumcase(completion_ack);
      enumcase(fake);
  }
  spkt_throw_printf(sprockit::value_error,
      "unknown mpi content type %d", content_type);
}

std::string
mpi_message::to_string() const
{
  std::stringstream ss;
  ss << "mpimessage("
     << (void*) local_buffer_.ptr
     << "," << (void*) remote_buffer_.ptr
     << ", count=" << count_
     << ", type=" << type_
     << ", src=" << src_rank_
     << ", dst=" << dst_rank_
     << ", tag=" << tag_
     << ", commid" << commid_;

  if (in_flight_) {
    ss << ", seq=(ignored)" << seqnum_;
  }
  else {
    ss << ", seq=" << seqnum_;
  }
  ss   << ", content=" << str(content_type_)
     << ", protocol=" << protocol()->to_string();

  return ss.str();
}

void
mpi_message::payload_to_completion_ack()
{
  reverse();
  content_type_ = mpi_message::completion_ack;
  recompute_bytes();
}

mpi_protocol*
mpi_message::protocol() const
{
  return mpi_protocol::get_protocol_object((mpi_protocol::PROTOCOL_ID)protocol_);
}

void
mpi_message::set_protocol(mpi_protocol* protocol)
{
  protocol_ = protocol->get_prot_id();
}


}