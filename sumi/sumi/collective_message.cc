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

#include <sumi/collective_message.h>
#include <sumi/serialization.h>

namespace sumi {

#define enumcase(x) case x: return #x;
const char*
collective_work_message::tostr(action_t action)
{
  switch(action)
  {
    enumcase(eager_payload);
    enumcase(nack_eager);
    enumcase(nack_get_header);
    enumcase(nack_put_header);
    enumcase(nack_put_payload);
    enumcase(nack_get_ack);
    enumcase(rdma_get_header);
    enumcase(rdma_put_header);
    enumcase(get_data);
    enumcase(put_data);
  }
  spkt_throw_printf(sprockit::value_error,
    "collective_work_message::invalid action %d",
    action);
}

void
collective_work_message::serialize_order(sumi::serializer &ser)
{
  message::serialize_order(ser);
  ser & action_;
  ser & tag_;
  ser & type_;
  ser & round_;
  ser & dense_sender_;
  ser & dense_recver_;
  //ser & failed_procs_;
}

std::string
collective_work_message::to_string() const
{
  return sprockit::printf(
    "message %p for collective %s event %s "
    "recver=%d(%d) sender=%d(%d) nbytes=%d round=%d tag=%d",
        this, collective::tostr(type_), message::tostr(message::payload_type()),
        dense_recver_, recver(), dense_sender_, sender(), byte_length(), round_, tag_);
}

void
collective_work_message::append_failed(const thread_safe_set<int>& failed)
{
  thread_safe_set<int>::iterator end = failed.start_iteration();
  failed_procs_.insert(failed.begin(), end);
  failed.end_iteration();
}

void
collective_work_message::clone_into(collective_work_message* cln) const
{
  message::clone_into(cln);
  cln->tag_ = tag_;
  cln->type_ = type_;
  cln->round_ = round_;
  cln->dense_sender_ = dense_sender_;
  cln->dense_recver_ = dense_recver_;
}

message*
collective_done_message::clone() const
{
  spkt_throw(sprockit::unimplemented_error,
    "collective_done_message::clone");
  return 0;
}

void
collective_work_message::reverse()
{
  message::reverse();
  int tmp = dense_recver_;
  dense_recver_ = dense_sender_;
  dense_sender_ = tmp;
}

}