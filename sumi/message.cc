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
}

std::string
message::to_string() const
{
  return sprockit::printf("message %s %d->%d",
            tostr(class_), sender_, recver_);
}

#define enumcase(x) case x: return #x

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

#if 0
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
  cln->num_bytes_ = num_bytes_;
#if SSTMAC_COMM_SYNC_STATS
  cln->sent_ = sent_;
  cln->header_arrived_ = header_arrived_;
  cln->payload_arrived_ = payload_arrived_;
  cln->synced_ = synced_;
#endif
}
#endif

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
  ser & send_cq_;
  ser & recv_cq_;
  network_message::serialize_order(ser);
}

/**
void
transport_message::clone_into(transport_message* cln) const
{
  //the payload is actually immutable now - so this is safe
  cln->payload_ = payload_;
  cln->src_app_ = src_app_;
  cln->dest_app_ = dest_app_;
  cln->src_ = src_;
  cln->dest_ = dest_;
  network_message::clone_into(cln);
  library_interface::clone_into(cln);
}

void
system_bcast_message::serialize_order(sstmac::serializer& ser)
{
  message::serialize_order(ser);
  ser & root_;
  ser & action_;
}
*/

}
