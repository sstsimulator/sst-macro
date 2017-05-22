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

#include <sstmac/hardware/common/packet.h>
#include <sstmac/hardware/common/recv_cq.h>
#include <sstmac/common/messages/sst_message.h>
#include <sprockit/output.h>

namespace sstmac {
namespace hw {

#define DEBUG_CQ 0

void
recv_cq::print()
{
  spkt_unordered_map<uint64_t,incoming_msg>::iterator it, end = bytes_recved_.end();
  coutn << "Completion Queue" << std::endl;
  for (it=bytes_recved_.begin(); it != end; ++it){
    incoming_msg& incoming = it->second;
    coutn << "Message " << it->first << " has "
        << incoming.bytes_arrived << " bytes arrived "
        << " out of " << incoming.bytes_total << "\n";
  }
}

message*
recv_cq::recv(uint64_t unique_id, int bytes, message* orig)
{
  incoming_msg& incoming  = bytes_recved_[unique_id];
#if SSTMAC_SANITY_CHECK
  if (incoming.msg && orig){
    spkt_abort_printf(
        "recv_cq::recv: only one message chunk should carry the parent payload for %lu: %s",
        unique_id, incoming.msg->to_string().c_str());
  }
#endif
  if (orig){
    //this guy is actually carrying the payload
    incoming.msg = orig;
    incoming.bytes_total = orig->byte_length();
  }
  incoming.bytes_arrived += bytes;

#if SSTMAC_SANITY_CHECK
  if (incoming.msg && (incoming.bytes_arrived > incoming.bytes_total)){
    spkt_throw(sprockit::illformed_error,
        "recv_cq::recv: have too many bytes in queue "
        "for parent message ", incoming.msg->to_string(),
        " and packet ", incoming.msg->to_string());
  }
#endif
  if (incoming.bytes_arrived == incoming.bytes_total){
    message* ret = incoming.msg;
    bytes_recved_.erase(unique_id);
    return ret;
  }
  else {
    return NULL;
  }
}

message*
recv_cq::recv(packet* pkt)
{
  message* flow = dynamic_cast<message*>(pkt->orig());
  return recv(pkt->flow_id(), pkt->byte_length(), flow);
}

}
}