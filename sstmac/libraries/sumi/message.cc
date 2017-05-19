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

#include <sstmac/libraries/sumi/message.h>
#include <sstmac/common/serializable.h>
#include <sumi/message.h>
#include <sprockit/util.h>
#include <sprockit/printable.h>
#include <iostream>

namespace sstmac {
  
void
transport_message::serialize_order(serializer& ser)
{
  network_message::serialize_order(ser);
  library_interface::serialize_order(ser);
  sumi::message* msg = payload_.get();
  ser & msg;
  payload_ = msg;
  ser & src_;
  ser & dest_;
  ser & src_app_;
  ser & dest_app_;
}

std::string
transport_message::to_string() const
{
  return sprockit::printf("sumi transport message %lu to node %d from %d:%d to %d:%d carrying %s",
    flow_id(), toaddr_, src_, src_app_, dest_, dest_app_, sprockit::to_string(payload_.get()).c_str());
}

void
transport_message::put_on_wire()
{
  if (!is_metadata()){
    payload_->buffer_send();
  }
}

sstmac::hw::network_message*
transport_message::clone_injection_ack() const
{
#if SSTMAC_SANITY_CHECK
  if (network_message::type_ == network_message::null_netmsg_type){
    spkt_throw(sprockit::value_error,
        "message::clone_injection_ack: null network message type");
  }
#endif
  transport_message* cln = new transport_message;
  clone_into(cln);
#if SSTMAC_SANITY_CHECK
  if (cln->network_message::type() == network_message::null_netmsg_type){
    spkt_throw(sprockit::value_error,
        "message::clone_injection_ack: did not clone correctly");
  }
#endif
  cln->convert_to_ack();
  return cln;
}

void
transport_message::clone_into(transport_message* cln) const
{
  //the payload is actually immutable now - so this is safe
  cln->payload_ = payload_->clone();
  cln->src_app_ = src_app_;
  cln->dest_app_ = dest_app_;
  cln->src_ = src_;
  cln->dest_ = dest_;
  network_message::clone_into(cln);
  library_interface::clone_into(cln);
}

void
transport_message::reverse()
{
  //payload_->reverse();
  network_message::reverse();
  int src = src_;
  int dst = dest_;
  src_ = dst;
  dest_ = src;

  src = src_app_;
  dst = dest_app_;
  src_app_ = dst;
  dest_app_ = src;
}  
  
}