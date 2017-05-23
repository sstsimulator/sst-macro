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

#include <sstmac/hardware/pisces/pisces.h>
#include <sstmac/hardware/router/routable.h>

RegisterDebugSlot(pisces,
    "print all the details of the pisces model including crossbar arbitration"
    ", buffer occupancies, and queue depths. This can be a LOT of information. User beware")

RegisterDebugSlot(pisces_queue,
    "print all the details of queue lengths in the packet flow model");

RegisterDebugSlot(pisces_config,
    "print all the details of the initial configuration of packet flow connections/credits/buffers/vcs");

namespace sstmac {
namespace hw {

const double pisces_payload::uninitialized_bw = -1;

pisces_payload::pisces_payload(
  serializable* msg,
  int num_bytes,
  bool is_tail) :
  packet(msg, num_bytes, is_tail),
  bw_(uninitialized_bw),
  max_in_bw_(1.0)
{
}

void
pisces_payload::serialize_order(serializer& ser)
{
  //routable::serialize_order(ser);
  packet::serialize_order(ser);
  ser & inport_;
  ser & bw_;
  ser & max_in_bw_;
  ser & arrival_;
  ser & vc_;
}

void
pisces_routable_packet::serialize_order(serializer& ser)
{
  pisces_payload::serialize_order(ser);
  routable::serialize_order(ser);
}

void
pisces_default_packet::serialize_order(serializer& ser)
{
  pisces_routable_packet::serialize_order(ser);
  ser & flow_id_;
}

void
pisces_delay_stats_packet::serialize_order(serializer& ser)
{
  pisces_default_packet::serialize_order(ser);
  ser & congestion_delay_;
}

std::string
pisces_default_packet::to_string() const
{
  return sprockit::printf("flow %16lu%s, %d bytes bw=%8.4e %d->%d",
                   uint64_t(flow_id()),
                   is_tail_ ? " tail" : "",
                   num_bytes_, bw_,
                   int(fromaddr()), int(toaddr()), bw_);
}

std::string
pisces_credit::to_string() const
{
  return sprockit::printf("credits n=%d port=%d vc=%d",
                          num_credits_, port_, vc_);
}

void
pisces_credit::serialize_order(serializer& ser)
{
  event::serialize_order(ser);
  ser & num_credits_;
  ser & port_;
  ser & vc_;
}

}
}