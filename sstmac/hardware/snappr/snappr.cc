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

#ifndef __STDC_FORMAT_MACROS
#define __STDC_FORMAT_MACROS
#endif

#include <inttypes.h>
#include <queue>

#include <sstmac/hardware/snappr/snappr.h>

RegisterDebugSlot(snappr, "print all the details of the snappr model")


namespace sstmac {
namespace hw {

SnapprPacket::SnapprPacket(
  Flow* msg,
  uint32_t num_bytes,
  bool is_tail,
  uint64_t flow_id,
  uint64_t offset,
  NodeId toaddr,
  NodeId fromaddr,
  int qos) :
  Packet(msg, num_bytes, flow_id, is_tail, fromaddr, toaddr, qos),
  offset_(offset),
  priority_(0),
  inport_(-1),
  deadlocked_(false)
{
}

std::string
SnapprPacket::toString() const
{
  return sprockit::sprintf("pkt bytes=%" PRIu32 " flow=%" PRIu64 " offset=%" PRIu64 ": %s",
                          numBytes(), flowId(), offset_,
                          (flow() ? flow()->toString().c_str() : "no payload"));

}

void
SnapprPacket::serialize_order(serializer& ser)
{
  //routable::serialize_order(ser);
  Packet::serialize_order(ser);
  ser & arrival_;
  ser & time_to_send_;
  ser & priority_;
  ser & inport_;
  ser & vl_;
  ser & input_vl_;
}

std::string
SnapprCredit::toString() const {
  return sprockit::sprintf("credit bytes=%" PRIu32 " port=%d", num_bytes_, port_);
}

void
SnapprCredit::serialize_order(serializer &ser)
{
  Event::serialize_order(ser);
  ser & port_;
  ser & num_bytes_;
  ser & vl_;
}

}
}
