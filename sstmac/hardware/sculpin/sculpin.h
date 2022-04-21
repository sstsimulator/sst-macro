/**
Copyright 2009-2022 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2022, NTESS

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

#ifndef sculpin_packet_h
#define sculpin_packet_h

#include <sstmac/hardware/common/packet.h>
#include <sstmac/hardware/common/flow.h>
#include <sprockit/thread_safe_new.h>
#include <sprockit/factory.h>
#include <sprockit/debug.h>


DeclareDebugSlot(sculpin)

namespace sstmac {
namespace hw {

/**
 @class pisces
 Encapsulates a group of machine packets traveling together on the
 same path between endpoints.  This is usually one fraction of
 a larger message.
 */
class SculpinPacket :
  public Packet,
  public sprockit::thread_safe_new<SculpinPacket>
{
  ImplementSerializable(SculpinPacket)

 public:
  SculpinPacket(
    Flow* msg,
    uint32_t numBytes,
    bool isTail,
    uint64_t flowId,
    NodeId toaddr,
    NodeId fromaddr);

  SculpinPacket(){} //for serialization

  std::string toString() const override;

  ~SculpinPacket() override {}

  int nextPort() const {
    return rtrHeader<Header>()->edge_port;
  }

  Timestamp arrival() const {
    return arrival_;
  }

  void setArrival(Timestamp time) {
    arrival_ = time;
  }

  TimeDelta timeToSend() const {
    return time_to_send_;
  }

  void setTimeToSend(TimeDelta time) {
    time_to_send_ = time;
  }

  int priority() const {
    return priority_;
  }

  void setPriority(int p) {
    priority_ = p;
  }

  uint32_t seqnum() const {
    return seqnum_;
  }

  void setSeqnum(uint32_t s){
    seqnum_ = s;
  }

  void serialize_order(serializer& ser) override;

 private:
  uint32_t seqnum_;

  Timestamp arrival_;

  TimeDelta time_to_send_;

  int priority_;


};

}
}


#endif // PACKETFLOW_H
