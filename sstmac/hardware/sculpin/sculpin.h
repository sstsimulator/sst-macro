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

#ifndef sculpin_packet_h
#define sculpin_packet_h

#include <sstmac/hardware/common/packet.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/hardware/router/routing_enum.h>
#include <sprockit/thread_safe_new.h>
#include <sprockit/factories/factory.h>
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
class sculpin_packet :
  public packet,
  public sprockit::thread_safe_new<sculpin_packet>
{
  ImplementSerializable(sculpin_packet)

 public:
  sculpin_packet(
    message* msg,
    uint32_t num_bytes,
    bool is_tail,
    uint64_t flow_id,
    node_id toaddr,
    node_id fromaddr);

  sculpin_packet(){} //for serialization

  std::string to_string() const override;

  virtual ~sculpin_packet() {}

  int next_port() const {
    return global_outport();
  }

  timestamp arrival() const {
    return arrival_;
  }

  void set_arrival(timestamp time) {
    arrival_ = time;
  }

  timestamp time_to_send() const {
    return time_to_send_;
  }

  void set_time_to_send(timestamp time) {
    time_to_send_ = time;
  }

  int priority() const {
    return priority_;
  }

  void set_priority(int p) {
    priority_ = p;
  }

  uint32_t seqnum() const {
    return seqnum_;
  }

  void set_seqnum(uint32_t s){
    seqnum_ = s;
  }

  void serialize_order(serializer& ser) override;

 private:
  uint32_t seqnum_;

  timestamp arrival_;

  timestamp time_to_send_;

  int priority_;


};

}
}


#endif // PACKETFLOW_H
