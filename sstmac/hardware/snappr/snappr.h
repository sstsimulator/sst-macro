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
#include <sstmac/hardware/common/flow.h>
#include <sprockit/thread_safe_new.h>
#include <sprockit/factory.h>
#include <sprockit/debug.h>


DeclareDebugSlot(snappr)

namespace sstmac {
namespace hw {

/**
 @class SnapprPacket
 */
class SnapprPacket :
  public Packet,
  public sprockit::thread_safe_new<SnapprPacket>
{
  ImplementSerializable(SnapprPacket)

 public:
  SnapprPacket(
    Flow* msg,
    uint32_t numBytes,
    uint64_t offset,
    bool isTail,
    uint64_t flowId,
    NodeId toaddr,
    NodeId fromaddr,
    int qos = 0);

  SnapprPacket(){} //for serialization

  std::string toString() const override;

  virtual ~SnapprPacket() {}

  int nextPort() const {
    return rtrHeader<Header>()->edge_port;
  }

  Timestamp arrival() const {
    return arrival_;
  }

  void setArrival(Timestamp time) {
    arrival_ = time;
  }

  void setVirtualLane(int vl){
    vl_ = vl;
  }

  void saveInputVirtualLane(){
    input_vl_ = vl_;
  }

  int inputVirtualLane() const {
    return input_vl_;
  }

  int virtualLane() const {
    return vl_;
  }

  int qos() const {
    return qos_;
  }

  TimeDelta timeToSend() const {
    return time_to_send_;
  }

  void setTimeToSend(TimeDelta time) {
    time_to_send_ = time;
  }

  TimeDelta congestionDelay() const {
    return congestion_delay_;
  }

  void accumulateCongestionDelay(TimeDelta delay){
    congestion_delay_ += delay;
  }

  void accumulateCongestionDelay(Timestamp departure){
    congestion_delay_ += (departure - arrival_);
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

  uint32_t inport() const {
    return inport_;
  }

  void setInport(uint32_t port){
    inport_ = port;
  }

  void serialize_order(serializer& ser) override;

 private:
  uint32_t seqnum_;

  uint64_t offset_;

  Timestamp arrival_;

  TimeDelta time_to_send_;

  TimeDelta congestion_delay_;

  int qos_;

  int vl_;

  int priority_;

  int inport_; //used for sending credits

  int input_vl_;

};

/**
 @class SnapprCredit
 */
class SnapprCredit :
  public Event,
  public sprockit::thread_safe_new<SnapprCredit>
{
  ImplementSerializable(SnapprCredit)

 public:
  SnapprCredit(uint32_t num_bytes, int vl, int port) :
    num_bytes_(num_bytes),
    port_(port),
    vl_(vl)
  {
  }

  int virtualLane() const {
    return vl_;
  }

  std::string toString() const;

  uint32_t numBytes() const {
    return num_bytes_;
  }

  int port() const {
    return port_;
  }

  SnapprCredit(){} //for serialization

  virtual ~SnapprCredit() {}

  void serialize_order(serializer& ser) override;

 private:
  uint32_t num_bytes_;
  int vl_;
  int port_;


};


}
}


#endif // PACKETFLOW_H
