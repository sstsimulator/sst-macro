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

#ifndef PACKETFLOW_H
#define PACKETFLOW_H

#include <sstmac/hardware/common/packet.h>
#include <sstmac/hardware/common/flow.h>
#include <sprockit/thread_safe_new.h>
#include <sprockit/factory.h>
#include <sprockit/debug.h>

DeclareDebugSlot(pisces)
DeclareDebugSlot(pisces_queue)
DeclareDebugSlot(pisces_config)

namespace sstmac {
namespace hw {

/**
 @class pisces
 Encapsulates a group of machine packets traveling together on the
 same path between endpoints.  This is usually one fraction of
 a larger message.
 */
class PiscesPacket :
  public Packet,
  public sprockit::thread_safe_new<PiscesPacket>
{
 public:
  ImplementSerializable(PiscesPacket)

  static const double uninitialized_bw;

 public:
  PiscesPacket(
    serializable* msg,
    uint32_t numBytes,
    uint64_t flowId,
    bool isTail,
    NodeId fromaddr,
    NodeId toaddr);

  std::string toString() const override;

  virtual ~PiscesPacket() {}

  /**
    Needed because of routable_message ambiguity
  */
  int vc() const {
    return current_vc_;
  }

  /**
   * @brief reset_stages Configure the internal ports to traverse on a switch
   * @param port0
   */
  void resetStages(uint8_t port0){
    stage_ = 0;
    outports_[0] = port0;
  }

  void resetStages(uint8_t port0, uint8_t port1){
    resetStages(port0);
    outports_[1] = port1;
  }

  void resetStages(uint8_t port0, uint8_t port1, uint8_t port2){
    resetStages(port0, port1);
    outports_[2] = port2;
  }

  void advanceStage(){
    inport_ = outports_[stage_];
    ++stage_;
  }

  uint8_t stage() const {
    return stage_;
  }

  int nextLocalOutport() const {
    return outports_[stage_];
  }

  int nextLocalInport() const {
    return inport_;
  }

  int nextVC() const {
    return rtrHeader<Packet::Header>()->deadlock_vc;
  }

  void updateVC() {
    current_vc_ = nextVC();
  }

  void setInport(int port) {
    inport_ = port;
  }

  Timestamp arrival() const {
    return arrival_;
  }

  void setArrival(Timestamp time) {
    arrival_ = time;
  }

  void initByteDelay(TimeDelta delay){
    if (byte_delay_.ticks() == 0){
      byte_delay_ = delay;
    }
  }

  TimeDelta byteDelay() const {
    return byte_delay_;
  }

  void setByteDelay(TimeDelta delay){
    byte_delay_ = delay;
  }

  void setMinByteDelay(TimeDelta delay){
    initByteDelay(delay);
    byte_delay_ = std::max(delay, byte_delay_);
  }

  void serialize_order(serializer& ser) override;

 private:
  PiscesPacket(){} //for serialization

  TimeDelta byte_delay_;

  Timestamp arrival_;

  int current_vc_;

  uint8_t stage_;

  uint8_t outports_[3];

  uint16_t inport_;



};

class PiscesCredit :
  public Event,
  public sprockit::printable,
  public sprockit::thread_safe_new<PiscesCredit>
{

 public:
  ImplementSerializable(PiscesCredit)

 public:
  PiscesCredit(){} //for serialization

  PiscesCredit(
    int port,
    int vc,
    int num_credits)
    : port_(port),
      num_credits_(num_credits),
      vc_(vc)
  {
  }

  int vc() const {
    return vc_;
  }

  int port() const {
    return port_;
  }

  int numCredits() const {
    return num_credits_;
  }

  std::string toString() const override;

  void serialize_order(serializer& ser) override;

 protected:
  int num_credits_;
  int port_;
  int vc_;


};

}
}


#endif // PACKETFLOW_H
