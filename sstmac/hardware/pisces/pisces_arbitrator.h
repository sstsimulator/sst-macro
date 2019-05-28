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

#ifndef PACKETFLOW_ARBITRATOR_H
#define PACKETFLOW_ARBITRATOR_H

#include <sstmac/hardware/pisces/pisces.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/common/timestamp.h>
#include <sprockit/factory.h>
#include <sstmac/hardware/noise/noise.h>

namespace sstmac {
namespace hw {

/**
 * @brief The PiscesBandwidthArbitrator class  This arbitrates the available bandwidth
 *        amongst incoming packets. This can either occur on discrete packets or it can
 *        attempt to simulate flits.
 */
class PiscesBandwidthArbitrator
{
 public:
  SST_ELI_DECLARE_BASE(PiscesBandwidthArbitrator)
  SST_ELI_DECLARE_DEFAULT_INFO()
  SST_ELI_DECLARE_CTOR(double)

  struct IncomingPacket
  {
    TimeDelta incoming_byte_delay;
    Timestamp now;
    Timestamp head_leaves;
    Timestamp tail_leaves;
    Timestamp credit_leaves;
    PiscesPacket* pkt;
    int src_outport;
    int dst_inport;
  };

  /**
      Assign bandwidth to payload.
      @return The time at which the packet can be forwarded to the next switch/node/etc.
  */
  virtual void arbitrate(IncomingPacket& st) = 0;

  virtual TimeDelta headTailDelay(PiscesPacket* pkt) = 0;

  virtual std::string toString() const = 0;

  virtual ~PiscesBandwidthArbitrator(){}

  TimeDelta byteDelay() const {
    return byteDelay_;
  }

 protected:
  PiscesBandwidthArbitrator(double bw);

 protected:
  TimeDelta byteDelay_;

};

/**
 * @brief The pisces_null_arbitrator class  The performs no congestion modeling.
 *        This assumes packets can always receive full bandwidth regardless of traffic.
 */
class PiscesNullArbitrator :
  public PiscesBandwidthArbitrator
{
 public:
  SST_ELI_REGISTER_DERIVED(
    PiscesBandwidthArbitrator,
    PiscesNullArbitrator,
    "macro",
    "null",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "Simple bandwidth arbitrator that models zero congestion on a link")

  PiscesNullArbitrator(double bw);

  virtual void arbitrate(IncomingPacket& st) override;

  std::string toString() const override {
    return "pisces null arbitrator";
  }

  TimeDelta headTailDelay(PiscesPacket *pkt) override;

};

/**
 * @brief The pisces_simple_arbitrator class Implements a store-and-forward arbitration scheme.
 * The entire packet has to arrive before it can be arbitrated.  Links/crossbars never interleave
 * flits from different packets in this scheme.  For larger packet sizes, this can lead to
 * unrealistic delays since packets cannot pipeline across network stages.
 */
class PiscesSimpleArbitrator :
  public PiscesBandwidthArbitrator
{
 public:
  SST_ELI_REGISTER_DERIVED(
    PiscesBandwidthArbitrator,
    PiscesSimpleArbitrator,
    "macro",
    "simple",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "Simple bandwidth arbitrator that only ever gives exclusive access to a link."
    "This corresponds to store-and-forward, which can be inaccurate for large packet sizes")

  PiscesSimpleArbitrator(double bw);

  virtual void arbitrate(IncomingPacket& st) override;

  std::string toString() const override {
    return "pisces simple arbitrator";
  }

  TimeDelta headTailDelay(PiscesPacket *pkt) override {
    //no delay
    return TimeDelta();
  }

 protected:
  Timestamp next_free_;

};

/**
 * @brief The pisces_simple_arbitrator class Implements a store-and-forward arbitration scheme.
 * The entire packet has to arrive before it can be arbitrated.  Links/crossbars never interleave
 * flits from different packets in this scheme.  For larger packet sizes, this can lead to
 * unrealistic delays since packets cannot pipeline across network stages.
 */
class PiscesCutThroughArbitrator :
  public PiscesBandwidthArbitrator
{

 public:
  SST_ELI_REGISTER_DERIVED(
    PiscesBandwidthArbitrator,
    PiscesCutThroughArbitrator,
    "macro",
    "cut_through",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "Bandwidth arbitrator that forwards packets as soon as they arrive and enough credits are received"
    "This is a much better approximation to wormhole or virtual cut_through routing")

  PiscesCutThroughArbitrator(double bw);

  ~PiscesCutThroughArbitrator();

  void arbitrate(IncomingPacket& st) override;

  std::string toString() const override {
    return "cut through arbitrator";
  }

  TimeDelta headTailDelay(PiscesPacket *pkt) override;

 private:
  struct Epoch : public sprockit::thread_safe_new<Epoch> {
    Timestamp start;
    uint32_t numCycles;
    Epoch* next;
  };

  Epoch* advance(Epoch* epoch, Epoch* prev);

  void clearOut(Timestamp now);

  Epoch* head_;
  TimeDelta cycleLength_;
  Timestamp lastEpochEnd_;

};

}
}

#endif // PACKETFLOW_ARBITRATOR_H
