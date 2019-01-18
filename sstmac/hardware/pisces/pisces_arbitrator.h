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
#include <sprockit/factories/factory.h>
#include <sstmac/hardware/noise/noise.h>
#include <sstmac/hardware/pisces/pisces_stats.h>

namespace sstmac {
namespace hw {

/**
 * @brief The PiscesBandwidthArbitrator class  This arbitrates the available bandwidth
 *        amongst incoming packets. This can either occur on discrete packets or it can
 *        attempt to simulate flits.
 */
class PiscesBandwidthArbitrator
{
  DeclareFactory(PiscesBandwidthArbitrator)
 public:
  /**
      Assign bandwidth to payload.
      @return The time at which the packet can be forwarded to the next switch/node/etc.
  */
  virtual void arbitrate(pkt_arbitration_t& st) = 0;

  virtual Timestamp headTailDelay(PiscesPacket* pkt) = 0;

  virtual std::string toString() const = 0;

  virtual ~PiscesBandwidthArbitrator(){}

  Timestamp byteDelay() const {
    return byteDelay_;
  }

  static inline Timestamp
  creditDelay(Timestamp minByteDelay, Timestamp actualByteDelay, uint32_t bytes){
    Timestamp credit_delta = bytes * (actualByteDelay - minByteDelay);
    return std::max(Timestamp(), credit_delta);
  }

  virtual uint32_t bytesSending(GlobalTimestamp now) const = 0;

 protected:
  PiscesBandwidthArbitrator(SST::Params& params);

 protected:
  Timestamp byteDelay_;

};

/**
 * @brief The pisces_null_arbitrator class  The performs no congestion modeling.
 *        This assumes packets can always receive full bandwidth regardless of traffic.
 */
class PiscesNullArbitrator :
  public PiscesBandwidthArbitrator
{
  FactoryRegister("null", PiscesBandwidthArbitrator, PiscesNullArbitrator,
              "Simple bandwidth arbitrator that models zero congestion on a link")
 public:
  PiscesNullArbitrator(SST::Params& params);

  virtual void arbitrate(pkt_arbitration_t& st) override;

  std::string toString() const override {
    return "pisces null arbitrator";
  }

  Timestamp headTailDelay(PiscesPacket *pkt) override;

  uint32_t bytesSending(GlobalTimestamp now) const override;

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
  FactoryRegister("simple", PiscesBandwidthArbitrator, PiscesSimpleArbitrator,
              "Simple bandwidth arbitrator that only ever gives exclusive access to a link."
              "This corresponds to store-and-forward, which can be inaccurate for large packet sizes")
 public:
  PiscesSimpleArbitrator(SST::Params& params);

  virtual void arbitrate(pkt_arbitration_t& st) override;

  std::string toString() const override {
    return "pisces simple arbitrator";
  }

  Timestamp headTailDelay(PiscesPacket *pkt) override {
    //no delay
    return Timestamp();
  }

  uint32_t bytesSending(GlobalTimestamp now) const override;

 protected:
  GlobalTimestamp next_free_;

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
  FactoryRegister("cut_through", PiscesBandwidthArbitrator, PiscesCutThroughArbitrator,
              "Bandwidth arbitrator that forwards packets as soon as they arrive and enough credits are received"
              "This is a much better approximation to wormhole or virtual cut_through routing")
 private:
  typedef uint64_t ticks_t;
  typedef double bw_t;

 public:
  PiscesCutThroughArbitrator(SST::Params& params);

  ~PiscesCutThroughArbitrator();

  void arbitrate(pkt_arbitration_t& st) override;

  uint32_t bytesSending(GlobalTimestamp now) const override;

  std::string toString() const override {
    return "cut through arbitrator";
  }

  Timestamp headTailDelay(PiscesPacket *pkt) override;

 private:
  struct BandwidthEpoch : public sprockit::thread_safe_new<BandwidthEpoch> {
    uint32_t numCycles;
    Timestamp cycleLength;
    GlobalTimestamp start;
    Timestamp length;
    BandwidthEpoch* next;

    BandwidthEpoch() :
      next(nullptr) {
    }
    
    ~BandwidthEpoch(){}
  };

  BandwidthEpoch* addEpoch(GlobalTimestamp start, BandwidthEpoch* prev);
  BandwidthEpoch* advance(BandwidthEpoch* prev);

  BandwidthEpoch* head_;
  Timestamp init_epoch_length_;
  uint32_t init_num_cycles_;

};

}
}

#endif // PACKETFLOW_ARBITRATOR_H
