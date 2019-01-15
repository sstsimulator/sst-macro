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

  double outgoing_bw() const {
    return out_bw_;
  }

  static inline Timestamp
  credit_delay(double max_in_bw, double out_bw, long bytes){
    double credit_delta = 1.0/out_bw - 1.0/max_in_bw;
    credit_delta = std::max(0., credit_delta);
    return Timestamp(bytes * credit_delta);
  }

  virtual void initNoiseModel(NoiseModel* noise);

  /**
   * @brief partition Partition the arbitrator time windows into a series of randomly sized chunks
   * @param noise  The noise model that randomly selects time values
   * @param num_intervals
   */
  virtual void partition(NoiseModel* noise, int num_intervals);

  virtual uint32_t bytesSending(Timestamp now) const = 0;

 protected:
  PiscesBandwidthArbitrator(sprockit::sim_parameters::ptr& params);

 protected:
  double out_bw_;
  double inv_out_bw_;

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
  PiscesNullArbitrator(sprockit::sim_parameters::ptr& params);

  virtual void arbitrate(pkt_arbitration_t& st) override;

  std::string toString() const override {
    return "pisces null arbitrator";
  }

  Timestamp headTailDelay(PiscesPacket *pkt) override;

  uint32_t bytesSending(Timestamp now) const override;

};

/**
 * @brief The pisces_simple_arbitrator class Implements a store-and-forward arbitration scheme.
 * The entire packet has to arrive before it can be arbitrated.  Links/crossbars never interleave
 * flits from different packets in this scheme.  For larger packet sizes, this can lead to
 * unrealistic delays since packets cannot pipeline across network stages.
 */
class pisces_simple_arbitrator :
  public PiscesBandwidthArbitrator
{
  FactoryRegister("simple", PiscesBandwidthArbitrator, pisces_simple_arbitrator,
              "Simple bandwidth arbitrator that only ever gives exclusive access to a link."
              "This corresponds to store-and-forward, which can be inaccurate for large packet sizes")
 public:
  pisces_simple_arbitrator(sprockit::sim_parameters::ptr& params);

  virtual void arbitrate(pkt_arbitration_t& st) override;

  std::string toString() const override {
    return "pisces simple arbitrator";
  }

  Timestamp headTailDelay(PiscesPacket *pkt) override {
    //no delay
    return Timestamp();
  }

  uint32_t bytesSending(Timestamp now) const override;

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
  FactoryRegister("cut_through", PiscesBandwidthArbitrator, PiscesCutThroughArbitrator,
              "Bandwidth arbitrator that forwards packets as soon as they arrive and enough credits are received"
              "This is a much better approximation to wormhole or virtual cut_through routing")
 private:
  typedef uint64_t ticks_t;
  typedef double bw_t;

 public:
  PiscesCutThroughArbitrator(sprockit::sim_parameters::ptr& params);

  ~PiscesCutThroughArbitrator();

  virtual void arbitrate(pkt_arbitration_t& st) override;

  uint32_t bytesSending(Timestamp now) const override;

  std::string toString() const override {
    return "cut through arbitrator";
  }

  void partition(NoiseModel* model,
    int num_intervals) override;

  void initNoiseModel(NoiseModel* noise) override;

  Timestamp headTailDelay(PiscesPacket *pkt) override;

 private:
  void cleanUp(ticks_t now);

  void doArbitrate(pkt_arbitration_t& st);

  struct bandwidth_epoch : public sprockit::thread_safe_new<bandwidth_epoch> {
    bw_t bw_available; //bandwidth is bytes per timestamp tick
    ticks_t start;
    ticks_t length;
    bandwidth_epoch* next;

    bandwidth_epoch() :
      next(nullptr) {
    }
    
    ~bandwidth_epoch(){
    }

    void truncateAfter(ticks_t delta_t);

    void split(ticks_t delta_t);
  };

  bandwidth_epoch* head_;

  /** Convert from bytes/sec to bytes/tick */
  double bw_tick_to_sec_conversion_;
  double bw_sec_to_tick_conversion_;

};

}
}

#endif // PACKETFLOW_ARBITRATOR_H
