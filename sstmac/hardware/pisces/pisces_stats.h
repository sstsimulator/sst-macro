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

#ifndef pisces_STATS_H
#define pisces_STATS_H

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/serializable.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/topology/structured_topology_fwd.h>
#include <sstmac/hardware/pisces/pisces.h>
#include <sstmac/common/stats/stat_spyplot_fwd.h>
#include <sstmac/common/stats/stat_histogram_fwd.h>
#include <sstmac/common/event_manager_fwd.h>
#include <vector>

namespace sstmac {
namespace hw {

struct pkt_arbitration_t
{
  Timestamp incoming_byte_delay;
  GlobalTimestamp now;
  GlobalTimestamp head_leaves;
  GlobalTimestamp tail_leaves;
  PiscesPacket* pkt;
  int src_outport;
  int dst_inport;
};

/**
 * @brief The PacketStatsCallback class is an optional callback
 * that can be registered with PiscesSender objects or packetizer
 * objects to log events ocurring on particular packets
 */
class PacketStatsCallback
{
  DeclareFactoryArgs(PacketStatsCallback, EventScheduler*)
 public:
  virtual ~PacketStatsCallback(){}

  /**
   * @brief collect_single_event Collect stats associated with a single
   *            packet aribtration event. This is invokved by PiscesSender
   *            objects.
   * @param st All the details of the last arbitration of a given packet
   */
  virtual void collectSingleEvent(const pkt_arbitration_t& st);

  /**
   * @brief collect_final_event Collects stats associated with flow-level
   *          packet event. This is attached to packetizer objecs the end
   *          of the flow (usually on a NIC) to log any important stats
   *          attached to a packet at the end of the path
   * @param pkt
   */
  virtual void collectFinalEvent(PiscesPacket* pkt);

 protected:
  PacketStatsCallback(SST::Params& params, EventScheduler* parent);

};

class CongestionSpyplot :
 virtual public PacketStatsCallback
{
  FactoryRegister("congestion_spyplot", PacketStatsCallback, CongestionSpyplot)
 public:
  CongestionSpyplot(SST::Params& params, EventScheduler* parent);

  virtual ~CongestionSpyplot();

  virtual void collectSingleEvent(const pkt_arbitration_t& st);

  virtual void collectFinalEvent(PiscesPacket* pkt);

 protected:
  void collect(double delay_us, PiscesPacket* pkt);

 private:
  StatSpyplot* congestion_spyplot_;
};


class DelayHistogram :
  virtual public PacketStatsCallback
{
  FactoryRegister("delay_histogram", PacketStatsCallback, DelayHistogram)
 public:
  DelayHistogram(SST::Params& params, EventScheduler* parent);

  virtual ~DelayHistogram();

  virtual void collectFinalEvent(PiscesPacket* pkt);

  virtual void collectSingleEvent(const pkt_arbitration_t& st);

 private:
  StatHistogram* congestion_hist_;
};

class PacketDelayStats :
 virtual public PacketStatsCallback
{
  FactoryRegister("congestion_delay", PacketStatsCallback, PacketDelayStats)
 public:
  PacketDelayStats(SST::Params& params, EventScheduler* parent) :
    PacketStatsCallback(params, parent)
  {
  }

  virtual void collectSingleEvent(const pkt_arbitration_t &st);

};

class NullStats : public PacketStatsCallback
{
  FactoryRegister("null", PacketStatsCallback, NullStats)
 public:
  NullStats(SST::Params& params, EventScheduler* parent) :
    PacketStatsCallback(params, parent)
  {
  }

  virtual void collectSingleEvent(const pkt_arbitration_t &st){}

  virtual void collectFinalEvent(PiscesPacket *pkt){}
};

class MultiStats : public PacketStatsCallback
{
  FactoryRegister("multi", PacketStatsCallback, MultiStats)
 public:
  MultiStats(SST::Params& params, EventScheduler* parent);

  void collectSingleEvent(const pkt_arbitration_t &st);

  void collectFinalEvent(PiscesPacket *pkt);

 private:
  std::vector<PacketStatsCallback*> cbacks_;

};

}
}


#endif // pisces_STATS_H
