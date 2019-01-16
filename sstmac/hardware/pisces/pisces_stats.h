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
#include <sstmac/common/stats/stat_global_int_fwd.h>
#include <sstmac/common/stats/stat_histogram_fwd.h>
#include <sstmac/common/event_manager_fwd.h>
#include <vector>

namespace sstmac {
namespace hw {

struct pkt_arbitration_t
{
  double incoming_bw;
  Timestamp now;
  Timestamp head_leaves;
  Timestamp tail_leaves;
  Timestamp credit_leaves;
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
  DeclareFactory(PacketStatsCallback, EventScheduler*)
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

  /**
   * @brief id
   * Either a node or switch id, depending on the device
   * @return
   */
  int id() const {
    return id_;
  }

 protected:
  PacketStatsCallback(SST::Params& params,
                        EventScheduler* parent);

 private:
  int id_;

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

class null_stats : public PacketStatsCallback
{
  FactoryRegister("null", PacketStatsCallback, null_stats)
 public:
  null_stats(SST::Params& params, EventScheduler* parent) :
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

class ByteHopCollector :
 virtual public PacketStatsCallback
{
  FactoryRegister("byte_hops", PacketStatsCallback, ByteHopCollector)
 public:
  ByteHopCollector(SST::Params& params, EventScheduler* parent);

  virtual ~ByteHopCollector();

  virtual void collectSingleEvent(const pkt_arbitration_t& st);

 private:
  StatGlobalInt* byte_hops_;
};

class StatBytesSent : public MultiStatistic<int, uint32_t>
{
  using Parent = MultiStatistic<int,uint32_t>;
  FRIEND_SERIALIZATION;
  FactoryRegister("bytes_sent", Parent, StatBytesSent)
 public:
  StatBytesSent(SST::Params& params);

  virtual ~StatBytesSent();

  void addData_impl(int port, uint32_t bytes){
    port_map_[port] += bytes;
  }

 private:
  void globalReduceNonRoot(ParallelRuntime* rt, int root, char* buffer, int buffer_size);

  void collectbufferAtRoot(char* buffer, int buffer_size);

  void outputSwitch(int sid, std::fstream& data_str);

  struct global_gather_stats_t {
    int buffer_size;
  };

  void collectCountsAtRoot(ParallelRuntime* rt, int src, global_gather_stats_t stats);

  void globalReduceRoot(ParallelRuntime* rt, global_gather_stats_t* stats,
                          char* my_buffer, int my_buffer_size);


 private:
  Topology* top_;

  typedef std::map<int, long> port_map;
  port_map port_map_;

  class aggregation
  {
    friend class StatBytesSent;
   public:
    struct entry
    {
      port_map pmap;
      int sid;

      void
      serialize_order(serializer& ser);
    };

   private:
    std::list<entry> entries_;
    int max_sid_;
    int num_counts_;

   public:
    aggregation() : max_sid_(0), num_counts_(0) {}

    void append(int sid, const port_map& pmap){
      entry e;
      e.pmap = pmap;
      e.sid = sid;
      entries_.push_back(e);
      max_sid_ = std::max(max_sid_, sid);
      num_counts_ += pmap.size();
    }

    int num_counts() const {
      return num_counts_;
    }

    int num_entries() const {
      return entries_.size();
    }

    int ser_size() const {
      int entry_size = sizeof(int) + sizeof(size_t); //sid + map size
      int count_size = sizeof(int) + sizeof(long); //port + num bytes
      return num_entries() * entry_size + num_counts_ * count_size + sizeof(size_t);
    }

    int max_sid() const {
      return max_sid_;
    }

    const std::list<entry>& entries() const {
      return entries_;
    }

  };

  aggregation* local_aggregation_;
  std::vector<port_map> global_aggregation_;

};

class BytesSentCollector :
 virtual public PacketStatsCallback
{
  FactoryRegister("bytes_sent", PacketStatsCallback, BytesSentCollector)
 public:
  BytesSentCollector(SST::Params& params, EventScheduler* parent);

  virtual ~BytesSentCollector();

  virtual void collectSingleEvent(const pkt_arbitration_t &st);

 private:
  StatBytesSent* bytes_sent_;
};

}
}

START_SERIALIZATION_NAMESPACE
template <>
class serialize<sstmac::hw::StatBytesSent::aggregation::entry> {
 public:
  void operator()(sstmac::hw::StatBytesSent::aggregation::entry& e, sstmac::serializer& ser){
    e.serialize_order(ser);
  }
};
END_SERIALIZATION_NAMESPACE


#endif // pisces_STATS_H
