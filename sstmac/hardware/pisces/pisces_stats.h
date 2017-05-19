/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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
  timestamp now;
  timestamp head_leaves;
  timestamp tail_leaves;
  timestamp credit_leaves;
  pisces_payload* pkt;
  int src_outport;
  int dst_inport;
};

/**
 * @brief The packet_stats_callback class is an optional callback
 * that can be registered with pisces_sender objects or packetizer
 * objects to log events ocurring on particular packets
 */
class packet_stats_callback
{
  DeclareFactory(packet_stats_callback, event_scheduler*)
 public:
  virtual ~packet_stats_callback(){}

  /**
   * @brief collect_single_event Collect stats associated with a single
   *            packet aribtration event. This is invokved by pisces_sender
   *            objects.
   * @param st All the details of the last arbitration of a given packet
   */
  virtual void collect_single_event(const pkt_arbitration_t& st);

  /**
   * @brief collect_final_event Collects stats associated with flow-level
   *          packet event. This is attached to packetizer objecs the end
   *          of the flow (usually on a NIC) to log any important stats
   *          attached to a packet at the end of the path
   * @param pkt
   */
  virtual void collect_final_event(pisces_payload* pkt);

  /**
   * @brief id
   * Either a node or switch id, depending on the device
   * @return
   */
  int id() const {
    return id_;
  }

 protected:
  packet_stats_callback(sprockit::sim_parameters* params,
                        event_scheduler* parent);

 private:
  int id_;

};

class congestion_spyplot :
 virtual public packet_stats_callback
{
  FactoryRegister("congestion_spyplot", packet_stats_callback, congestion_spyplot)
 public:
  congestion_spyplot(sprockit::sim_parameters* params, event_scheduler* parent);

  virtual ~congestion_spyplot();

  virtual void collect_single_event(const pkt_arbitration_t& st);

  virtual void collect_final_event(pisces_payload* pkt);

 protected:
  void collect(double delay_us, pisces_payload* pkt);

 private:
  stat_spyplot* congestion_spyplot_;
};


class delay_histogram :
  virtual public packet_stats_callback
{
  FactoryRegister("delay_histogram", packet_stats_callback, delay_histogram)
 public:
  delay_histogram(sprockit::sim_parameters* params, event_scheduler* parent);

  virtual ~delay_histogram();

  virtual void collect_final_event(pisces_payload* pkt);

  virtual void collect_single_event(const pkt_arbitration_t& st);

 private:
  stat_histogram* congestion_hist_;
};

class packet_delay_stats :
 virtual public packet_stats_callback
{
  FactoryRegister("congestion_delay", packet_stats_callback, packet_delay_stats)
 public:
  packet_delay_stats(sprockit::sim_parameters* params, event_scheduler* parent) :
    packet_stats_callback(params, parent)
  {
  }

  virtual void collect_single_event(const pkt_arbitration_t &st);

};

class null_stats : public packet_stats_callback
{
  FactoryRegister("null", packet_stats_callback, null_stats)
 public:
  null_stats(sprockit::sim_parameters* params, event_scheduler* parent) :
    packet_stats_callback(params, parent)
  {
  }

  virtual void collect_single_event(const pkt_arbitration_t &st){}

  virtual void collect_final_event(pisces_payload *pkt){}
};

class multi_stats : public packet_stats_callback
{
  FactoryRegister("multi", packet_stats_callback, multi_stats)
 public:
  multi_stats(sprockit::sim_parameters* params, event_scheduler* parent);

  void collect_single_event(const pkt_arbitration_t &st);

  void collect_final_event(pisces_payload *pkt);

 private:
  std::vector<packet_stats_callback*> cbacks_;

};

class byte_hop_collector :
 virtual public packet_stats_callback
{
  FactoryRegister("byte_hops", packet_stats_callback, byte_hop_collector)
 public:
  byte_hop_collector(sprockit::sim_parameters* params, event_scheduler* parent);

  virtual ~byte_hop_collector();

  virtual void collect_single_event(const pkt_arbitration_t& st);

 private:
  stat_global_int* byte_hops_;
};

class stat_bytes_sent :
  public stat_collector
{
  FRIEND_SERIALIZATION;
  FactoryRegister("bytes_sent", stat_collector, stat_bytes_sent)
 public:
  stat_bytes_sent(sprockit::sim_parameters* params);

  std::string to_string() const override {
    return "stat bytes sent";
  }

  virtual ~stat_bytes_sent();

  void record(int port, long bytes){
    port_map_[port] += bytes;
  }

  void simulation_finished(timestamp end) override;

  void dump_local_data() override;

  void dump_global_data() override;

  void global_reduce(parallel_runtime *rt) override;

  void reduce(stat_collector *coll) override;

  stat_collector* do_clone(sprockit::sim_parameters* params) const override {
    return new stat_bytes_sent(params);
  }

  void clear() override {}

 private:
  void global_reduce_non_root(parallel_runtime* rt, int root, char* buffer, int buffer_size);

  void collect_buffer_at_root(char* buffer, int buffer_size);

  void output_switch(int sid, std::fstream& data_str);

  struct global_gather_stats_t {
    int buffer_size;
  };

  void collect_counts_at_root(parallel_runtime* rt, int src, global_gather_stats_t stats);

  void global_reduce_root(parallel_runtime* rt, global_gather_stats_t* stats,
                          char* my_buffer, int my_buffer_size);


 private:
  topology* top_;

  typedef std::map<int, long> port_map;
  port_map port_map_;

  class aggregation
  {
    friend class stat_bytes_sent;
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

class bytes_sent_collector :
 virtual public packet_stats_callback
{
  FactoryRegister("bytes_sent", packet_stats_callback, bytes_sent_collector)
 public:
  bytes_sent_collector(sprockit::sim_parameters* params, event_scheduler* parent);

  virtual ~bytes_sent_collector();

  virtual void collect_single_event(const pkt_arbitration_t &st);

 private:
  stat_bytes_sent* bytes_sent_;
};

}
}

SER_NAMESPACE_OPEN
template <>
class serialize<sstmac::hw::stat_bytes_sent::aggregation::entry> {
 public:
  void operator()(sstmac::hw::stat_bytes_sent::aggregation::entry& e, sstmac::serializer& ser){
    e.serialize_order(ser);
  }
};
SER_NAMESPACE_CLOSE


#endif // pisces_STATS_H