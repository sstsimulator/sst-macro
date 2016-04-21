#ifndef PACKET_FLOW_STATS_H
#define PACKET_FLOW_STATS_H

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/serializable.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/topology/structured_topology_fwd.h>
#include <vector>

namespace sstmac {
namespace hw {

class stat_bytes_sent :
  public stat_collector
{
  template <class T> friend class sstmac::serialize;

 public:
  stat_bytes_sent() : top_(0), local_aggregation_(0) {}

  void
  record(int port, long bytes){
    port_map_[port] += bytes;
  }

  void
  set_topology(topology* top){
    top_ = top;
  }

  void
  simulation_finished(timestamp end);

  void
  dump_local_data();

  void
  dump_global_data();

  void
  global_reduce(parallel_runtime *rt);

  void
  reduce(stat_collector *coll);

  stat_collector*
  clone() const {
    stat_bytes_sent* cln = new stat_bytes_sent;
    clone_into(cln);
    return cln;
  }

  void
  clear(){}

 private:
  void
  global_reduce_non_root(parallel_runtime* rt, int root, char* buffer, int buffer_size);

  void
  collect_buffer_at_root(char* buffer, int buffer_size);

  void
  output_switch(int sid, std::fstream& data_str, structured_topology* top);

  struct global_gather_stats_t {
    int buffer_size;
  };

  void
  collect_counts_at_root(parallel_runtime* rt, int src, global_gather_stats_t stats);

  void
  global_reduce_root(parallel_runtime* rt, global_gather_stats_t* stats, char* my_buffer, int my_buffer_size);

 protected:
  void
  clone_into(stat_bytes_sent* cln) const;

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

    void
    append(int sid, const port_map& pmap){
      entry e;
      e.pmap = pmap;
      e.sid = sid;
      entries_.push_back(e);
      max_sid_ = std::max(max_sid_, sid);
      num_counts_ += pmap.size();
    }

    int
    num_counts() const {
      return num_counts_;
    }

    int
    num_entries() const {
      return entries_.size();
    }

    int
    ser_size() const {
      int entry_size = sizeof(int) + sizeof(size_t); //sid + map size
      int count_size = sizeof(int) + sizeof(long); //port + num bytes
      return num_entries() * entry_size + num_counts_ * count_size + sizeof(size_t);
    }

    int
    max_sid() const {
      return max_sid_;
    }

    const std::list<entry>&
    entries() const {
      return entries_;
    }

  };

  aggregation* local_aggregation_;
  std::vector<port_map> global_aggregation_;

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


#endif // PACKET_FLOW_STATS_H
