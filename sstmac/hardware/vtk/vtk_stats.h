#ifndef sstmac_hw_vtk_stats_included_h
#define sstmac_hw_vtk_stats_included_h

#include <sstmac/common/stats/stat_collector.h>
#include <vector>
#include <queue>
#include <memory>
#include <sstmac/hardware/topology/topology.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/statapi/statfieldinfo.h>
//#include <sst/core/sst_types.h>
using namespace SST;
#endif
namespace sstmac {
namespace hw {

class topology;

struct traffic_event {
  uint64_t time_; // progress time
  int port_;
  double intensity_;
  int id_;

  traffic_event(uint64_t t, int port, double intens, int id) :
    time_(t), port_(port), intensity_(intens), id_(id)
  {
  }
};

#define VTK_UPPER32_MASK =

struct vtk_link {
  uint16_t id1;
  uint16_t id2;
  uint16_t port1;
  uint16_t port2;

  vtk_link(uint16_t i1, uint16_t p1, uint16_t i2, uint16_t p2) :
    id1(i1), port1(p1), id2(i2), port2(p2)
  {
    //if (i1 > i2){ //link is identified with i1 < i2
    //  std::swap(id1,id2);
    //  std::swap(port1,port2);
    //}
  }

  uint64_t id64() const {
    uint32_t i1 = id1;
    uint32_t p1 = port1;
    uint32_t side1 = (i1 << 16) | p1;
    uint32_t i2 = id2;
    uint32_t p2 = port2;
    uint32_t side2 = (i2 << 16) | p2;

    uint64_t s1 = side1;
    uint64_t s2 = side2;
    uint64_t id = (s1 << 32) | s2;
    return id;
  }

  static vtk_link construct(uint64_t id){
    uint64_t upper32_mask = (~uint64_t(0)) << 32;
    uint32_t upper16_mask  = (~uint32_t(0)) << 16;
    uint64_t lower32_mask = (~uint64_t(0)) >> 32;
    uint32_t lower16_mask = (~(uint32_t(0))) >> 16;


    uint32_t upper32 = (upper32_mask & id) >> 32;
    uint32_t lower32 = lower32_mask & id;

    uint16_t i1 = (upper16_mask & upper32) >> 16;
    uint16_t p1 = lower16_mask & upper32;
    uint16_t i2 = (upper16_mask & lower32) >> 16;
    uint16_t p2 = lower16_mask & lower32;

    return vtk_link(i1,p1,i2,p2);
  }
};

struct vtk_port {
  uint16_t id;
  uint16_t port;
  vtk_port(uint16_t i, uint16_t p)  :
    id(i), port(p) {}

  static vtk_port construct(uint32_t id){
    uint32_t upper16_mask  = (~uint32_t(0)) << 16;
    uint32_t lower16_mask = (~(uint32_t(0))) >> 16;
    uint16_t i = (upper16_mask & id) >> 16;
    uint16_t p = (lower16_mask & id);
    return vtk_port(i,p);
  }

  uint32_t id32() const {
    uint32_t i = id;
    uint32_t p = port;
    uint32_t myid = (i << 16) | p;
    return myid;
  }
};

class stat_vtk : public stat_collector
{
  FactoryRegister("vtk", stat_collector, stat_vtk)
 public:
  stat_vtk(sprockit::sim_parameters* params);

  std::string to_string() const override {
    return "VTK stats";
  }

  static void outputExodus(const std::string& fileroot,
      double bidirectional_shift,
      std::multimap<uint64_t, traffic_event>&& traffMap,
      topology *topo =nullptr);

  void dump_local_data() override;

  void dump_global_data() override;

  void global_reduce(parallel_runtime *rt) override;

  void clear() override;

  void collect_departure(timestamp now, timestamp time, int port);

  void collect_arrival(timestamp time, int port);

  void reduce(stat_collector *coll) override;

  void finalize(timestamp t) override;

  stat_collector* do_clone(sprockit::sim_parameters* params) const override {
    return new stat_vtk(params);
  }

  int id() const {
    return id_;
  }

  void configure(switch_id sid, hw::topology* top);

 private:
  void collect_departure(timestamp time, int port);

  void clear_pending_departures(timestamp now);

  struct port_intensity {
    int active_ports;
    int congested_ports;
    timestamp last_collection;
    timestamp pending_collection_start;
    int current_level;
    double accumulated_level;
    port_intensity() :
      accumulated_level(0.),
      current_level(0)
    {
    }
  };

  void collect_new_level(int port, timestamp time, int level);

  struct compare_pair {
    bool operator()(const std::pair<timestamp,int>& l, const std::pair<timestamp,int>& r){
      return l.first > r.first;
    }
  };

  std::vector<hw::topology::vtk_face_t> port_to_face_;
  std::vector<int> raw_port_intensities_;
  std::priority_queue<std::pair<timestamp,int>,
      std::vector<std::pair<timestamp,int>>,
      compare_pair> pending_departures_;
  std::vector<port_intensity> port_intensities_;
  int transition_cutoff_;
  timestamp ignored_gap_;
  double bidirectional_shift_;
  int id_;

  std::vector<traffic_event> event_list_;

  std::multimap<uint64_t, traffic_event> traffic_event_map_;
  hw::topology* top_;

};

}
}

#endif
