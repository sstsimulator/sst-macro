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
  int face_; //the face to color
  double intensity_;
  int id_;

  traffic_event(uint64_t t, int face, double intens, int id) :
    time_(t), face_(face), intensity_(intens), id_(id)
  {
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

  struct face_intensity {
    int active_ports;
    int congested_ports;
    timestamp last_collection;
    timestamp pending_collection_start;
    int current_level;
    double accumulated_level;
    face_intensity() :
      active_ports(0),
      congested_ports(0),
      accumulated_level(0),
      current_level(0)
    {
    }
  };

  void collect_new_level(int face, timestamp time, int level);

  struct compare_pair {
    bool operator()(const std::pair<timestamp,double>& l, const std::pair<timestamp,double>& r){
      return l.first > r.first;
    }
  };

  std::vector<hw::topology::vtk_face_t> port_to_face_;
  std::vector<int> port_intensities_;
  std::priority_queue<std::pair<timestamp,double>,
      std::vector<std::pair<timestamp,double>>,
      compare_pair> pending_departures_;
  std::vector<face_intensity> face_intensities_;
  int transition_cutoff_;
  timestamp ignored_gap_;
  int id_;

  std::vector<traffic_event> event_list_;

  std::multimap<uint64_t, traffic_event> traffic_event_map_;
  hw::topology* top_;


};

}
}

#endif
