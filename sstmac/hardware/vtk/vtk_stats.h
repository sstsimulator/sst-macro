#ifndef sstmac_hw_vtk_stats_included_h
#define sstmac_hw_vtk_stats_included_h

#include <sstmac/common/stats/stat_collector.h>
#include <vector>

namespace sstmac {
namespace hw {

class stat_vtk : public stat_collector
{
  FactoryRegister("vtk", stat_collector, stat_vtk)
 public:
  stat_vtk(sprockit::sim_parameters* params);

  std::string to_string() const override {
    return "VTK stats";
  }

  void dump_local_data() override;

  void dump_global_data() override;

  void global_reduce(parallel_runtime *rt) override;

  void clear() override;

  void collect_departure(uint64_t time, int switch_id, int port);

  void collect_arrival(uint64_t time, int switch_id, int port);

  void reduce(stat_collector *coll) override;

  stat_collector* do_clone(sprockit::sim_parameters* params) const override {
    return new stat_vtk(params);
  }

 private:
  struct Event {};
  std::map<uint64_t, Event> events_;

};

}
}

#endif
