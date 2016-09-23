#ifndef STAT_GLOBAL_INT_H
#define STAT_GLOBAL_INT_H

#include <sstmac/common/stats/stat_collector.h>
#include <vector>

namespace sstmac
{

class stat_global_int :
  public stat_value<int>
{
 public:
  stat_global_int(sprockit::sim_parameters* params);

  std::string
  to_string() const override {
    return "stat global int";
  }

  void
  simulation_finished(timestamp end) override;

  void
  dump_local_data() override;

  void
  dump_global_data() override;

  void
  global_reduce(parallel_runtime *rt) override;

  void
  clear() override;

  void
  reduce(stat_collector *coll) override;

  stat_collector*
  do_clone(sprockit::sim_parameters* params) const override {
    return new stat_global_int(params);
  }

 protected:
  void
  dump(const std::string& froot);

};

}

#endif // STAT_GLOBAL_INT_H
