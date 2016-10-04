#ifndef STAT_LOCAL_INT_H
#define STAT_LOCAL_INT_H

#include <sstmac/common/stats/stat_collector.h>
#include <vector>

namespace sstmac
{

class stat_local_int :
  public stat_value<int>
{
 public:
  stat_local_int(sprockit::sim_parameters* params);

  std::string
  to_string() const override {
    return "stat local int";
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

  stat_collector*
  do_clone(sprockit::sim_parameters* params) const override {
    return new stat_local_int(params);
  }

  void
  reduce(stat_collector *coll) override;

 protected:
  void
  dump(const std::string& froot);

 protected:
  std::vector<int> values_;
};

}

#endif // STAT_LOCAL_INT_H
