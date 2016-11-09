#ifndef STAT_LOCAL_DOUBLE_H
#define STAT_LOCAL_DOUBLE_H

#include <sstmac/common/stats/stat_collector.h>
#include <vector>

namespace sstmac
{

class stat_local_double :
  public stat_value<double>
{
 public:
  stat_local_double(sprockit::sim_parameters* params);

  std::string
  to_string() const override {
    return "stat local double";
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
    return new stat_local_double(params);
  }

 protected:
  void
  dump(const std::string& froot);

 protected:
  std::vector<double> values_;
};

}

#endif // STAT_LOCAL_DOUBLE_H
