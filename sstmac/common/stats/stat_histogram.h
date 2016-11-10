#ifndef STAT_HISTOGRAM_H
#define STAT_HISTOGRAM_H

#include <sstmac/common/stats/stat_collector.h>
#include <vector>

namespace sstmac
{

class stat_histogram :
  public stat_collector
{
 public:
  stat_histogram(sprockit::sim_parameters* params);

  std::string
  to_string() const override {
    return "stat histogram";
  }

  void
  collect(double value);

  void
  collect(double value, int64_t num);

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
  reduce(stat_collector* coll) override;

  stat_collector*
  do_clone(sprockit::sim_parameters* params) const override {
    return new stat_histogram(params);
  }

 protected:
  void
  dump(const std::string& froot);

 protected:
  std::vector<int64_t> counts_;

  double bin_size_;

  int64_t max_bin_;

  bool is_log_;

};

class stat_time_histogram :
  public stat_histogram
{
 public:
  stat_time_histogram(sprockit::sim_parameters* params) :
    stat_histogram(params)
  {
  }

  void record(timestamp t, int64_t num);
};

}

#endif // STAT_HISTOGRAM_H
