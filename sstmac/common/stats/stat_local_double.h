#ifndef STAT_LOCAL_DOUBLE_H
#define STAT_LOCAL_DOUBLE_H

#include <sstmac/common/stats/stat_collector.h>
#include <vector>

namespace sstmac
{

class stat_local_double :
  public stat_collector
{
 public:
  stat_local_double(sprockit::sim_parameters* params);

  std::string
  to_string() const override {
    return "stat local double";
  }

  void
  collect(double value);

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

  stat_local_double*
  clone_me(int id) const {
    stat_local_double* cln = new stat_local_double(params_);
    cln->set_id(id);
    return cln;
  }

  stat_collector*
  clone() const override {
    return clone_me(-1);
  }

 protected:
  void
  dump(const std::string& froot);

 protected:
  int size_;
  double value_;
  std::vector<double> values_;
};

}

#endif // STAT_LOCAL_DOUBLE_H
