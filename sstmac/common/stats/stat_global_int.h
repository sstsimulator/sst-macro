#ifndef STAT_GLOBAL_INT_H
#define STAT_GLOBAL_INT_H

#include <sstmac/common/stats/stat_collector.h>
#include <vector>

namespace sstmac
{

class stat_global_int :
  public stat_collector
{
 public:
  stat_global_int(sprockit::sim_parameters* params);

  std::string
  to_string() const override {
    return "stat global int";
  }

  void
  collect(int value);

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

  stat_global_int*
  clone_me(int id) const {
    stat_global_int* cln = new stat_global_int(params_);
    cln->set_id(id);
    cln->set_label(label_);
    return cln;
  }

  stat_collector*
  clone() const override {
    return clone_me(-1);
  }

  void
  set_label(std::string label) {
    label_ = label;
  }

 protected:
  void
  dump(const std::string& froot);

 protected:
  int value_;
  std::string label_;
};

}

#endif // STAT_GLOBAL_INT_H
