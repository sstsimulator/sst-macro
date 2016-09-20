#ifndef STAT_LOCAL_INT_H
#define STAT_LOCAL_INT_H

#include <sstmac/common/stats/stat_collector.h>
#include <vector>

namespace sstmac
{

class stat_local_int :
  public stat_collector
{
 public:
  stat_local_int(sprockit::sim_parameters* params);

  std::string
  to_string() const override {
    return "stat local int";
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

  stat_local_int*
  clone_me(int id) const  {
    stat_local_int* cln = new stat_local_int(params_);
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
  int value_;
  std::vector<int> values_;
};

}

#endif // STAT_LOCAL_INT_H
