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
  stat_global_int();

  void
  collect(int value);

  void
  simulation_finished(timestamp end);

  void
  dump_local_data();

  void
  dump_global_data();

  void
  global_reduce(parallel_runtime *rt);

  void
  clear();

  void
  reduce(stat_collector *coll);

  void
  init_factory_params(sprockit::sim_parameters *params);

  stat_global_int*
  clone_me(int id) const {
    stat_global_int* cln = new stat_global_int;
    clone_into(cln);
    cln->set_id(id);
    cln->set_label(label_);
    return cln;
  }

  stat_collector*
  clone() const {
    return clone_me(-1);
  }

  void
  set_label(std::string label) {
    label_ = label;
  }

 protected:
  void
  dump(const std::string& froot);

  void
  clone_into(stat_global_int* cln) const;

 protected:
  int value_;
  std::string label_;
};

}

#endif // STAT_GLOBAL_INT_H
