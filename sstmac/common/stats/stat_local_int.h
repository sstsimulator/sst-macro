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
  stat_local_int();

  void
  init(int id) {
    id_ = id;
  }

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

  stat_local_int*
  clone_me(int id) const {
    stat_local_int* cln = new stat_local_int;
    clone_into(cln);
    cln->set_id(id);
    return cln;
  }

  stat_collector*
  clone() const {
    return clone_me(-1);
  }

 protected:
  void
  dump(const std::string& froot);

  void
  clone_into(stat_local_int* cln) const;

 protected:
  int size_;
  int id_;
  int value_;
  std::vector<int> values_;
};

}

#endif // STAT_LOCAL_INT_H
