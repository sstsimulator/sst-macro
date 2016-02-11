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
  stat_histogram();

  void
  collect(double value);

  void
  collect(double value, int num);

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

  stat_histogram*
  clone_me(int id) const {
    stat_histogram* cln = new stat_histogram;
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
  clone_into(stat_histogram* cln) const;

 protected:
  std::vector<long> counts_;

  double bin_size_;

  long max_bin_;

  bool is_log_;

};

}

#endif // STAT_HISTOGRAM_H
