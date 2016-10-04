#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/common/stats/stat_local_int.h>
#include <sprockit/spkt_string.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <math.h>

namespace sstmac {

SpktRegister("local_int", stat_collector, stat_local_int);

stat_local_int::stat_local_int(sprockit::sim_parameters* params) :
  stat_value<int>(params)
{
}

void
stat_local_int::global_reduce(parallel_runtime *rt)
{
  if (rt->nproc() == 1)
    return;
  spkt_throw(sprockit::unimplemented_error, "stat_local_int::global_reduce");
}

void
stat_local_int::reduce(stat_collector *coll)
{
  stat_local_int* other = safe_cast(stat_local_int, coll);
  if ((other->id_ + 1) > values_.size()) {
    values_.resize(other->id_ + 1);
  }
  values_[other->id_] = other->value_;
}

void
stat_local_int::dump(const std::string& froot)
{
  std::string data_file = froot + ".dat";
  std::fstream data_str;
  check_open(data_str, data_file);
  data_str << "Id Value\n";
  for (int i=0; i < values_.size(); ++i)
    data_str << sprockit::printf("%i %i\n", i, values_[i]);
  data_str.close();
}

void
stat_local_int::dump_global_data()
{
  dump(fileroot_);
}

void
stat_local_int::dump_local_data()
{
  std::string fname = sprockit::printf("%s.%d", fileroot_.c_str(), id_);
  dump(fname);
}

void
stat_local_int::clear()
{
}

void
stat_local_int::simulation_finished(timestamp end)
{
}




}
