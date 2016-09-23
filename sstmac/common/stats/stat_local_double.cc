#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/common/stats/stat_local_double.h>
#include <sprockit/spkt_string.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <math.h>

namespace sstmac {

SpktRegister("local_double", stat_collector, stat_local_double);

stat_local_double::stat_local_double(sprockit::sim_parameters* params) :
    stat_value<double>(params)
{
}

void
stat_local_double::global_reduce(parallel_runtime *rt)
{
  if (rt->nproc() == 1)
    return;
  spkt_throw(sprockit::unimplemented_error, "stat_local_double::global_reduce");
}

void
stat_local_double::reduce(stat_collector *coll)
{
  stat_local_double* other = safe_cast(stat_local_double, coll);
  //std::cerr << "id: " << other->id_ << "\n";
  //if (other->id_ < 0) return;
  if ((other->id_ + 1) > values_.size()) {
    values_.resize(other->id_ + 1);
  }
  values_[other->id_] = other->value_;
}

void
stat_local_double::dump(const std::string& froot)
{
  std::string data_file = froot + ".dat";
  std::fstream data_str;
  check_open(data_str, data_file);
  data_str << "Id Value\n";
  for (int i=0; i < values_.size(); ++i)
    data_str << sprockit::printf("%i %lf\n", i, values_[i]);
  data_str.close();
}

void
stat_local_double::dump_global_data()
{
  dump(fileroot_);
}

void
stat_local_double::dump_local_data()
{
  std::string fname = sprockit::printf("%s.%d", fileroot_.c_str(), id_);
  dump(fname);
}

void
stat_local_double::clear()
{
}

void
stat_local_double::simulation_finished(timestamp end)
{
}




}
