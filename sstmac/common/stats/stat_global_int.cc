#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/common/stats/stat_global_int.h>
#include <sprockit/spkt_string.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <math.h>

namespace sstmac {

SpktRegister("global_int", stat_collector, stat_global_int);

stat_global_int::stat_global_int(sprockit::sim_parameters* params) :
    stat_value<int>(params)
{
}

void
stat_global_int::global_reduce(parallel_runtime *rt)
{
  if (rt->nproc() == 1)
    return;
  spkt_throw(sprockit::unimplemented_error, "stat_global_int::global_reduce");
}

void
stat_global_int::reduce(stat_collector *coll)
{
  stat_global_int* other = safe_cast(stat_global_int, coll);
  value_ += other->value_;
}

void
stat_global_int::dump(const std::string& froot)
{
  //std::string data_file = froot + ".dat";
  //std::fstream data_str;
  //check_open(data_str, data_file);
  //data_str << "Value\n";
  //data_str << sprockit::printf("%i\n", value_);
  //data_str.close();
  cout0 << sprockit::printf("%s: %i\n", label_.c_str(), value_);
}

void
stat_global_int::dump_global_data()
{
  dump(fileroot_);
}

void
stat_global_int::dump_local_data()
{
  std::string fname = sprockit::printf("%s.%d", fileroot_.c_str(), id_);
  dump(fname);
}

void
stat_global_int::clear()
{
}

void
stat_global_int::simulation_finished(timestamp end)
{
}




}
