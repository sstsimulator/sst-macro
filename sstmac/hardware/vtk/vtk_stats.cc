#include <sstmac/hardware/vtk/vtk_stats.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
{ "name", "a test parameter" },
);

namespace sstmac {
namespace hw {


stat_vtk::stat_vtk(sprockit::sim_parameters *params) :
  stat_collector(params)
{
  std::cout << "I got named " << params->get_param("name")
            << " with id "
            << (params->has_param("id") ? params->get_param("id") : "aggregator")
            << std::endl;
}

void
stat_vtk::reduce(stat_collector* main)
{
  std::cout << "reduce called on switch" << std::endl;
  stat_vtk* aggregator = safe_cast(stat_vtk, main);

  for (auto& pair : events_){
    uint64_t ticks = pair.first;
    double time_in_secs = timestamp(pair.first, timestamp::exact).sec();
    aggregator->events_[pair.first] = pair.second;
  }
}

void
stat_vtk::dump_global_data()
{
  //here is where I dump the VTU file or whatever
  std::cout << "dump global data" << std::endl;
}

void
stat_vtk::dump_local_data()
{
  //if I want a file per node/switch
}

void
stat_vtk::clear()
{
}

void
stat_vtk::collect_arrival(uint64_t time, int switch_id, int port)
{
}

void
stat_vtk::collect_departure(uint64_t time, int switch_id, int port)
{
}

void
stat_vtk::global_reduce(parallel_runtime *rt)
{
  if (rt->nproc() > 1){
    sprockit::abort("stat_vtk::global_reduce for MPI parallel");
  }
  //otherwise nothing to do
}

}
}
