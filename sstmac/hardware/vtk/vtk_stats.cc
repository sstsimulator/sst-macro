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
stat_vtk::reduce(stat_collector* element)
{
  std::cout << "reduce called on switch" << std::endl;
  stat_vtk* contribution = safe_cast(stat_vtk, element);

  std::cout << " reduce " << contribution->traffic_event_map_.size()<<std::endl;

  for(auto it = contribution->traffic_event_map_.cbegin(); it != contribution->traffic_event_map_.cend(); ++it)
  {
      auto tp = std::make_shared<traffic_progress>();
      tp->time_ = it->first;
      tp->id_ = it->second->id_;
      // How to compute intensity ?
      // Let's do it by making a +1/-1 depending of the previous scalar value
      int previousIntensity = !contribution->traffic_progress_map_.empty() ?
                      contribution->traffic_progress_map_.crbegin()->second->intensity_ : 0;
      tp->intensity_ = it->second->type_ ? previousIntensity - 1 : previousIntensity + 1;

      contribution->traffic_progress_map_.insert({it->first, tp});

      if(tp->id_ == 3) {
          std::cout << "intensity "<<tp->time_ << " " << previousIntensity << ":"<<tp->intensity_<< std::endl;
          std::cout<< "map content ";
          for(auto it = contribution->traffic_progress_map_.cbegin(); it != contribution->traffic_progress_map_.cend(); ++it)
          {

              std::cout<< it->second->intensity_ << " ";

          }
          std::cout<<std::endl;
      }
  }

  for (auto& pair : contribution->traffic_progress_map_){
    traffic_progress_map_.insert({pair.first, pair.second});
  }
}

void
stat_vtk::dump_global_data()
{

  //here is where I dump the VTU file or whatever
  std::cout << "dump global data" << std::endl;

  std::multimap<int, std::multimap<int, int>> tf_nodes_map;

  // Fill the masp to display
  for(auto it = traffic_progress_map_.cbegin(); it != traffic_progress_map_.cend(); ++it)
  {
      auto nodeId = it->second->id_;
      auto resIt = tf_nodes_map.find(nodeId);
      if(resIt == tf_nodes_map.cend())
      {
          auto map = std::multimap<int, int>{};
          map.insert({it->second->time_, it->second->intensity_});

          tf_nodes_map.insert({nodeId, map});
      }
      else
      {
          auto &map = resIt->second;
          map.insert({it->second->time_, it->second->intensity_});
      }

  }
  // display the map

  for(auto it = tf_nodes_map.cbegin(); it != tf_nodes_map.cend(); ++it)
  {
      auto nodeId = it->first;
      const auto &map = it->second;
      std::cout<<nodeId<<":";
      for(auto it = map.cbegin(); it != map.cend(); ++it)
      {
          std::cout<< it->second << " ";
      }
      std::cout<<std::endl;
  }

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

    auto tp = std::make_shared<traffic_event>();
    tp->time_ = time;
    tp->id_ = switch_id;
    tp->p_ = port;
    tp->type_ = 0;

    if(switch_id == 3)
    {
        std::cout << "ARRIVAL " << time << std::endl;
    }
    traffic_event_map_.insert({time, tp});
}

void
stat_vtk::collect_departure(uint64_t time, int switch_id, int port)
{
    auto tp = std::make_shared<traffic_event>();
    tp->time_ = time;
    tp->id_ = switch_id;
    tp->p_ = port;
    tp->type_ = 1;
    if(switch_id == 3)
    {
        std::cout << "LEAVE " << time << std::endl;
    }
    traffic_event_map_.insert({time, tp});
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
