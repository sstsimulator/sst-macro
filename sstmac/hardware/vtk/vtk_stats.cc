#include <sstmac/hardware/vtk/vtk_stats.h>
#include <sstmac/hardware/vtk/vtkXMLPVDWriter.h>
#include <sstmac/hardware/vtk/vtkTrafficSource.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>


#if SSTMAC_VTK_ENABLED
#include <vtkInformation.h>
#include <vtkStreamingDemandDrivenPipeline.h>
#include <vtkVersion.h>
#include <vtkSmartPointer.h>
#include <vtkPolyVertex.h>
#include <vtkVertex.h>
#include <vtkCellArray.h>
#include <vtkXMLUnstructuredGridReader.h>
#include <vtkDataSetMapper.h>
#include <vtkActor.h>
#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkUnstructuredGrid.h>
#include <vtkPointData.h>
#include <vtkVertexGlyphFilter.h>
#include "vtkStdString.h"
#include "vtkExodusIIWriter.h"
#endif

RegisterKeywords(
{ "name","a test parameter" },
{ "count_x", "topology size in x dim"},
{ "count_y", "topology size in y dim"},
    );

namespace sstmac {
namespace hw {

void
outputExodusWithSharedMap(const std::string& fileroot,
   const std::multimap<uint64_t, std::shared_ptr<traffic_event>> &trafficMap,
   int count_x, int count_y){

  // The goal:
  // The idea is to construct the geometry using coordinates of the switch and the coordinates of each ports.
  // Then, we can apply varying time traffic values on ports and switch points.

  // What we know:
  // We assuming the traffic of each port start with 0
  // for each time step (key) of the traffic map parameter, we know the traffic value to update for the (switch,port) associated.
  // The switch traffic value itself needs to be recomputed using the mean of all current traffic values of the ports of the switch.


  // Init traffic array with default 0 traffic value
  vtkSmartPointer<vtkIntArray> traffic =
      vtkSmartPointer<vtkIntArray>::New();
  traffic->SetNumberOfComponents(1);
  traffic->SetName("MyTraffic");
  int count_xy = count_x * count_y; //
  traffic->SetNumberOfValues(count_xy);
  for (auto i = 0; i < count_xy; ++i){
    traffic->SetValue(i, 0);
  }

  // Create the vtkPoints
  vtkSmartPointer<vtkPoints> points =
      vtkSmartPointer<vtkPoints>::New();

  // Use the geometry to place them correctly
  for (auto j = 0; j < count_y; ++j){
    for (auto i = 0; i < count_x; ++i){
      points->InsertNextPoint(i, j, 0);
    }
  }

  // Create the vtkCellArray
  vtkSmartPointer<vtkCellArray> cell_array =
      vtkSmartPointer<vtkCellArray>::New();

  for (auto i = 0; i < count_xy; ++i){
    vtkSmartPointer<vtkVertex> point_data =
        vtkSmartPointer<vtkVertex>::New();
    point_data->GetPointIds()->SetId(0,i);
    cell_array->InsertNextCell(point_data);
  }

  vtkSmartPointer<vtkUnstructuredGrid> unstructured_grid =
      vtkSmartPointer<vtkUnstructuredGrid>::New();
  unstructured_grid->SetPoints(points);
  unstructured_grid->SetCells(VTK_VERTEX, cell_array);
  unstructured_grid->GetPointData()->AddArray(traffic);

  // Init Time Step
  double current_time = -1;
  double *time_step_value = new double[trafficMap.size() + 1];
  time_step_value[0] = 0.;
  int currend_index = 1;
  for (auto it = trafficMap.cbegin(); it != trafficMap.cend(); ++it){
    if (it->first != current_time){
      current_time = it->first;
      time_step_value[currend_index] =it->first;
      ++currend_index;
    }
  }
  // TOCHECK: time_step_value for trafficMap.size() > values >= currend_index isn't intialized (and shouldn't be used)
  // time_step_value should be resized ?

  vtkSmartPointer<vtkTrafficSource> trafficSource =
      vtkSmartPointer<vtkTrafficSource>::New();
  trafficSource->SetSteps(time_step_value);
  trafficSource->SetNumberOfSteps(currend_index);
  trafficSource->SetPoints(points);
  trafficSource->SetCells(cell_array);
  trafficSource->SetTraffics(traffic);
  trafficSource->SetTrafficProgressMap(trafficMap);
  vtkSmartPointer<vtkExodusIIWriter> exodusWriter =
      vtkSmartPointer<vtkExodusIIWriter>::New();
  std::string fileName = fileroot + ".e";
  exodusWriter->SetFileName(fileName.c_str());
  exodusWriter->SetInputConnection (trafficSource->GetOutputPort());
  exodusWriter->WriteAllTimeStepsOn ();
  exodusWriter->Write();

}

void
stat_vtk::outputExodus(const std::string& fileroot,
    const std::multimap<uint64_t, traffic_event> &traffMap,
    int count_x, int count_y) {

  std::multimap<uint64_t, std::shared_ptr<traffic_event>> trafficMap;
  for(auto elt : traffMap){
    auto eltSecond =  std::make_shared<traffic_event>(elt.second);
    trafficMap.insert(std::pair<uint64_t, std::shared_ptr<traffic_event>>(elt.first, eltSecond));
  }

  outputExodusWithSharedMap(fileroot, trafficMap, count_x, count_y);
}


stat_vtk::stat_vtk(sprockit::sim_parameters *params) :
  stat_collector(params)
{
  /**
  std::cout << "I got named " << params->get_param("name")
            << " with id "
            << (params->has_param("id") ? params->get_param("id") : "aggregator")
            << std::endl;
  std::cout << "geom " <<params->get_param("count_x") << " "<< params->get_param("count_y") <<std::endl;
  */
  count_x_ = params->has_param("count_x") ? std::stoi(params->get_param("count_x")) : 0;
  count_y_ = params->has_param("count_y") ? std::stoi(params->get_param("count_y")) : 0;
}

void
stat_vtk::reduce(stat_collector* element)
{
  stat_vtk* contribution = safe_cast(stat_vtk, element);

  for (auto it = contribution->traffic_event_map_.cbegin(); it != contribution->traffic_event_map_.cend(); ++it){
    auto tp = std::make_shared<traffic_event>();
    tp->time_ = it->first;
    tp->id_ = it->second->id_;
    // How to compute intensity ?
    // Let's do it by making a +1/-1 depending of the previous scalar value
    int previousIntensity = !contribution->traffic_progress_map_.empty() ?
          contribution->traffic_progress_map_.crbegin()->second->intensity_ : 0;
    tp->intensity_ = it->second->type_ ? previousIntensity - 1 : previousIntensity + 1;

    contribution->traffic_progress_map_.insert({it->first, tp});

    //    if(tp->id_ == 3) {
    //      std::cout << "intensity "<<tp->time_ << " " << previousIntensity << ":"<<tp->intensity_<< std::endl;
    //      std::cout<< "map content ";
    //      for(auto it = contribution->traffic_progress_map_.cbegin(); it != contribution->traffic_progress_map_.cend(); ++it) {
    //        std::cout<< it->second->intensity_ << " ";
    //      }
    //      std::cout<<std::endl;
    //    }
  }

  for (auto& pair : contribution->traffic_progress_map_){
    traffic_progress_map_.insert({pair.first, pair.second});
  }
}

void
stat_vtk::dump_global_data()
{
  //here is where I dump the VTU file or whatever
  std::multimap<int, std::multimap<int, int>> tf_nodes_map;

  for (auto it = traffic_progress_map_.cbegin(); it != traffic_progress_map_.cend(); ++it){
    auto nodeId = it->second->id_;
    auto resIt = tf_nodes_map.find(nodeId);
    if(resIt == tf_nodes_map.cend()){
      auto map = std::multimap<int, int>{};
      map.insert({it->first, it->second->intensity_});

      tf_nodes_map.insert({nodeId, map});
    } else {
      auto &map = resIt->second;
      map.insert({it->first, it->second->intensity_});
    }
  }
  // TORM: display the map
  //  for (auto it = tf_nodes_map.cbegin(); it != tf_nodes_map.cend(); ++it){
  //    auto node_id = it->first;
  //    const auto &map = it->second;
  //    std::cout<<node_id<<":";
  //    for(auto it = map.cbegin(); it != map.cend(); ++it){
  //      std::cout<< it->second << " ";
  //    }
  //    std::cout<<std::endl;
  //  }


#if SSTMAC_VTK_ENABLED
  outputExodusWithSharedMap(fileroot_, traffic_progress_map_, count_x_, count_y_);
#endif
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
