#include <sstmac/hardware/vtk/vtk_stats.h>
#include <sstmac/hardware/vtk/vtkTrafficSource.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/hardware/topology/topology.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>

#include <utility>


#if SSTMAC_VTK_ENABLED
#include <vtkInformation.h>
#include <vtkStreamingDemandDrivenPipeline.h>
#include <vtkVersion.h>
#include <vtkSmartPointer.h>
#include <vtkPolyVertex.h>
#include <vtkVertex.h>
#include <vtkQuad.h>
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
#include <vtkCellData.h>
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

struct switchContents {

};

void outputExodusWithSharedMap(const std::multimap<uint64_t, std::shared_ptr<traffic_event>> &trafficMap, int count_x, int count_y, topology *topo){

  // The goal:
  // The idea is to construct the geometry using coordinates of the switch and the coordinates of each ports.
  // Then, we can apply varying time traffic values on ports and switch points.

  // What we know:
  // We assuming the traffic of each port start with 0
  // for each time step (key) of the traffic map parameter, we know the traffic value to update for the (switch,port) associated.
  // The switch traffic value itself needs to be recomputed using the mean of all current traffic values of the ports of the switch.


  // We need to quickly find info associated with the switch
 std::map<int, switchContents> switchIdToContents;
 for(auto it = trafficMap.cbegin(); it != trafficMap.cend(); ++it){
   switchIdToContents.insert(std::make_pair(it->second->id_, switchContents{}));
 }
std::cout << "vtk_stats : num of switch "<< switchIdToContents.size()<<std::endl;
 // store the exact number of switch
 const int numberOfSwitch = switchIdToContents.size();

  // Init traffic array with default 0 traffic value
  vtkSmartPointer<vtkIntArray> traffic =
      vtkSmartPointer<vtkIntArray>::New();
  traffic->SetNumberOfComponents(1);
  traffic->SetName("MyTraffic");
//  int count_xy = count_x * count_y; //
  // 1 color per face and 6 face per switch
  traffic->SetNumberOfValues(numberOfSwitch * 6);
  for (auto i = 0; i < numberOfSwitch; ++i){
    traffic->SetValue(i, 0);
  }

  // Create the vtkPoints
  vtkSmartPointer<vtkPoints> points =
      vtkSmartPointer<vtkPoints>::New();

  // Use the geometry to place them correctly
  for(auto it = switchIdToContents.cbegin(); it != switchIdToContents.cend(); ++it){
    auto geom = topo->get_vtk_geometry(it->first);
    auto box_geom = geom.box;
    auto v0 = box_geom.vertex(0);
    auto v1 = box_geom.vertex(1);
    auto v2 = box_geom.vertex(2);
    auto v3 = box_geom.vertex(3);
    auto v4 = box_geom.vertex(4);
    auto v5 = box_geom.vertex(5);
    auto v6 = box_geom.vertex(6);
    auto v7 = box_geom.vertex(7);
    points->InsertNextPoint(v0.x, v0.y, v0.z);
    points->InsertNextPoint(v1.x, v1.y, v1.z);
    points->InsertNextPoint(v2.x, v2.y, v2.z);
    points->InsertNextPoint(v3.x, v3.y, v3.z);
    points->InsertNextPoint(v4.x, v4.y, v4.z);
    points->InsertNextPoint(v5.x, v5.y, v5.z);
    points->InsertNextPoint(v6.x, v6.y, v6.z);
    points->InsertNextPoint(v7.x, v7.y, v7.z);
  }
  std::cout << "vtk_stats : num of points "<< points->GetNumberOfPoints() <<std::endl;

  // Create the vtkCellArray
  // 6 face cell array per switch
  vtkSmartPointer<vtkCellArray> cell_array =
      vtkSmartPointer<vtkCellArray>::New();

  for (auto i = 0; i < numberOfSwitch; ++i){
    vtkSmartPointer<vtkQuad> plusXface =
        vtkSmartPointer<vtkQuad>::New();
    vtkSmartPointer<vtkQuad> plusYface =
        vtkSmartPointer<vtkQuad>::New();
    vtkSmartPointer<vtkQuad> plusZface =
        vtkSmartPointer<vtkQuad>::New();
    vtkSmartPointer<vtkQuad> minusXface =
        vtkSmartPointer<vtkQuad>::New();
    vtkSmartPointer<vtkQuad> minusYface =
        vtkSmartPointer<vtkQuad>::New();
    vtkSmartPointer<vtkQuad> minusZface =
        vtkSmartPointer<vtkQuad>::New();

    auto pIdxStart = i * 8; // 8 points per switch
    plusXface->GetPointIds()->SetId(0,pIdxStart + 4);
    plusXface->GetPointIds()->SetId(1,pIdxStart + 6);
    plusXface->GetPointIds()->SetId(2,pIdxStart + 7);
    plusXface->GetPointIds()->SetId(3,pIdxStart + 5);

    plusYface->GetPointIds()->SetId(0,pIdxStart + 1);
    plusYface->GetPointIds()->SetId(1,pIdxStart + 5);
    plusYface->GetPointIds()->SetId(2,pIdxStart + 7);
    plusYface->GetPointIds()->SetId(3,pIdxStart + 3);

    plusZface->GetPointIds()->SetId(0,pIdxStart + 6);
    plusZface->GetPointIds()->SetId(1,pIdxStart + 2);
    plusZface->GetPointIds()->SetId(2,pIdxStart + 3);
    plusZface->GetPointIds()->SetId(3,pIdxStart + 7);

    minusXface->GetPointIds()->SetId(0,pIdxStart + 2);
    minusXface->GetPointIds()->SetId(1,pIdxStart);
    minusXface->GetPointIds()->SetId(2,pIdxStart + 1);
    minusXface->GetPointIds()->SetId(3,pIdxStart + 3);

    minusYface->GetPointIds()->SetId(0,pIdxStart + 2);
    minusYface->GetPointIds()->SetId(1,pIdxStart + 6);
    minusYface->GetPointIds()->SetId(2,pIdxStart + 4);
    minusYface->GetPointIds()->SetId(3,pIdxStart);

    minusZface->GetPointIds()->SetId(0,pIdxStart);
    minusZface->GetPointIds()->SetId(1,pIdxStart + 4);
    minusZface->GetPointIds()->SetId(2,pIdxStart + 5);
    minusZface->GetPointIds()->SetId(3,pIdxStart + 1);

    cell_array->InsertNextCell(plusXface);
    cell_array->InsertNextCell(plusYface);
    cell_array->InsertNextCell(plusZface);
    cell_array->InsertNextCell(minusXface);
    cell_array->InsertNextCell(minusYface);
    cell_array->InsertNextCell(minusZface);
  }


  std::cout << "vtk_stats : num of cells "<< cell_array->GetNumberOfCells()<<std::endl;

  vtkSmartPointer<vtkUnstructuredGrid> unstructured_grid =
      vtkSmartPointer<vtkUnstructuredGrid>::New();
  unstructured_grid->SetPoints(points);
  unstructured_grid->SetCells(VTK_QUAD, cell_array);
  unstructured_grid->GetCellData()->AddArray(traffic);

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
  exodusWriter->SetFileName("/Users/perrinel/Dev/sst-macro-jw/build/vtkTrafficSource.e");
  exodusWriter->SetInputConnection (trafficSource->GetOutputPort());
  exodusWriter->WriteAllTimeStepsOn ();
  exodusWriter->Write();

}

void stat_vtk::outputExodus(const std::multimap<uint64_t, traffic_event> &traffMap, int count_x, int count_y, topology *topo) {

  std::cout << " The call back is called ::: outputExodus"<<std::endl;
  std::multimap<uint64_t, std::shared_ptr<traffic_event>> trafficMap;
  for(auto elt : traffMap){
    auto eltSecond =  std::make_shared<traffic_event>(elt.second);
    trafficMap.insert(std::pair<uint64_t, std::shared_ptr<traffic_event>>(elt.first, eltSecond));
  }

  outputExodusWithSharedMap(trafficMap, count_x, count_y, topo);
}


stat_vtk::stat_vtk(sprockit::sim_parameters *params) :
  stat_collector(params)
{
  std::cout << "I got named " << params->get_param("name")
            << " with id "
            << (params->has_param("id") ? params->get_param("id") : "aggregator")
            << std::endl;
  std::cout << "geom " <<params->get_param("count_x") << " "<< params->get_param("count_y") <<std::endl;
  count_x_ = params->has_param("count_x") ? std::stoi(params->get_param("count_x")) : 0;
  count_y_ = params->has_param("count_y") ? std::stoi(params->get_param("count_y")) : 0;
}

void
stat_vtk::reduce(stat_collector* element)
{
  std::cout << "reduce called on switch" << std::endl;
  stat_vtk* contribution = safe_cast(stat_vtk, element);

  std::cout << " reduce " << contribution->traffic_event_map_.size()<<std::endl;

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
  std::cout << "dump global data" << std::endl;

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
  outputExodusWithSharedMap(traffic_progress_map_, count_x_, count_y_, nullptr);
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
