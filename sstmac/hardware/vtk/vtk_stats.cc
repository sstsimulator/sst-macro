#include <sstmac/hardware/vtk/vtk_stats.h>
#include <sstmac/hardware/vtk/vtkTrafficSource.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/hardware/topology/topology.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>

#include <utility>

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

RegisterKeywords(
{ "transition_cutoff", "the port occupancy level that should be considered 'congestion'" },
{ "ignored_time_gap", "a time gap that should be considered negligible" },
);

namespace sstmac {
namespace hw {

struct switchContents {

};

void
outputExodusWithSharedMap(const std::string& fileroot,
   std::multimap<uint64_t, traffic_event>&& trafficMap,
   topology *topo)
{

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
   const traffic_event& e = it->second;
   switchIdToContents.insert(std::make_pair(e.id_, switchContents{}));
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
      time_step_value[currend_index] = it->first;
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
  trafficSource->SetTrafficProgressMap(std::move(trafficMap));
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
    std::multimap<uint64_t, traffic_event>&& traffMap,
    topology *topo) {
  outputExodusWithSharedMap(fileroot, std::move(traffMap), topo);
}


stat_vtk::stat_vtk(sprockit::sim_parameters *params) :
  stat_collector(params)
{
  transition_cutoff_ = params->get_int_param("transition_cutoff");
  ignored_gap_ = params->get_optional_time_param("ignored_time_gap", 1e-3);
}

void
stat_vtk::reduce(stat_collector* element)
{
  stat_vtk* contribution = safe_cast(stat_vtk, element);

  for (traffic_event& e : contribution->event_list_){
    traffic_event_map_.emplace(e.time_, e);
  }

}

void
stat_vtk::finalize(timestamp t)
{
  clear_pending_departures(t);
  for (int face=0; face < face_intensities_.size(); ++face){
    collect_new_level(face, t, 0);
  }
}

void
stat_vtk::dump_global_data()
{
  //here is where I dump the VTU file or whatever
 // std::multimap<int, std::multimap<int, int>> tf_nodes_map;

//  for (auto it = traffic_progress_map_.cbegin(); it != traffic_progress_map_.cend(); ++it){
//    auto nodeId = it->second->id_;
//    auto resIt = tf_nodes_map.find(nodeId);
//    if(resIt == tf_nodes_map.cend()){
//      auto map = std::multimap<int, int>{};
//      map.insert({it->first, it->second->intensity_});

//      tf_nodes_map.insert({nodeId, map});
//    } else {
//      auto &map = resIt->second;
//      map.insert({it->first, it->second->intensity_});
//    }
//  }
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

  //DUMP FOR TRAFFIC
  /**
  std::cout << "StatisticOutputEXODUS::traffic_progress_map_ size  "<< traffic_progress_map_.size()<< std::endl;
  std::multimap<std::string, std::multimap<int, int>> tf_nodes_map;
  for (auto it = traffic_progress_map_.cbegin(); it != traffic_progress_map_.cend(); ++it){
    auto nodeId = it->second->id_;
    auto portId = it->second->p_;
    auto nodIdPortIdKey = std::to_string(nodeId) +":"+ std::to_string(portId);
    auto resIt = tf_nodes_map.find(nodIdPortIdKey);
    if(resIt == tf_nodes_map.cend()){
      auto map = std::multimap<int, int>{};
      map.insert({it->first, it->second->intensity_});

      tf_nodes_map.insert({nodIdPortIdKey, map});
    } else {
      auto &map = resIt->second;
      map.insert({it->first, it->second->intensity_});
    }
  }
  */
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

  outputExodusWithSharedMap(fileroot_, std::move(traffic_event_map_), topology::global());
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
stat_vtk::configure(switch_id sid, topology *top)
{
  top_ = top;
  auto geom = top_->get_vtk_geometry(sid);
  port_to_face_ = geom.port_faces;
  id_ = sid;
  port_intensities_.resize(port_to_face_.size());
  face_intensities_.resize(6);
}

void
stat_vtk::collect_new_level(int face, timestamp time, int level)
{
  face_intensity& face_int = face_intensities_[face];
  timestamp interval_length = time - face_int.pending_collection_start;
  double current_state_length = (time - face_int.last_collection).msec();
  double accumulated_state_length = (face_int.last_collection - face_int.pending_collection_start).msec();
  double total_state_length = current_state_length + accumulated_state_length;
  double new_level = (face_int.current_level * current_state_length
                       + face_int.accumulated_level * accumulated_state_length) / total_state_length;

  if (interval_length > ignored_gap_){
    //we have previous collections that are now large enough  to commit
    event_list_.emplace_back(
      face_int.pending_collection_start.ticks(), face, new_level, id_);
    face_int.pending_collection_start = face_int.last_collection = time;
    face_int.accumulated_level = 0;
    face_int.current_level = level;
  } else {
    face_int.last_collection = time;
    face_int.accumulated_level = new_level;
  }
  face_int.last_collection = time;
}

void
stat_vtk::collect_arrival(timestamp time, int port)
{
  clear_pending_departures(time);

  if (port >= port_intensities_.size()) return;

  auto face = port_to_face_[port];
  face_intensity& face_int = face_intensities_[face];
  int intensity = ++port_intensities_[port];
  if (intensity >= transition_cutoff_){
    face_int.congested_ports++;
  } else {
    face_int.active_ports++;
  }

  int level = 1; //1 means active
  if (face_int.congested_ports > 0){
    level = 2;
  }

  if (level != face_int.current_level){
    collect_new_level(face, time, level);
  }
}

void
stat_vtk::collect_departure(timestamp time, int port)
{
  auto face = port_to_face_[port];
  int intensity = port_intensities_[port]--;
  face_intensity& face_int = face_intensities_[face];
  if (intensity >= transition_cutoff_){
    face_int.congested_ports--;
  } else {
    face_int.active_ports--;
  }

  int level = face_int.active_ports > 0 ? 1 : 0; //1 means active
  if (face_int.congested_ports > 0){
    level = 2;
  }

  if (level != face_int.current_level){
    collect_new_level(face, time, level);
  }

}

void
stat_vtk::clear_pending_departures(timestamp now)
{
  while (!pending_departures_.empty()){
    auto& pair = pending_departures_.top();
    timestamp time = pair.first;
    int port = pair.second;
    if (time > now) return;

    collect_departure(time, port);
    pending_departures_.pop();
  }
}

void
stat_vtk::collect_departure(timestamp now, timestamp time, int port)
{
  clear_pending_departures(now);
  if (port < port_intensities_.size()){
    if (time > now){
      pending_departures_.push(std::make_pair(time,port));
    } else {
      collect_departure(time, port);
    }
  }
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
