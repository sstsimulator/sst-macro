#include <sstmac/hardware/vtk/vtk_stats.h>
#include <sstmac/hardware/vtk/vtkXMLPVDWriter.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>


#if SSTMAC_VTK_ENABLED
#include <vtkInformation.h>
#include <vtkStreamingDemandDrivenPipeline.h>
#include <vtkVersion.h>
#include <vtkSmartPointer.h>
#include <vtkPolyVertex.h>
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
#endif

RegisterKeywords(
{ "name","a test parameter" },
{ "count_x", "topology size in x dim"},
{ "count_y", "topology size in y dim"},
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
    auto tp = std::make_shared<traffic_progress>();
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
  // display the map

  for (auto it = tf_nodes_map.cbegin(); it != tf_nodes_map.cend(); ++it){
    auto node_id = it->first;
    const auto &map = it->second;
    std::cout<<node_id<<":";
    for(auto it = map.cbegin(); it != map.cend(); ++it){
      std::cout<< it->second << " ";
    }
    std::cout<<std::endl;
  }


#if SSTMAC_VTK_ENABLED
  // Init traffic value
  vtkSmartPointer<vtkIntArray> traffic =
      vtkSmartPointer<vtkIntArray>::New();
  traffic->SetNumberOfComponents(1);
  traffic->SetName("MyTraffic");
  int count_xy = count_x_ * count_y_;
  traffic->SetNumberOfValues(count_xy);
  for (auto i = 0; i < count_xy; ++i){
    traffic->SetValue(i, 0);
  }

  // Create the vtu file
  vtkSmartPointer<vtkPoints> points =
      vtkSmartPointer<vtkPoints>::New();

  for (auto j = 0; j < count_y_; ++j){
    for (auto i = 0; i < count_x_; ++i){
      points->InsertNextPoint(i, j, 0);
    }
  }

  vtkSmartPointer<vtkPolyVertex> points_data =
      vtkSmartPointer<vtkPolyVertex>::New();
  points_data->GetPointIds()->SetNumberOfIds(count_xy);
  for (auto i = 0; i < count_xy; ++i){
    points_data->GetPointIds()->SetId(i,i);
  }

  vtkSmartPointer<vtkCellArray> cell_array =
      vtkSmartPointer<vtkCellArray>::New();
  cell_array->InsertNextCell(points_data);

  vtkSmartPointer<vtkUnstructuredGrid> unstructured_grid =
      vtkSmartPointer<vtkUnstructuredGrid>::New();
  unstructured_grid->SetPoints(points);
  unstructured_grid->SetCells(VTK_VERTEX, cell_array);
  unstructured_grid->GetPointData()->AddArray(traffic);

  // Init Time Step
  double current_time = -1;
  double *time_step_value = new double[traffic_progress_map_.size() + 1];
  time_step_value[0] = 0.;
  int currend_index = 1;
  for (auto it = traffic_progress_map_.cbegin(); it != traffic_progress_map_.cend(); ++it){
    if (it->first != current_time){
      current_time = it->first;
      time_step_value[currend_index] =it->first;
      ++currend_index;
    }
  }

  // write the PVD
  vtkSmartPointer<vtkXMLPVDWriter> pdw_writer =
      vtkSmartPointer<vtkXMLPVDWriter>::New();
  pdw_writer->SetFileName("/Users/perrinel/Dev/sst-macro-jw/build/vtkStats.pvd");
  pdw_writer->SetByteOrderToLittleEndian();
  pdw_writer->SetCompressorTypeToNone();
  // pdw_writer->AddInputDataObject(unstructured_grid);
  pdw_writer->SetInputData(0, unstructured_grid);
  pdw_writer->SetDataModeToAscii();
  pdw_writer->GetInputInformation()->Set(vtkStreamingDemandDrivenPipeline::TIME_STEPS(), &time_step_value[0], currend_index);
  pdw_writer->WriteAllTimeStepsOn();
  //   pdw_writer->Update();
  pdw_writer->Write();

  // Time is different we need to create a file
  // Write file
  vtkSmartPointer<vtkXMLUnstructuredGridWriter> writer =
      vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
  writer->SetByteOrderToLittleEndian();
  writer->SetCompressorTypeToNone();
  writer->SetDataModeToAscii();
  writer->SetInputData(0, unstructured_grid);

  currend_index = 0;
  auto it = traffic_progress_map_.cbegin();
  current_time = -1;
  for (; it != traffic_progress_map_.cend(); ++it){
    if (it->first != current_time){
      std::string file_name = "/Users/perrinel/Dev/sst-macro-jw/build/vtkStats/vtkStats_0_";
      file_name += std::to_string(currend_index);
      file_name += ".vtu";
      writer->SetFileName(file_name.c_str());

      writer->Write();

      // update index
      ++currend_index;
    }

    current_time = it->first;
    traffic->SetValue(it->second->id_, it->second->intensity_);
    //  std::cout << std::to_string(current_time) << ": " << it->second->id_ << ": "<< it->second->intensity_<< ";" <<std::endl;
  }
  if(it == traffic_progress_map_.cend()){
    std::string file_name = "/Users/perrinel/Dev/sst-macro-jw/build/vtkStats/vtkStats_0_";
    file_name += std::to_string(currend_index);
    file_name += ".vtu";
    writer->SetFileName(file_name.c_str());
    writer->Update();
    writer->Write();
  }

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
