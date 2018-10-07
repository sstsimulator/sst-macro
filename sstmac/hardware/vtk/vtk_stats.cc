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
#include <vtkLine.h>
#include <vtkLagrangeCurve.h>
#include <vtkCubicLine.h>
#include <vtkCellArray.h>
#include <vtkQuadraticEdge.h>
#include <vtkHexahedron.h>
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
{ "intensity_levels", "the port occupancy level that should cause intensity transitions" },
{ "idle_switch_color", "the color value to use for idle switches" },
{ "idle_link_color", "the color value to use for idle links" },
{ "highlight_switch_color", "the color value to use for specially highlighted switches" },
{ "highlight_link_color", "the color value to use for specially highlighed links" },
{ "min_interval", "the minimum time interval to paint as a given color" },
{ "bidirectional_shift", "the displacement in space for each direction of a birectional link" },
{ "filter", "filter on id" },
{ "highlight", "a list of switches to fill with special highlight color"},
{ "collect_delays", "whether to collect packet delays as the intensity"},
{ "flicker", "whether to 'flicker' when packets leave/arrive" },
);

namespace sstmac {
namespace hw {

static constexpr double link_midpoint_shift = 2.0;

void
outputExodusWithSharedMap(const std::string& fileroot,
   std::multimap<uint64_t, traffic_event>&& trafficMap,
   stat_vtk::display_config display_cfg,
   topology *topo)
{

  // The goal:
  // The idea is to construct the geometry using coordinates of the switch and the coordinates of each ports.
  // Then, we can apply varying time traffic values on ports and switch points.

  // What we know:
  // We assuming the traffic of each port start with 0
  // for each time step (key) of the traffic map parameter, we know the traffic value to update for the (switch,port) associated.
  // The switch traffic value itself needs to be recomputed using the mean of all current traffic values of the ports of the switch.

  int num_switches = topo->num_switches();
  std::vector<topology::vtk_switch_geometry> geoms; geoms.reserve(num_switches);


  std::unordered_map<uint32_t,uint64_t> outport_to_link;
  for (int i=0; i < num_switches; ++i){
    std::vector<topology::connection> conns;
    topo->connected_outports(i, conns);
    for (topology::connection& conn : conns){
      uint16_t id1 = conn.src;
      uint16_t id2 = conn.dst;
      uint16_t port1 = conn.src_outport;
      uint16_t port2 = conn.dst_inport;
      vtk_link link(id1,port1,id2,port2);
      vtk_port port(id1,port1);
      outport_to_link[port.id32()] = link.id64();

      vtk_link test_link = vtk_link::construct(link.id64());
      if (   test_link.id1 != link.id1 || test_link.id2 != link.id2
          || test_link.port1 != link.port1 || test_link.port2 != link.port2){
        spkt_abort_printf("Bad link bit arithmetic: link(%d,%d,%d,%d) != link(%d,%d,%d,%d)",
                          int(link.id1), int(link.port1), int(link.id2), int(link.port2),
                          int(test_link.id1), int(test_link.port1), int(test_link.id2), int(test_link.port2));
      }
    }

    geoms.push_back(topo->get_vtk_geometry(i));
  }

  int global_link_id = 0;
  std::unordered_map<uint32_t,int> port_to_vtk_cell_mapping;
  std::unordered_map<int,uint32_t> link_to_port_mapping;
  for (auto& pair : trafficMap){
    traffic_event& e = pair.second;
    vtk_port p(e.id_, e.port_);
    vtk_port test_p = vtk_port::construct(p.id32());
    if (test_p.id != p.id || test_p.port != p.port){
      spkt_abort_printf("Bad port bit arithmetic: port(%d,%d) != port(%d,%d)",
                        int(p.id), int(p.port), int(test_p.id), int(test_p.port));
    }

    uint32_t port_global_id = p.id32();
    auto iter = outport_to_link.find(port_global_id);
    if (iter == outport_to_link.end()){
      spkt_abort_printf("Could not find link for global port %d = %d x %d",
                        int(port_global_id), int(e.id_), int(e.port_));
    }

    auto link_iter = port_to_vtk_cell_mapping.find(port_global_id);
    if (link_iter == port_to_vtk_cell_mapping.end()){
      port_to_vtk_cell_mapping[port_global_id] = global_link_id;
      link_to_port_mapping[global_link_id] = port_global_id;
      ++global_link_id;
    }
  }
  std::cout << "vtk_stats : num links=" << port_to_vtk_cell_mapping.size() << std::endl;
  std::cout << "vtk_stats : num switches=" << num_switches << std::endl;

  //if a link is never used in the simulation, don't draw it
  //limit the visualization to only those links that actually matter
  int num_links_to_paint = port_to_vtk_cell_mapping.size();


  // Init traffic array with default 0 traffic value
  vtkSmartPointer<vtkIntArray> traffic = vtkSmartPointer<vtkIntArray>::New();
  traffic->SetNumberOfComponents(1);
  traffic->SetName("MyTraffic");
  //  int count_xy = count_x * count_y; //
  // 1 color per face and 6 face per switch
  int numTrafficValues = num_switches*6 + num_links_to_paint;
  traffic->SetNumberOfValues(numTrafficValues);
  //for (auto i = 0; i < numTrafficValues; ++i){
  //  traffic->SetValue(i, 0);
  //}

  static constexpr int NUM_POINTS_PER_LINK = 3;
  static constexpr int NUM_POINTS_PER_SWITCH = 56; //8 for switch, 8x6 for faces

  int num_switch_points = num_switches*NUM_POINTS_PER_SWITCH;
  int num_link_points = num_links_to_paint*NUM_POINTS_PER_LINK;

  /** Each switch has 8 vertices
   *  Each link has an additional (optional) midpoint defining it as a parabola */
  int numPoints = num_switch_points + num_link_points;

  // Create the vtkPoints
  vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
  points->SetNumberOfPoints(numPoints);

  // Create the vtkCellArray
  // 6 face cell array per switch
  vtkSmartPointer<vtkCellArray> cell_array = vtkSmartPointer<vtkCellArray>::New();
  std::vector<int> cell_types; cell_types.reserve(num_switches*7 + num_links_to_paint);

  int idx = 0;
  auto addBox = [&](topology::vtk_box_geometry& box_geom){
    auto v0 = box_geom.vertex(0); // corner (minus_x, minus_y, minus_z)
    auto v1 = box_geom.vertex(1); // corner (minus_x, plus_y, minus_z)
    auto v2 = box_geom.vertex(2); // corner (minus_x, minus_y, plus_z)
    auto v3 = box_geom.vertex(3); // corner (minus_x, plus_y, plus_z)
    auto v4 = box_geom.vertex(4); // corner (plus_x, minus_y, minus_z)
    auto v5 = box_geom.vertex(5); // corner (plus_x, plus_y, minus_z)
    auto v6 = box_geom.vertex(6); // corner (plus_x, minus_y, plus_z)
    auto v7 = box_geom.vertex(7); // corner (plus_x, plus_y, plus_z)
    points->SetPoint(idx + 0, v0.x, v0.y, v0.z);
    points->SetPoint(idx + 1, v1.x, v1.y, v1.z);
    points->SetPoint(idx + 2, v2.x, v2.y, v2.z);
    points->SetPoint(idx + 3, v3.x, v3.y, v3.z);
    points->SetPoint(idx + 4, v4.x, v4.y, v4.z);
    points->SetPoint(idx + 5, v5.x, v5.y, v5.z);
    points->SetPoint(idx + 6, v6.x, v6.y, v6.z);
    points->SetPoint(idx + 7, v7.x, v7.y, v7.z);

    vtkSmartPointer<vtkHexahedron> cell = vtkSmartPointer<vtkHexahedron>::New();
    cell->GetPointIds()->SetId(0, idx + 0);
    cell->GetPointIds()->SetId(1, idx + 1);
    cell->GetPointIds()->SetId(2, idx + 2);
    cell->GetPointIds()->SetId(3, idx + 3);
    cell->GetPointIds()->SetId(4, idx + 4);
    cell->GetPointIds()->SetId(5, idx + 5);
    cell->GetPointIds()->SetId(6, idx + 6);
    cell->GetPointIds()->SetId(7, idx + 7);
    idx += 8;

    cell_array->InsertNextCell(cell);
    cell_types.push_back(VTK_HEXAHEDRON);
  };


  // Use the geometry to place them correctly

  for (int swid = 0; swid < num_switches; ++swid){
    auto geom = topo->get_vtk_geometry(swid);
    addBox(geom.box);

    for (int face=0; face < 6; ++face){
      auto box = geom.box.get_face_geometry(topology::vtk_face_t(face), 0.3);
      addBox(box);
    }
  }


  //need to visit links in order
  for (int link_id=0; link_id < link_to_port_mapping.size(); ++link_id){
    uint32_t port_hash_id = link_to_port_mapping[link_id];
    vtk_port vp = vtk_port::construct(port_hash_id);
    uint64_t link_hash_id = outport_to_link[port_hash_id];
    vtk_link link = vtk_link::construct(link_hash_id);
    topology::vtk_switch_geometry switch1 = topo->get_vtk_geometry(link.id1);//geoms[link.id1];
    topology::vtk_switch_geometry switch2 = topo->get_vtk_geometry(link.id2);//geoms[link.id2];
    topology::vtk_box_geometry face1 = switch1.face_on_port(link.port1, 0.0);
    topology::vtk_box_geometry face2 = switch2.face_on_port(link.port2, 0.0);
    topology::vtk_box_geometry face3 = link.id1 > link.id2
          ? switch1.face_on_port(link.port1, -link_midpoint_shift)
          : switch2.face_on_port(link.port2, -link_midpoint_shift);
    int idx = num_switch_points + link_id * NUM_POINTS_PER_LINK;
    auto c1 = face1.center();
    auto c2 = face2.center();
    auto c3 = face3.center();

    if (link.id1 != vp.id){
      spkt_abort_printf("Port thinks switch id is %d, but link thinks switch id is %d",
                        int(vp.id), int(link.id1));
    }

    //links are bidirectional - which means two links will get painted on top of each other
    //for now (for lack of a better way) shift +/- on the z-axis to avoid conflicts
    double z_shift = link.id1 > link.id2 ? display_cfg.bidirectional_shift : -display_cfg.bidirectional_shift;
    c1.z += z_shift;
    c2.z += z_shift;
    /** move this quite far out of the plane for clarity */
    int link_shift = link.port1 + link.port2;
    c3.z += link_shift % 2 ? -link_shift*0.05 : link_shift*0.05;

    points->SetPoint(idx + 0, c1.x, c1.y, c1.z);
    points->SetPoint(idx + 1, c2.x, c2.y, c2.z);
    points->SetPoint(idx + 2, c3.x, c3.y, c3.z);

    if (topo->is_curved_vtk_link(link.id1, link.port1)){
      vtkSmartPointer<vtkQuadraticEdge> line = vtkSmartPointer<vtkQuadraticEdge>::New();
      line->GetPointIds()->SetId(0, idx + 0);
      line->GetPointIds()->SetId(1, idx + 1);
      line->GetPointIds()->SetId(2, idx + 2);
      cell_array->InsertNextCell(line); cell_types.push_back(VTK_QUADRATIC_EDGE);
    } else {
      vtkSmartPointer<vtkLine> line = vtkSmartPointer<vtkLine>::New();
      line->GetPointIds()->SetId(0, idx + 0);
      line->GetPointIds()->SetId(1, idx + 1);
      cell_array->InsertNextCell(line); cell_types.push_back(VTK_LINE);
    }
  }

  std::cout << "vtk_stats : num cells=" << cell_array->GetNumberOfCells()<<std::endl;
  std::cout << "vtk_stats : num events=" << trafficMap.size() << std::endl;

  vtkSmartPointer<vtkUnstructuredGrid> unstructured_grid =
      vtkSmartPointer<vtkUnstructuredGrid>::New();
  unstructured_grid->SetPoints(points);
  unstructured_grid->SetCells(cell_types.data(), cell_array);
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

  vtkSmartPointer<vtkTrafficSource> trafficSource = vtkSmartPointer<vtkTrafficSource>::New();
  trafficSource->SetDisplayParameters(display_cfg);
  trafficSource->SetNumObjects(num_switches, num_links_to_paint);
  trafficSource->SetSteps(time_step_value);
  trafficSource->SetNumberOfSteps(currend_index);
  trafficSource->SetPoints(points);
  trafficSource->SetGeometries(std::move(geoms));
  trafficSource->SetCells(cell_array);
  trafficSource->SetCellTypes(std::move(cell_types));
  trafficSource->SetTraffics(traffic);
  trafficSource->SetPortLinkMap(std::move(port_to_vtk_cell_mapping));
  trafficSource->SetLocalToGlobalLinkMap(std::move(outport_to_link));
  trafficSource->SetTrafficProgressMap(std::move(trafficMap));
  vtkSmartPointer<vtkExodusIIWriter> exodusWriter = vtkSmartPointer<vtkExodusIIWriter>::New();
  std::string fileName = fileroot + ".e";
  exodusWriter->SetFileName(fileName.c_str());
  exodusWriter->SetInputConnection (trafficSource->GetOutputPort());
  exodusWriter->WriteAllTimeStepsOn ();
  exodusWriter->Write();

}

void
stat_vtk::outputExodus(const std::string& fileroot,
    std::multimap<uint64_t, traffic_event>&& traffMap,
    const display_config& cfg,
    topology *topo)
{
  outputExodusWithSharedMap(fileroot, std::move(traffMap),
                            cfg, topo);
}


stat_vtk::stat_vtk(sprockit::sim_parameters *params) :
  stat_collector(params), active_(true)
{
  min_interval_ = params->get_optional_time_param("min_interval", 1e-6);
  display_cfg_.bidirectional_shift = params->get_optional_double_param("bidirectional_shift", 0.02);
  display_cfg_.highlight_link_color = params->get_optional_double_param("highlight_link_color", 1.0);
  display_cfg_.highlight_switch_color = params->get_optional_double_param("highlight_switch_color", 1.0);
  display_cfg_.idle_link_color = params->get_optional_double_param("idle_link_color", 0);
  display_cfg_.idle_switch_color = params->get_optional_double_param("idle_switch_color", 0.01);
  params->get_vector_param("intensity_levels", intensity_levels_);
  collect_delays_ = params->get_optional_bool_param("collect_delays", true);
  flicker_ = params->get_optional_bool_param("flicker", true);

  if (params->has_param("filter")){
    std::vector<int> filters;
    params->get_vector_param("filter", filters);
    for (int i=0; i < filters.size(); i += 2){
      int start = filters[i];
      int stop = filters[i+1];
      filters_.emplace_back(start, stop);
    }
  }

  if (params->has_param("highlight")){
    std::vector<int> fills;
    params->get_vector_param("highlight", fills);
    for (int f : fills) display_cfg_.special_fills.insert(f);
  }
}

void
stat_vtk::reduce(stat_collector* element)
{
  stat_vtk* contribution = safe_cast(stat_vtk, element);

  for (const traffic_event& e : contribution->sorted_event_list_){
    traffic_event_map_.emplace(e.time_, e);
  }

}

void
stat_vtk::finalize(timestamp t)
{
  if (!active_) return;

  clear_pending_departures(t);
  min_interval_ = 0;
  for (int port=0; port < port_states_.size(); ++port){
    if (port_states_[port].current_level != 0){
      spkt_abort_printf("Port %d on VTK %d did not return to zero", port, id_);
    }
    auto& port_int = port_states_[port];
    //some ports may have collected nothing
    if (port_int.last_collection.ticks() != 0){
      collect_new_color(t, port, 0);
    }

    if (port_int.active_vtk_color != 0){
      spkt_abort_printf("Port %d on VTK %d did not return to zero", port, id_);
    }

    if (!port_int.delayed.empty()){
      spkt_abort_printf("Port %d on VTK %d did not clear all delayed packets", port, id_);
    }
  }

}

void
stat_vtk::dump_global_data()
{
  outputExodusWithSharedMap(fileroot_, std::move(traffic_event_map_),
                            display_cfg_, topology::global());
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
  raw_port_intensities_.resize(port_to_face_.size());
  port_states_.resize(port_to_face_.size());
  //face_intensities_.resize(6);

  if (!filters_.empty()){
    active_ = false;
    for (auto& pair : filters_){
      if (pair.first <= id_ && id_ <= pair.second){
        active_ = true;
        break;
      }
    }
  }
}

void
stat_vtk::collect_new_color(timestamp time, int port, double color)
{
  port_state& port_int = port_states_[port];
  timestamp interval_length = time - port_int.pending_collection_start;

  if (min_interval_.ticks() == 0){
    //log all events - no aggregation or averaging
    auto pair = sorted_event_list_.emplace(time.ticks(), port, color, id_);
    if (!pair.second){
      //we got blocked! overwrite their value
      auto& e = *pair.first;
      e.color_ = color;
    }
    port_int.active_vtk_color = color;
    return;
  }

  double new_color = color;
  if (time != port_int.pending_collection_start){ //otherwise this will NaN
    double current_state_length = (time - port_int.last_collection).msec();
    double accumulated_state_length = (port_int.last_collection - port_int.pending_collection_start).msec();
    double total_state_length = (time - port_int.pending_collection_start).msec();
    new_color = (port_int.current_color * current_state_length
                  + port_int.accumulated_color * accumulated_state_length) / total_state_length;
  }

  if (interval_length >= min_interval_){
    //we have previous collections that are now large enough  to commit
    if (port_int.pending_collection_start.ticks() == 0){
      //we need this to check to avoid having non-zero state accidentally at the beginning
      sorted_event_list_.emplace(port_int.last_collection.ticks(),
                                 port, new_color, id_);
    } else {
      sorted_event_list_.emplace(port_int.pending_collection_start.ticks(),
                                 port, new_color, id_);
    }
    port_int.pending_collection_start = port_int.last_collection = time;
    port_int.accumulated_color = 0;
    port_int.active_vtk_color = new_color;
  } else {
    port_int.last_collection = time;
    port_int.accumulated_color = new_color;
  }
}

void
stat_vtk::collect_new_intensity(timestamp time, int port, double intensity)
{
  auto& port_int = port_states_[port];
  int level = 0;
  for (int i=intensity_levels_.size()-1; i >=0; --i){
    if (intensity >= intensity_levels_[i]){
      level = i+1;
      break;
    }
  }

  if (level != port_int.current_level || port_int.active_vtk_color != double(level)){
    collect_new_color(time, port, level);
    port_int.current_level = level;
  }
}

void
stat_vtk::increment_intensity(timestamp time, int port, int increment, const char* type)
{
  raw_port_intensities_[port] += increment;
  int intensity = raw_port_intensities_[port];
  collect_new_intensity(time, port, intensity);
}

void
stat_vtk::collect_arrival(timestamp time, int port, intptr_t id)
{
  if (!active_) return;

  clear_pending_departures(time);

  if (port >= port_states_.size()) return;

  if (collect_delays_){
    auto& port_int = port_states_[port];
    port_int.delayed[id] = time;
  } else {
    increment_intensity(time, port, 1, "arrive");
  }
}

void
stat_vtk::collect_departure(timestamp time, int port, intptr_t id)
{
  static const timestamp min_time_gap(5e-9);
  if (collect_delays_){
    auto& port_int = port_states_[port];
    auto iter = port_int.delayed.find(id);
    if (iter == port_int.delayed.end()){
      spkt_abort_printf("Switch %d port %d has no previous events matching id %llu",
                        id_, port, id);
    }
    timestamp arrival = iter->second;
    port_int.delayed.erase(iter);
    timestamp time_start_waiting = arrival;
    if (port_int.last_wait_finished > arrival){
      time_start_waiting = port_int.last_wait_finished + min_time_gap;
    }
    timestamp delay = time - arrival;

    collect_new_color(time_start_waiting, port, delay.usec());

    port_int.last_wait_finished = time;
    if (flicker_){
      //cause a flicker after each packet
      collect_new_color(time - min_time_gap, port, 0);
    }
  } else {
    increment_intensity(time, port, -1, "depart");
  }
}

void
stat_vtk::clear_pending_departures(timestamp now)
{
  while (!pending_departures_.empty()){
    const delayed_event& e = pending_departures_.top();
    if (e.time > now) return;

    collect_departure(e.time, e.port, e.id);
    pending_departures_.pop();
  }
}

void
stat_vtk::collect_departure(timestamp now, timestamp time, int port, intptr_t id)
{
  if (!active_) return;

  clear_pending_departures(now);
  if (port < port_states_.size()){
    if (time > now){
      pending_departures_.emplace(time, port, id);
    } else {
      collect_departure(time, port, id);
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
