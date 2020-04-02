/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/
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
{ "min_face_color", "the minimum color to use for active switches"},
{ "max_face_color_sum", "the max color to allow for summation coloring of faces" },
{ "scale_face_color_sum", "" },
{ "active_face_width", "the width fraction of an active face" },
{ "field_name", "" }
);

namespace sstmac {
namespace hw {

static constexpr double link_midpoint_shift = 2.0;

void
outputExodusWithSharedMap(const std::string& fileroot,
   std::multimap<uint64_t, traffic_event>&& trafficMap,
   StatVTK::display_config display_cfg,
   Topology *topo)
{

  // The goal:
  // The idea is to construct the geometry using coordinates of the switch and the coordinates of each ports.
  // Then, we can apply varying time traffic values on ports and switch points.

  // What we know:
  // We assuming the traffic of each port start with 0
  // for each time step (key) of the traffic map parameter, we know the traffic value to update for the (switch,port) associated.
  // The switch traffic value itself needs to be recomputed using the mean of all current traffic values of the ports of the switch.

  int num_switches = topo->numSwitches();
  std::vector<Topology::VTKSwitchGeometry> geoms; geoms.reserve(num_switches);


  std::unordered_map<uint32_t,uint64_t> outport_to_link;
  for (int i=0; i < num_switches; ++i){
    std::vector<Topology::Connection> conns;
    topo->connectedOutports(i, conns);
    for (Topology::Connection& conn : conns){
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

    geoms.push_back(topo->getVtkGeometry(i));
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
    if (iter != outport_to_link.end()){ //not all ports get links
      auto link_iter = port_to_vtk_cell_mapping.find(port_global_id);
      if (link_iter == port_to_vtk_cell_mapping.end()){
        port_to_vtk_cell_mapping[port_global_id] = global_link_id;
        link_to_port_mapping[global_link_id] = port_global_id;
        ++global_link_id;
      }
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

  static constexpr int NUM_POINTS_PER_LINK = 3;
  static constexpr int NUM_POINTS_PER_BOX = 8;
  static constexpr int NUM_BOXES_PER_SWITCH = 7;
  static constexpr int NUM_POINTS_PER_SWITCH = NUM_BOXES_PER_SWITCH * NUM_POINTS_PER_BOX;

  int num_link_points = num_links_to_paint*NUM_POINTS_PER_LINK;

  int num_unique_boxes = 0;
  for (int swid=0; swid < num_switches; ++swid){
    auto geom = topo->getVtkGeometry(swid);
    num_unique_boxes += 1 + geom.ports.size();
  }

  int num_switch_points =  num_unique_boxes * NUM_POINTS_PER_BOX;

  /** Each switch has 8 vertices
   *  Each link has an additional (optional) midpoint defining it as a parabola */
  int num_points = num_switch_points + num_link_points;

  // Create the vtkPoints
  vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
  points->SetNumberOfPoints(num_points);

  std::cout << "vtk_stats : num points = " << num_points << std::endl;



  // Create the vtkCellArray
  // 6 face cell array per switch
  vtkSmartPointer<vtkCellArray> cell_array = vtkSmartPointer<vtkCellArray>::New();
  std::vector<int> cell_types; cell_types.reserve(num_unique_boxes + num_links_to_paint);

  std::vector<int> cell_offsets; cell_offsets.reserve(num_switches + 1);

  int num_boxes = 0;
  auto addBox = [&](Topology::VTKBoxGeometry& box_geom){
    //auto v0 = box_geom.vertex(0); // corner (minus_x, minus_y, minus_z)
    //auto v1 = box_geom.vertex(1); // corner (minus_x, plus_y, minus_z)
    //auto v2 = box_geom.vertex(2); // corner (minus_x, minus_y, plus_z)
    //auto v3 = box_geom.vertex(3); // corner (minus_x, plus_y, plus_z)
    //auto v4 = box_geom.vertex(4); // corner (plus_x, minus_y, minus_z)
    //auto v5 = box_geom.vertex(5); // corner (plus_x, plus_y, minus_z)
    //auto v6 = box_geom.vertex(6); // corner (plus_x, minus_y, plus_z)
    //auto v7 = box_geom.vertex(7); // corner (plus_x, plus_y, plus_z)

    auto v0 = box_geom.vertex(0);
    auto v1 = box_geom.vertex(1);
    auto v2 = box_geom.vertex(3);
    auto v3 = box_geom.vertex(2);
    auto v4 = box_geom.vertex(4);
    auto v5 = box_geom.vertex(5);
    auto v6 = box_geom.vertex(7);
    auto v7 = box_geom.vertex(6);

    int offset = num_boxes * NUM_POINTS_PER_BOX;

    points->SetPoint(offset + 0, v0.x, v0.y, v0.z);
    points->SetPoint(offset + 1, v1.x, v1.y, v1.z);
    points->SetPoint(offset + 2, v2.x, v2.y, v2.z);
    points->SetPoint(offset + 3, v3.x, v3.y, v3.z);
    points->SetPoint(offset + 4, v4.x, v4.y, v4.z);
    points->SetPoint(offset + 5, v5.x, v5.y, v5.z);
    points->SetPoint(offset + 6, v6.x, v6.y, v6.z);
    points->SetPoint(offset + 7, v7.x, v7.y, v7.z);

    vtkSmartPointer<vtkHexahedron> cell = vtkSmartPointer<vtkHexahedron>::New();
    cell->GetPointIds()->SetId(0, offset + 0);
    cell->GetPointIds()->SetId(1, offset + 1);
    cell->GetPointIds()->SetId(2, offset + 2);
    cell->GetPointIds()->SetId(3, offset + 3);
    cell->GetPointIds()->SetId(4, offset + 4);
    cell->GetPointIds()->SetId(5, offset + 5);
    cell->GetPointIds()->SetId(6, offset + 6);
    cell->GetPointIds()->SetId(7, offset + 7);
    cell_array->InsertNextCell(cell);
    cell_types.push_back(VTK_HEXAHEDRON);

    num_boxes++;
  };


  // Use the geometry to place them correctly
  for (int swid = 0; swid < num_switches; ++swid){
    cell_offsets.push_back(cell_types.size());
    auto geom = topo->getVtkGeometry(swid);
    addBox(geom.box);

    for (int port=0; port < geom.ports.size(); ++port){
      auto box = geom.get_port_geometry(port);
      addBox(box);
    }
  }
  cell_offsets.push_back(cell_types.size());

  if (num_switch_points != num_boxes * NUM_POINTS_PER_BOX){
    spkt_abort_printf("Expected switch points %d does not match actual %d",
                      num_switch_points, num_boxes * NUM_POINTS_PER_BOX);
  }

  //need to visit links in order
  for (int link_id=0; link_id < link_to_port_mapping.size(); ++link_id){
    uint32_t port_hash_id = link_to_port_mapping[link_id];
    vtk_port vp = vtk_port::construct(port_hash_id);
    uint64_t link_hash_id = outport_to_link[port_hash_id];
    vtk_link link = vtk_link::construct(link_hash_id);
    Topology::VTKSwitchGeometry switch1 = topo->getVtkGeometry(link.id1);//geoms[link.id1];
    Topology::VTKSwitchGeometry switch2 = topo->getVtkGeometry(link.id2);//geoms[link.id2];
    Topology::VTKBoxGeometry port1 = switch1.get_port_geometry(link.port1);
    Topology::VTKBoxGeometry port2 = switch2.get_port_geometry(link.port2);

    int offset = num_boxes*NUM_POINTS_PER_BOX + link_id * NUM_POINTS_PER_LINK;
    auto c1 = port1.x_anchor();
    auto c2 = port2.x_anchor();
    auto tmp = port1.center();
    //c3 = c1 + N*(c1-tmp)
    auto c3 = c1;
    c3.x += 5.0*(c1.x-tmp.x);
    c3.y += 5.0*(c1.y-tmp.y);
    c3.z += 5.0*(c1.z-tmp.z);

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
    //int link_shift = link.port1 + link.port2;
    //c3.y += link_shift % 2 ? -link_shift*0.05 : link_shift*0.05;

    points->SetPoint(offset + 0, c1.x, c1.y, c1.z);
    points->SetPoint(offset + 1, c2.x, c2.y, c2.z);
    points->SetPoint(offset + 2, c3.x, c3.y, c3.z);
    if (topo->isCurvedVtkLink(link.id1, link.port1)){
      vtkSmartPointer<vtkQuadraticEdge> line = vtkSmartPointer<vtkQuadraticEdge>::New();
      line->GetPointIds()->SetId(0, offset + 0);
      line->GetPointIds()->SetId(1, offset + 1);
      line->GetPointIds()->SetId(2, offset + 2);
      cell_array->InsertNextCell(line); cell_types.push_back(VTK_QUADRATIC_EDGE);
    } else {
      vtkSmartPointer<vtkLine> line = vtkSmartPointer<vtkLine>::New();
      line->GetPointIds()->SetId(0, offset + 0);
      line->GetPointIds()->SetId(1, offset + 1);
      cell_array->InsertNextCell(line); cell_types.push_back(VTK_LINE);
    }
  }

  if (cell_types.size() != cell_array->GetNumberOfCells()){
    spkt_abort_printf("Cell types and array size mismatch: %d != %d",
                      cell_types.size() != cell_array->GetNumberOfCells());
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
  trafficSource->SetNumObjects(num_switches, num_links_to_paint, std::move(cell_offsets));

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
StatVTK::outputExodus(const std::string& fileroot,
    std::multimap<uint64_t, traffic_event>&& traffMap,
    const display_config& cfg,
    Topology *topo)
{
  outputExodusWithSharedMap(fileroot, std::move(traffMap),
                            cfg, topo);
}


StatVTK::StatVTK(SST::Params& params) :
  StatCollector(params), active_(true)
{
  min_interval_ = sstmac::TimeDelta(params.find<SST::UnitAlgebra>("min_interval", "1us").getValue().toDouble());
  display_cfg_.bidirectional_shift = params.find<double>("bidirectional_shift", 0.02);
  display_cfg_.highlight_link_color = params.find<double>("highlight_link_color", 1.0);
  display_cfg_.highlight_switch_color = params.find<double>("highlight_switch_color", 1.0);
  display_cfg_.idle_link_color = params.find<double>("idle_link_color", 0);
  display_cfg_.idle_switch_color = params.find<double>("idle_switch_color", 0.01);
  display_cfg_.min_face_color = params.find<double>("min_face_color", 0);
  display_cfg_.active_face_width = params.find<double>("active_face_width", 0.4);
  display_cfg_.max_face_color_sum = params.find<double>("max_face_color_sum", 1e9);
  display_cfg_.scale_face_color_sum = params.find<double>("scale_face_color_sum", 1.0);
  display_cfg_.name = params.find<std::string>("field_name", "Traffic");
  if (params.contains("intensity_levels")){
    params.find_array("intensity_levels", intensity_levels_);
  }

  flicker_ = params.find<bool>("flicker", true);

  if (params.contains("filter")){
    std::vector<int> filters;
    params.find_array("filter", filters);
    for (int i=0; i < filters.size(); i += 2){
      int start = filters[i];
      int stop = filters[i+1];
      filters_.emplace_back(start, stop);
    }
  }

  if (params.contains("highlight")){
    std::vector<int> fills;
    params.find_array("highlight", fills);
    for (int f : fills) display_cfg_.special_fills.insert(f);
  }
}

void
StatVTK::reduce(StatCollector* element)
{
  StatVTK* contribution = safe_cast(StatVTK, element);

  for (const traffic_event& e : contribution->sorted_event_list_){
    traffic_event_map_.emplace(e.time_, e);
  }

}

void
StatVTK::finalize(TimeDelta t)
{
  if (!active_) return;

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

    if (fabs(port_int.active_vtk_color) > 1e-12){
      spkt_abort_printf("Port %d on VTK %d did not return to zero - got %12.8e",
                        port, id_, port_int.active_vtk_color);
    }
  }

}

void
StatVTK::dumpGlobalData()
{
  outputExodusWithSharedMap(fileroot_, std::move(traffic_event_map_),
                            display_cfg_, Topology::global());
}

void
StatVTK::dumpLocalData()
{
  //if I want a file per node/switch
}

void
StatVTK::clear()
{
}

void
StatVTK::configure(SwitchId sid, Topology *top)
{
  top_ = top;
  auto geom = top_->getVtkGeometry(sid);
  id_ = sid;
  port_states_.resize(geom.ports.size());

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
StatVTK::collect_new_color(TimeDelta time, int port, double color)
{
  if (!active_ || port >= port_states_.size()) return;

  port_state& port_int = port_states_[port];
  TimeDelta interval_length = time - port_int.pending_collection_start;

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
StatVTK::collect_new_intensity(TimeDelta time, int port, double intensity)
{
  if (!active_ || port >= port_states_.size()) return;

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
StatVTK::globalReduce(ParallelRuntime *rt)
{
  if (rt->nproc() > 1){
    sprockit::abort("stat_vtk::global_reduce for MPI parallel");
  }
  //otherwise nothing to do
}

}
}
