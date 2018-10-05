/*=========================================================================

  Program:   Visualization Toolkit
  Module:    vtkTrafficSource.h

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
/**
 * @class   vtkTimeSource
 * @brief   creates a simple time varying data set.
 *
 * Creates a small easily understood time varying data set for testing.
 * The output is a vtkUntructuredGrid in which the point and cell values vary
 * over time in a sin wave. The analytic ivar controls whether the output
 * corresponds to a step function over time or is continuous.
 * The X and Y Amplitude ivars make the output move in the X and Y directions
 * over time. The Growing ivar makes the number of cells in the output grow
 * and then shrink over time.
*/

#ifndef vtkTrafficSource_h
#define vtkTrafficSource_h

#include "vtkUnstructuredGridAlgorithm.h"

#include "vtkCellArray.h"
#include "vtkIntArray.h"
#include "vtkPoints.h"
#include "vtkSmartPointer.h"
#include "vtk_stats.h"

using namespace sstmac::hw;

class vtkTrafficSource : public vtkUnstructuredGridAlgorithm
{
public:
  static vtkTrafficSource *New();
  vtkTypeMacro(vtkTrafficSource,vtkUnstructuredGridAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  void SetNumberOfSteps(double count);
  void SetSteps(double *steps);

  // Topology
  void SetPoints(vtkSmartPointer<vtkPoints> points);
  void SetCells(vtkSmartPointer<vtkCellArray> cells);

  void SetNumObjects(int num_switches, int num_links){
    link_index_offset_ = num_switches*VTK_NUM_CELLS_PER_SWITCH; //6 faces;
    num_switches_ = num_switches;
    num_links_ = num_links;
    port_intensities_.resize(num_switches);
  }

  void SetGeometries(std::vector<topology::vtk_switch_geometry>&& vec){
    geoms_ = std::move(vec);
  }

  template <class Vec> //allow move or copy
  void SetCellTypes(Vec&& types){
    CellTypes = std::forward<Vec>(types);
  }

  template <class Map> //allow move or copy
  void SetPortLinkMap(Map&& map){
    port_to_link_id_ = std::forward<Map>(map);
  }

  template <class Map>
  void SetLocalToGlobalLinkMap(Map&& map){
    local_to_global_link_id_ = std::forward<Map>(map);
  }

  // Traffic
  void SetTrafficProgressMap(std::multimap<uint64_t, traffic_event>&& trafficProgressMap){
    traffic_progress_map_ = std::move(trafficProgressMap);
  }

  void SetTraffics(vtkSmartPointer<vtkIntArray> traffics,
                   const std::set<int>& special_fills);

  void SetPaintSwitches(uint64_t paintLength);

protected:
  vtkTrafficSource();
  ~vtkTrafficSource() override;

  int RequestInformation(vtkInformation*,
                         vtkInformationVector**,
                         vtkInformationVector*) override;

  int RequestData(vtkInformation*,
                  vtkInformationVector**,
                  vtkInformationVector*) override;

  int NumSteps;
  double *Steps;
  std::multimap<uint64_t, traffic_event> traffic_progress_map_;
  std::unordered_map<uint32_t,int> port_to_link_id_;
  std::unordered_map<uint32_t,uint64_t> local_to_global_link_id_;
  vtkDoubleArray * Traffics;
  vtkSmartPointer<vtkPoints> Points;
  vtkSmartPointer<vtkCellArray> Cells;
  vtkSmartPointer<vtkCellArray> Lines;
  std::vector<int> CellTypes;
  std::vector<topology::vtk_switch_geometry> geoms_;
  int num_switches_;
  int num_links_;
  int link_index_offset_;
  std::set<int> special_fills_;

  std::vector<std::map<int,double>> port_intensities_;


private:
  vtkTrafficSource(const vtkTrafficSource&) = delete;
  void operator=(const vtkTrafficSource&) = delete;
};

#endif

