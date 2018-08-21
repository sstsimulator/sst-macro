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

#if !SSTMAC_INTEGRATED_SST_CORE
using namespace sstmac::hw;
#endif
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

  // Traffic
  void SetTrafficProgressMap(std::multimap<uint64_t, std::shared_ptr<traffic_event>> trafficProgressMap);
  void SetTraffics(vtkSmartPointer<vtkIntArray> traffics);

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
  std::multimap<uint64_t, std::shared_ptr<traffic_event>> traffic_progress_map_;
  vtkIntArray * Traffics;
  vtkSmartPointer<vtkPoints> Points;
  vtkSmartPointer<vtkCellArray> Cells;
private:
  vtkTrafficSource(const vtkTrafficSource&) = delete;
  void operator=(const vtkTrafficSource&) = delete;
};

#endif

