/*=========================================================================

  Program:   Visualization Toolkit
  Module:    vtkTrafficSource.cxx

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/

#include "vtkTrafficSource.h"

#include "vtkObjectFactory.h"
#include "vtkAlgorithm.h"
#include "vtkAlgorithmOutput.h"
#include "vtkCellData.h"
#include "vtkCellTypes.h"
#include "vtkDoubleArray.h"
#include "vtkIdTypeArray.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkMath.h"
#include "vtkPointData.h"
#include "vtkPoints.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkUnstructuredGrid.h"
#include <vector>

vtkStandardNewMacro(vtkTrafficSource);

static constexpr double idle_switch_intensity = 0.01;

//----------------------------------------------------------------------------
vtkTrafficSource::vtkTrafficSource()
{
  this->SetNumberOfInputPorts(0);
}

//----------------------------------------------------------------------------
vtkTrafficSource::~vtkTrafficSource()
{
}

void vtkTrafficSource::SetNumberOfSteps(double count)
{
  this->NumSteps = count;
}
void vtkTrafficSource::SetSteps(double *steps)
{
  this->Steps = steps;
}

// Topology
void vtkTrafficSource::SetPoints(vtkSmartPointer<vtkPoints> points)
{
  this->Points = points;
}

void vtkTrafficSource::SetCells(vtkSmartPointer<vtkCellArray> cells)
{
  this->Cells = cells;
}

// Traffic
void vtkTrafficSource::SetTraffics(vtkSmartPointer<vtkIntArray> traffics,
                                   const std::set<int>& nonzero_fills)
{
  special_fills_ = nonzero_fills;

  //Send traffic to output
  this->Traffics = vtkDoubleArray::New();
  this->Traffics->SetNumberOfComponents(1);
  this->Traffics->SetName("MyTraffic");
  this->Traffics->SetNumberOfValues(this->Cells->GetNumberOfCells());

  int cell = 0;
  for (int i=0; i < num_switches_; ++i){
    double intensity = idle_switch_intensity;
    if (nonzero_fills.find(i) != nonzero_fills.end()){
      intensity = 5.;
    }
    for (int j=0; j < VTK_NUM_CELLS_PER_SWITCH; ++j, ++cell){
      this->Traffics->SetValue(cell, intensity); //paint switches as always 0.01
      intensity = 0; //only the first one (main box) should start nonzero
    }
  }

  link_index_offset_ = num_switches_ * VTK_NUM_CELLS_PER_SWITCH;
  for (int i=0; i < num_links_; ++i){
    this->Traffics->SetValue(i+link_index_offset_, 0);
  }
}

//----------------------------------------------------------------------------
int vtkTrafficSource::RequestInformation(
  vtkInformation* reqInfo,
  vtkInformationVector** inVector,
  vtkInformationVector* outVector
  )
{
  if(!this->Superclass::RequestInformation(reqInfo,inVector,outVector))
  {
    return 0;
  }

  vtkInformation *info=outVector->GetInformationObject(0);

  //tell the caller that I can provide time varying data and
  //tell it what range of times I can deal with
  double tRange[2];
  tRange[0] = this->Steps[0];
  tRange[1] = this->Steps[this->NumSteps-1];
  info->Set(
    vtkStreamingDemandDrivenPipeline::TIME_RANGE(),
    tRange,
    2);

  //tell the caller if this filter can provide values ONLY at discrete times
  //or anywhere within the time range

  info->Set(
    vtkStreamingDemandDrivenPipeline::TIME_STEPS(),
    this->Steps,
    this->NumSteps);


  info->Set(CAN_HANDLE_PIECE_REQUEST(), 1);

  return 1;
}

//----------------------------------------------------------------------------
int vtkTrafficSource::RequestData(
  vtkInformation* vtkNotUsed(reqInfo),
  vtkInformationVector** vtkNotUsed(inVector),
  vtkInformationVector* outVector
  )
{

  static int timestep = 0;

  vtkInformation *outInfo = outVector->GetInformationObject(0);
  vtkUnstructuredGrid *output= vtkUnstructuredGrid::SafeDownCast(
    outInfo->Get(vtkDataObject::DATA_OBJECT()));
  if (!output)
  {
    return 0;
  }

  uint64_t reqTS(0);
  if (outInfo->Has(vtkStreamingDemandDrivenPipeline::UPDATE_TIME_STEP()))
  {
    double requested = outInfo->Get(vtkStreamingDemandDrivenPipeline::UPDATE_TIME_STEP());
    reqTS = llround(requested);
  }

  //if analytic compute the value at that time
  //TODO: check if it's necessary to look up the nearest time and value from the table
  output->Initialize();
  output->GetInformation()->Set(vtkDataObject::DATA_TIME_STEP(), reqTS);

  //Updade and Send traffic to output
  auto currentIntensities =  traffic_progress_map_.equal_range(reqTS);

  for(auto it = currentIntensities.first; it != currentIntensities.second; ++it){
    // traffic face index = switchId * 6 + getFaceIndex(switchId, port)
    traffic_event& event = it->second;
    vtk_port port(event.id_, event.port_);

    auto iter = port_to_link_id_.find(port.id32());
    if (iter == port_to_link_id_.end()){
      spkt_abort_printf("VTK %d port %d on hash %d has no associated link",
                        event.id_, event.port_, int(port.id32()));
    }
    int link = iter->second;
    vtk_link vl = vtk_link::construct(local_to_global_link_id_[port.id32()]);
    this->Traffics->SetValue(link_index_offset_ + link, event.level_);
    double max_i = idle_switch_intensity;

    /**
    if (special_fills_.find(event.id_) == special_fills_.end()){
      auto& map = port_intensities_[event.id_];
      map[event.port_] = event.level_;

      for (auto& pair : map){
        max_i = std::max(max_i, pair.second);
      }

      int offset = event.id_ * 6;
      for (int i=0; i < 6; ++i){
        this->Traffics->SetValue(offset + i, max_i);
      }
    }

    max_i = idle_switch_intensity;
    if (special_fills_.find(vl.id2) == special_fills_.end()){
      auto& map = port_intensities_[vl.id2];
      map[100*vl.port2] = event.level_;
      for (auto& pair : map){
        max_i = std::max(max_i, pair.second);
      }
      int offset = vl.id2 * 6;
      for (int i=0; i < 6; ++i){
        this->Traffics->SetValue(offset + i, max_i);
      }
    }
    */

    {
      int offset = event.id_ * VTK_NUM_CELLS_PER_SWITCH + 1; //+1 is main box
      int face = geoms_[event.id_].get_face(event.port_);
      this->Traffics->SetValue(offset + face, event.level_);
    }
    {
      int offset = vl.id2 * VTK_NUM_CELLS_PER_SWITCH + 1; //+1 is main box
      int face = geoms_[vl.id2].get_face(vl.port2);
      this->Traffics->SetValue(offset + face, event.level_);
    }


  }

  output->GetCellData()->AddArray(this->Traffics);

  // Send Topology to output
  output->SetPoints(this->Points);

  output->SetCells(CellTypes.data(), this->Cells);

  return 1;
}

//----------------------------------------------------------------------------
void vtkTrafficSource::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);
}
