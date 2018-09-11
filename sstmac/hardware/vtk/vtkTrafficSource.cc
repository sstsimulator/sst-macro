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
void vtkTrafficSource::SetTraffics(vtkSmartPointer<vtkIntArray> traffics)
{
  //Send traffic to output
  this->Traffics = vtkIntArray::New();
  this->Traffics->SetNumberOfComponents(1);
  this->Traffics->SetName("MyTraffic");
  this->Traffics->SetNumberOfValues(this->Cells->GetNumberOfCells());

  for(auto i = 0; i < this->Cells->GetNumberOfCells(); ++i){
    this->Traffics->SetValue(i, 0);
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
  vtkInformation *outInfo = outVector->GetInformationObject(0);
  vtkUnstructuredGrid *output= vtkUnstructuredGrid::SafeDownCast(
    outInfo->Get(vtkDataObject::DATA_OBJECT()));
  if (!output)
  {
    return 0;
  }

  double reqTS(0);
  if (outInfo->Has(vtkStreamingDemandDrivenPipeline::UPDATE_TIME_STEP()))
  {
    reqTS = outInfo->Get
      (vtkStreamingDemandDrivenPipeline::UPDATE_TIME_STEP());
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
    auto valueIndex = event.id_ * 6 + event.face_;
    this->Traffics->SetValue(valueIndex, event.intensity_);
  }

  output->GetCellData()->AddArray(this->Traffics);

  // Send Topology to output
  vtkPoints *points = vtkPoints::New();
  points->DeepCopy(this->Points);
  output->SetPoints(points);
  points->Delete();

  vtkCellArray *cells = vtkCellArray::New();
  cells->DeepCopy(this->Cells);
  output->SetCells(VTK_QUAD, this->Cells);
  cells->Delete();

  return 1;
}

//----------------------------------------------------------------------------
void vtkTrafficSource::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);
}
