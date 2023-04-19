/**
Copyright 2009-2023 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2023, NTESS

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
/*=========================================================================

  Program:   ParaView
  Module:    vtkXMLPVDWriter.h

  Copyright (c) Kitware, Inc.
  All rights reserved.
  See Copyright.txt or http://www.paraview.org/HTML/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
/**
 * @class   vtkXMLPVDWriter
 * @brief   Data writer for ParaView
 *
 * vtkXMLPVDWriter is used to save all parts of a current
 * source to a file with pieces spread across other server processes.
*/

#ifndef vtkXMLPVDWriter_h
#define vtkXMLPVDWriter_h

//#include "vtkPVVTKExtensionsDefaultModule.h" //needed for exports
#include "vtkXMLWriter.h"

class vtkCallbackCommand;
class vtkXMLPVDWriterInternals;

class vtkXMLPVDWriter : public vtkXMLWriter
{
public:
  static vtkXMLPVDWriter* New();
  vtkTypeMacro(vtkXMLPVDWriter, vtkXMLWriter);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  /**
   * Get the default file extension for files written by this writer.
   */
  const char* GetDefaultFileExtension() override;

  //@{
  /**
   * Get/Set the piece number to write.  The same piece number is used
   * for all inputs.
   */
  vtkGetMacro(Piece, int);
  vtkSetMacro(Piece, int);
  //@}

  //@{
  /**
   * Get/Set the number of pieces into which the inputs are split.
   */
  vtkGetMacro(NumberOfPieces, int);
  vtkSetMacro(NumberOfPieces, int);
  //@}

  //@{
  /**
   * Get/Set the number of ghost levels to be written for unstructured
   * data.
   */
  vtkGetMacro(GhostLevel, int);
  vtkSetMacro(GhostLevel, int);
  //@}

  //@{
  /**
   * When WriteAllTimeSteps is turned ON, the writer is executed once for
   * each timestep available from its input. The default is OFF.
   */
  vtkSetMacro(WriteAllTimeSteps, int);
  vtkGetMacro(WriteAllTimeSteps, int);
  vtkBooleanMacro(WriteAllTimeSteps, int);
  //@}

  /**
   * Add an input of this algorithm.
   */
  void AddInputData(vtkDataObject*);

  //@{
  /**
   * Get/Set whether this instance will write the main collection
   * file.
   */
  vtkGetMacro(WriteCollectionFile, int);
  virtual void SetWriteCollectionFile(int flag);
  //@}

  // See the vtkAlgorithm for a description of what these do
  int ProcessRequest(vtkInformation*, vtkInformationVector**, vtkInformationVector*) override;

protected:
  vtkXMLPVDWriter();
  ~vtkXMLPVDWriter() override;

  // see algorithm for more info
  int FillInputPortInformation(int port, vtkInformation* info) override;

  // add in request update extent to set time step information
  virtual int RequestUpdateExtent(vtkInformation*, vtkInformationVector**, vtkInformationVector*);

  // Replace vtkXMLWriter's writing driver method.
  int RequestData(vtkInformation*, vtkInformationVector**, vtkInformationVector*) override;
  int WriteData() override;
  const char* GetDataSetName() override;

  // Methods to create the set of writers matching the set of inputs.
  void CreateWriters();
  vtkXMLWriter* GetWriter(int index);

  // Methods to help construct internal file names.
  void SplitFileName();
  const char* GetFilePrefix();
  const char* GetFilePath();

  // Methods to construct the list of entries for the collection file.
  void AppendEntry(const char* entry);
  void DeleteAllEntries();

  // Write the collection file if it is requested.
  int WriteCollectionFileIfRequested();

  // Make a directory.
  void MakeDirectory(const char* name);

  // Remove a directory.
  void RemoveADirectory(const char* name);

  // Internal implementation details.
  vtkXMLPVDWriterInternals* Internal;

  // The piece number to write.
  int Piece;

  // The number of pieces into which the inputs are split.
  int NumberOfPieces;

  // The number of ghost levels to write for unstructured data.
  int GhostLevel;

  // Whether to write the collection file on this node.
  int WriteCollectionFile;
  int WriteCollectionFileInitialized;

  // Callback registered with the ProgressObserver.
  static void ProgressCallbackFunction(vtkObject*, unsigned long, void*, void*);
  // Progress callback from internal writer.
  virtual void ProgressCallback(vtkAlgorithm* w);

  // The observer to report progress from the internal writer.
  vtkCallbackCommand* ProgressObserver;

  // Garbage collection support.
  void ReportReferences(vtkGarbageCollector*) override;

  // The current time step for time series inputs.
  int CurrentTimeIndex;

  // Option to write all time steps (ON) or just the current one (OFF)
  int WriteAllTimeSteps;

private:
  vtkXMLPVDWriter(const vtkXMLPVDWriter&) = delete;
  void operator=(const vtkXMLPVDWriter&) = delete;
};

#endif
