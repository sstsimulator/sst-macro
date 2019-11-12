
//@HEADER
// ***************************************************
//
// HPCG: High Performance Conjugate Gradient Benchmark
//
// Contact:
// Michael A. Heroux ( maherou@sandia.gov)
// Jack Dongarra     (dongarra@eecs.utk.edu)
// Piotr Luszczek    (luszczek@eecs.utk.edu)
//
// ***************************************************
//@HEADER

/*!
 @file SetupHalo_ref.cpp

 HPCG routine
 */

#ifndef HPCG_NO_MPI
#include <mpi.h>
#include <map>
#include <set>
#endif

#ifndef HPCG_NO_OPENMP
#include <omp.h>
#endif

#ifdef HPCG_DETAILED_DEBUG
#include <fstream>
using std::endl;
#include "hpcg.hpp"
#include <cassert>
#endif

#include "SetupHalo_ref.hpp"
#include "mytimer.hpp"

#ifndef HPCG_NO_MPI
void setupSkeleton(Geometry* geom, std::map<int,int>& sendList, std::map<int,int>& receiveList)
{
  global_int_t nx = geom->nx;
  global_int_t ny = geom->ny;
  global_int_t nz = geom->nz;
  global_int_t npx = geom->npx;
  global_int_t npy = geom->npy;
  global_int_t npz = geom->npz;
  global_int_t ipx = geom->ipx;
  global_int_t ipy = geom->ipy;
  global_int_t ipz = geom->ipz;
  global_int_t gnx = nx*npx;
  global_int_t gny = ny*npy;
  global_int_t gnz = nz*npz;

  global_int_t myZstart = nz*ipz;
  global_int_t myZstop = nz*(ipz+1);
  global_int_t myYstart = ny*ipy;
  global_int_t myYstop = ny*(ipy+1);
  global_int_t myXstart = nx*ipx;
  global_int_t myXstop = nx*(ipx+1);

  std::vector<global_int_t> zFaces; zFaces.reserve(2);
  if (myZstart != 0) zFaces.push_back(myZstart-1);
  if (myZstop < gnz) zFaces.push_back(myZstop+1);

  std::vector<global_int_t> yFaces; yFaces.reserve(2);
  if (myYstart != 0) yFaces.push_back(myYstart-1);
  if (myYstop < gny) yFaces.push_back(myYstop+1);

  std::vector<global_int_t> xFaces; xFaces.reserve(2);
  if (myXstart != 0) xFaces.push_back(myXstart-1);
  if (myXstop < gnx) xFaces.push_back(myXstop+1);

  for (global_int_t giz : zFaces){
    local_int_t ipz = giz/nz;
    global_int_t zRankOffset = ipz*npy*npx;
    for (global_int_t giy=myYstart; giy < myYstop; ++giy){
      local_int_t ipy = giy/ny;
      global_int_t yRankOffset = ipy*npx;
      for (global_int_t gix=myXstart; gix < myXstop; ++gix){
        local_int_t ipx = gix/nx;
        int rank = ipx+zRankOffset+yRankOffset;
        sendList[rank]++;
        receiveList[rank]++;
      }
    }
  }

  for (global_int_t giz=myZstart; giz < myZstop; ++giz){
    local_int_t ipz = giz/nz;
    global_int_t zRankOffset = ipz*npy*npx;
    for (global_int_t giy : yFaces){
      local_int_t ipy = giy/ny;
      global_int_t yRankOffset = ipy*npx;
      for (global_int_t gix=myXstart; gix < myXstop; ++gix){
        local_int_t ipx = gix/nx;
        int rank = ipx+zRankOffset+yRankOffset;
        sendList[rank]++;
        receiveList[rank]++;
      }
    }
  }

  for (global_int_t giz=myZstart; giz < myZstop; ++giz){
    local_int_t ipz = giz/nz;
    global_int_t zRankOffset = ipz*npy*npx;
    for (global_int_t giy=myYstart; giy < myYstop; giy++){
      local_int_t ipy = giy/ny;
      global_int_t yRankOffset = ipy*npx;
      for (global_int_t gix : xFaces){
        local_int_t ipx = gix/nx;
        int rank = ipx+zRankOffset+yRankOffset;
        sendList[rank]++;
        receiveList[rank]++;
      }
    }
  }

  //y and z faces
  for (global_int_t giz : zFaces){
    local_int_t ipz = giz/nz;
    global_int_t zRankOffset = ipz*npy*npx;
    for (global_int_t giy : yFaces){
      local_int_t ipy = giy/ny;
      global_int_t yRankOffset = ipy*npx;
      for (global_int_t gix=myXstart; gix < myXstop; ++gix){
        local_int_t ipx = gix/nx;
        int rank = ipx+zRankOffset+yRankOffset;
        sendList[rank]++;
        receiveList[rank]++;
      }
    }
  }

  //x and z faces
  for (global_int_t giz : zFaces){
    local_int_t ipz = giz/nz;
    global_int_t zRankOffset = ipz*npy*npx;
    for (global_int_t giy=myYstart; giy < myYstop; giy++){
      local_int_t ipy = giy/ny;
      global_int_t yRankOffset = ipy*npx;
      for (global_int_t gix : xFaces){
        local_int_t ipx = gix/nx;
        int rank = ipx+zRankOffset+yRankOffset;
        sendList[rank]++;
        receiveList[rank]++;
      }
    }
  }

  //x and y faces
  for (global_int_t giz=myZstart; giz < myZstop; ++giz){
    local_int_t ipz = giz/nz;
    global_int_t zRankOffset = ipz*npy*npx;
    for (global_int_t giy : yFaces){
      local_int_t ipy = giy/ny;
      global_int_t yRankOffset = ipy*npx;
      for (global_int_t gix : xFaces){
        local_int_t ipx = gix/nx;
        int rank = ipx+zRankOffset+yRankOffset;
        sendList[rank]++;
        receiveList[rank]++;
      }
    }
  }

  //x,y,and z face
  for (global_int_t giz : zFaces){
    local_int_t ipz = giz/nz;
    global_int_t zRankOffset = ipz*npy*npx;
    for (global_int_t giy : yFaces){
      local_int_t ipy = giy/ny;
      global_int_t yRankOffset = ipy*npx;
      for (global_int_t gix : xFaces){
        local_int_t ipx = gix/nx;
        int rank = ipx+zRankOffset+yRankOffset;
        sendList[rank]++;
        receiveList[rank]++;
      }
    }
  }
}

void setupSkeletonNeighbors(int* neighbors, int* sendLength, std::map<int,int>& sendList,
                   int* receiveLength, std::map<int,int>& receiveList)
{
  int index = 0;
  for (auto& pair : sendList){
    neighbors[index] = pair.first;
    sendLength[index] = pair.second;
    ++index;
  }
  index = 0;
  for (auto& pair : receiveList){
    receiveLength[index] = pair.second;
    ++index;
  }
}
#endif


/*!
  Reference version of SetupHalo that prepares system matrix data structure and creates data necessary
  for communication of boundary values of this process.

  @param[inout] A    The known system matrix

  @see ExchangeHalo
*/
void SetupHalo_ref(SparseMatrix & A) {

  // Extract Matrix pieces

  local_int_t localNumberOfRows = A.localNumberOfRows;
#pragma sst null_variable
  char  * nonzerosInRow = A.nonzerosInRow;
#pragma sst null_variable
  global_int_t ** mtxIndG = A.mtxIndG;
#pragma sst null_variable
  local_int_t ** mtxIndL = A.mtxIndL;

#ifdef HPCG_NO_MPI  // In the non-MPI case we simply copy global indices to local index storage
#ifndef HPCG_NO_OPENMP
  #pragma omp parallel for
#endif
  for (local_int_t i=0; i< localNumberOfRows; i++) {
    int cur_nnz = nonzerosInRow[i];
   #pragma sst loop_count 27
    for (int j=0; j<cur_nnz; j++) mtxIndL[i][j] = mtxIndG[i][j];
  }

#else // Run this section if compiling for MPI

  // Scan global IDs of the nonzeros in the matrix.  Determine if the column ID matches a row ID.  If not:
  // 1) We call the ComputeRankOfMatrixRow function, which tells us the rank of the processor owning the row ID.
  //  We need to receive this value of the x vector during the halo exchange.
  // 2) We record our row ID since we know that the other processor will need this value from us, due to symmetry.
#pragma sst null_type std::map<int,int> size
  std::map< int, std::set< global_int_t> > sendList;
#pragma sst null_type std::map<int,int> size
  std::map< int, std::set< global_int_t> > receiveList;
  typedef std::map< int, std::set< global_int_t> >::iterator map_iter;
  typedef std::set<global_int_t>::iterator set_iter;
#pragma sst null_type
  std::map< local_int_t, local_int_t > externalToLocalMap;

  // TODO: With proper critical and atomic regions, this loop could be threaded, but not attempting it at this time
#pragma sst compute
  for (local_int_t i=0; i< localNumberOfRows; i++) {
    global_int_t currentGlobalRow = A.localToGlobalMap[i];
#pragma sst loop_count 27
    for (int j=0; j<nonzerosInRow[i]; j++) {
      global_int_t curIndex = mtxIndG[i][j];
      int rankIdOfColumnEntry = ComputeRankOfMatrixRow(*(A.geom), curIndex);
#ifdef HPCG_DETAILED_DEBUG
      HPCG_fout << "rank, row , col, globalToLocalMap[col] = " << A.geom->rank << " " << currentGlobalRow << " "
          << curIndex << " " << A.globalToLocalMap[curIndex] << endl;
#endif
      if (A.geom->rank!=rankIdOfColumnEntry) {// If column index is not a row index, then it comes from another processor
        receiveList[rankIdOfColumnEntry].insert(curIndex);
        sendList[rankIdOfColumnEntry].insert(currentGlobalRow); // Matrix symmetry means we know the neighbor process wants my value
      }
    }
  }

  // Count number of matrix entries to send and receive
#pragma sst init localNumberOfRows*27
  local_int_t totalToBeSent = 0;
#pragma sst instead setupSkeleton(A.geom,sendList,receiveList);
  for (map_iter curNeighbor = sendList.begin(); curNeighbor != sendList.end(); ++curNeighbor) {
    totalToBeSent += (curNeighbor->second).size();
  }
#pragma sst init localNumberOfRows*27
  local_int_t totalToBeReceived = 0;
#pragma sst delete
  for (map_iter curNeighbor = receiveList.begin(); curNeighbor != receiveList.end(); ++curNeighbor) {
    totalToBeReceived += (curNeighbor->second).size();
  }

#ifdef HPCG_DETAILED_DEBUG
  // These are all attributes that should be true, due to symmetry
  HPCG_fout << "totalToBeSent = " << totalToBeSent << " totalToBeReceived = " << totalToBeReceived << endl;
  assert(totalToBeSent==totalToBeReceived); // Number of sent entry should equal number of received
  assert(sendList.size()==receiveList.size()); // Number of send-to neighbors should equal number of receive-from
  // Each receive-from neighbor should be a send-to neighbor, and send the same number of entries
  for (map_iter curNeighbor = receiveList.begin(); curNeighbor != receiveList.end(); ++curNeighbor) {
    assert(sendList.find(curNeighbor->first)!=sendList.end());
    assert(sendList[curNeighbor->first].size()==receiveList[curNeighbor->first].size());
  }
#endif

  // Build the arrays and lists needed by the ExchangeHalo function.
#pragma sst new
  double * sendBuffer = new double[totalToBeSent];
#pragma sst new
  local_int_t * elementsToSend = new local_int_t[totalToBeSent];
  int * neighbors = new int[sendList.size()];
  local_int_t * receiveLength = new local_int_t[receiveList.size()];
  local_int_t * sendLength = new local_int_t[sendList.size()];
  int neighborCount = 0;
  local_int_t receiveEntryCount = 0;
  local_int_t sendEntryCount = 0;
#pragma sst instead setupSkeletonNeighbors(neighbors, sendLength, sendList, receiveLength, receiveList);
  for (map_iter curNeighbor = receiveList.begin(); curNeighbor != receiveList.end(); ++curNeighbor, ++neighborCount) {
    int neighborId = curNeighbor->first; // rank of current neighbor we are processing
    neighbors[neighborCount] = neighborId; // store rank ID of current neighbor
    receiveLength[neighborCount] = receiveList[neighborId].size();
    sendLength[neighborCount] = sendList[neighborId].size(); // Get count if sends/receives
    for (set_iter i = receiveList[neighborId].begin(); i != receiveList[neighborId].end(); ++i, ++receiveEntryCount) {
      externalToLocalMap[*i] = localNumberOfRows + receiveEntryCount; // The remote columns are indexed at end of internals
    }
    for (set_iter i = sendList[neighborId].begin(); i != sendList[neighborId].end(); ++i, ++sendEntryCount) {
      //if (geom.rank==1) HPCG_fout << "*i, globalToLocalMap[*i], sendEntryCount = " << *i << " " << A.globalToLocalMap[*i] << " " << sendEntryCount << endl;
      elementsToSend[sendEntryCount] = A.globalToLocalMap[*i]; // store local ids of entry to send
    }
  }

  // Convert matrix indices to local IDs
#ifndef HPCG_NO_OPENMP
  #pragma omp parallel for
#endif
  for (local_int_t i=0; i< localNumberOfRows; i++) {
#pragma sst loop_count 27
    for (int j=0; j<nonzerosInRow[i]; j++) {
      global_int_t curIndex = mtxIndG[i][j];
      int rankIdOfColumnEntry = ComputeRankOfMatrixRow(*(A.geom), curIndex);
      if (A.geom->rank==rankIdOfColumnEntry) { // My column index, so convert to local index
        mtxIndL[i][j] = A.globalToLocalMap[curIndex];
      } else { // If column index is not a row index, then it comes from another processor
        mtxIndL[i][j] = externalToLocalMap[curIndex];
      }
    }
  }

  // Store contents in our matrix struct
#pragma sst init 0
  A.numberOfExternalValues = externalToLocalMap.size();
  A.localNumberOfColumns = A.localNumberOfRows + A.numberOfExternalValues;
  A.numberOfSendNeighbors = sendList.size();
  A.totalToBeSent = totalToBeSent;
  A.elementsToSend = elementsToSend;
  A.neighbors = neighbors;
  A.receiveLength = receiveLength;
  A.sendLength = sendLength;
  A.sendBuffer = sendBuffer;

#ifdef HPCG_DETAILED_DEBUG
  HPCG_fout << " For rank " << A.geom->rank << " of " << A.geom->size << ", number of neighbors = " << A.numberOfSendNeighbors << endl;
  for (int i = 0; i < A.numberOfSendNeighbors; i++) {
    HPCG_fout << "     rank " << A.geom->rank << " neighbor " << neighbors[i] << " send/recv length = " << sendLength[i] << "/" << receiveLength[i] << endl;
    for (local_int_t j = 0; j<sendLength[i]; ++j)
      HPCG_fout << "       rank " << A.geom->rank << " elementsToSend[" << j << "] = " << elementsToSend[j] << endl;
  }
#endif

#endif
// ifdef HPCG_NO_MPI

  return;
}
