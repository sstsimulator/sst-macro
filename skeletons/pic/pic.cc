#include <vector>
#include <cstdlib>
#include <mpi.h>
#include <sys/time.h>
#include <cstring>
#include <iostream>
#include "pic.h"

void backfill(Patch& patch)
{
  //stub - to be filled in
}

//this is never actually usable
#pragma sst delete
void moveParticle(int idx, Particle& part, Patch& patch)
{
#pragma sst loop_count 2
  while (part.deltaT > 0){
    int minFace = 0;
    double minDeltaT = part.deltaT;
    //find intersection with each face
    Cell& cell = patch.cells[part.cell];
#pragma sst loop_count 6 //6 faces
    for (int f=0; f < cell.faces.size(); ++f){
      Face& face = cell.faces[f];
      double vecComponent = dot(face.n, part.v);
#pragma sst branch_predict 0.5 //this will be true on half of the faces
      if (vecComponent > 0){
        //great - will hit this face
        double deltaX[3];
        deltaX[0] = face.x[0] - part.x[0];
        deltaX[1] = face.x[1] - part.x[1];
        deltaX[2] = face.x[2] - part.x[2];
        double distance = dot(deltaX, face.n);
        double tIntersect = distance / vecComponent;
        double tMove = std::min(tIntersect, part.deltaT);
        if (tMove < minDeltaT){
          minFace = f;
          minDeltaT = tMove;
        }
      }
    }
    Face& dstFace = cell.faces[minFace];
    part.cell = dstFace.dstCell;
    if (dstFace.dstRank >= 0){
      patch.outgoing[dstFace.dstRank].parts.push_back(part);
      patch.holes.push_back(idx);
    }
  }
}


int exchange(Patch& patch, int substep)
{
  std::vector<int> numSending(patch.outgoing.size(), 0);
  std::vector<int> numRecving(patch.incoming.size(), 0);
  std::vector<MPI_Request> sizeRequests(patch.outgoing.size() + patch.incoming.size());
  std::vector<MPI_Request> partRequests;
  MPI_Request collectiveReq;
  int totalOutgoing = 0;
  int systemTotal = 0;
  for (int f=0; f < patch.outgoing.size(); ++f){
    Migration& m = patch.outgoing[f];
    numSending[f] = m.parts.size();
    totalOutgoing += m.parts.size();
#pragma sst keep
    MPI_Isend(&numSending[f], 1, MPI_INT, m.rank, send_size_tag,
              MPI_COMM_WORLD, &sizeRequests[f]);
    debug("Rank %d sending %d to %d on face %c%c\n",
           patch.id, numSending[f], m.rank, outChar(f), dimChar(f));
    if (numSending[f] > 0){
      MPI_Request req;
      MPI_Isend(m.parts.data(), m.parts.size() * sizeof(Particle), MPI_BYTE,
                m.rank, send_parts_tag, MPI_COMM_WORLD, &req);
      partRequests.push_back(req);
    }
  }

  for (int f=0; f < patch.incoming.size(); ++f){
    Migration& m = patch.incoming[f];
#pragma sst keep
    MPI_Irecv(&numRecving[f], 1, MPI_INT, m.rank, send_size_tag,
              MPI_COMM_WORLD, &sizeRequests[f + patch.outgoing.size()]);
  }

  
  MPI_Iallreduce(&totalOutgoing, &systemTotal, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD, &collectiveReq);

  int numDone = 0;
  int numNeeded = patch.incoming.size() + patch.outgoing.size();
  while (numDone < numNeeded){
    int idx;
    MPI_Waitany(numNeeded, sizeRequests.data(), &idx, MPI_STATUSES_IGNORE);
    if (idx >= patch.outgoing.size()){
      int recvIdx = idx - patch.outgoing.size();
      //we received the size of a particle we need - post its recv
      int numIncoming = numRecving[recvIdx];
      if (numIncoming > 0){
        Migration& m = patch.incoming[recvIdx];
        m.parts.resize(numIncoming);
        debug("Rank %d receiving %d from %d on face %c%c\n",
              patch.id, numRecving[recvIdx], m.rank, inChar(recvIdx), dimChar(recvIdx));
        MPI_Request req;
        MPI_Irecv(m.parts.data(), numIncoming*sizeof(Particle), MPI_BYTE,
                  m.rank, send_parts_tag, MPI_COMM_WORLD, &req);
        partRequests.push_back(req);
      }
    }
    ++numDone;
  }
  if (partRequests.size()){
    //we have pushed progress forward and now all sizes have been communicated
    //now wait on all the particles themselves to get shuffled
    MPI_Waitall(partRequests.size(), partRequests.data(), MPI_STATUSES_IGNORE);
  }
  partRequests.clear(); //for the next round
  MPI_Wait(&collectiveReq, MPI_STATUS_IGNORE);

  //pack the migrated particles into the main buffer
#pragma sst instead skeletonPackMigrated(patch)
  packMigrated(patch);

  return systemTotal;
}

void move(int step, Patch& patch)
{
  debug("Rank %d moving %d particles on step %d\n",
        patch.id, int(patch.local.size()), step);
#pragma omp parallel for
  for (int i=0; i < patch.local.size(); ++i){
    Particle& part = patch.local[i];
    part.deltaT = patch.deltaT;
    moveParticle(i, part, patch);
    backfill(patch);
  }

#pragma sst call skeletonFillOutgoing(patch)
#pragma sst call skeletonInitOutgoing(step,patch) //gets called first for now
  int numQuiesced = patch.local.size();
  int substep = 0;
  int systemTotalMoves = exchange(patch, substep);

  if (patch.local.size() < numQuiesced){
    std::cerr << "how is patch size " << patch.local.size()
              << " less than numQ " << numQuiesced
              << "???" << std::endl;
    abort();
  }

  while (systemTotalMoves > 0){
    substep += 1;
#pragma omp parallel for
    for (int i=numQuiesced; i < patch.local.size(); ++i){
      Particle& part = patch.local[i];
      moveParticle(i, part, patch);
    }
#pragma sst call skeletonFillOutgoing(patch)
    systemTotalMoves = exchange(patch, substep);
    numQuiesced = patch.local.size();
  }
}

void init(Patch& patch, int ppc, int nPatchesX, int nPatchesY, int nPatchesZ)
{
  patch.local.resize(ppc*patch.nCells);
  //id = z*ny*nx + y*nx + x;
  int myZ = patch.id / (nPatchesX*nPatchesY);
  int remId = patch.id % (nPatchesX*nPatchesY);
  int myY= remId / nPatchesX;
  int myX = remId % nPatchesX;

  patch.gridPosition[0] = myX;
  patch.gridPosition[1] = myY;
  patch.gridPosition[2] = myZ;

  /**
  This is how you would initialize if it were a real app
  int lastX = patch.localGridDims[0] - 1;
  for (int y=0; y < patch.localGridDims[1]; ++y){
    for (int z=0; z < patch.localGridDims[2]; ++z){
      int localCell = cellId(patch, lastX, y, z);
      int remoteCell = cellId(patch, 0, y, z);
    }
  }
  */

  debug("Rank %d maps to %d-%d-%d\n", patch.id, myX, myY, myZ);

  patch.outgoing.resize(6);
  patch.incoming.resize(6);

  //I have a plus X partner
  int plusX = (myX + 1) % nPatchesX;
  int plusXpartner = patchId(plusX,myY,myZ,nPatchesX,nPatchesY,nPatchesZ);
  debug("Rank %d outgoing face +X is %d\n", patch.id, plusXpartner);
  patch.outgoing[1].rank = plusXpartner;
  patch.incoming[0].rank = plusXpartner;

  //I have a minus X partner
  int minusX = (myX + nPatchesX - 1) % nPatchesX;
  int minusXpartner = patchId(minusX,myY,myZ,nPatchesX,nPatchesY,nPatchesZ);
  debug("Rank %d outgoing face -X is %d\n", patch.id, minusXpartner);
  patch.outgoing[0].rank = minusXpartner;
  patch.incoming[1].rank = minusXpartner;

  //I have a plus Y partner
  int plusY = (myY + 1) % nPatchesY;
  int plusYpartner = patchId(myX,plusY,myZ,nPatchesX,nPatchesY,nPatchesZ);
  debug("Rank %d outgoing face +Y is %d\n", patch.id, plusYpartner);
  patch.outgoing[3].rank = plusYpartner;
  patch.incoming[2].rank = plusYpartner;

  //I have a minus Y partner
  int minusY = (myY + nPatchesY - 1) % nPatchesY;
  int minusYpartner = patchId(myX,minusY,myZ,nPatchesX,nPatchesY,nPatchesZ);
  debug("Rank %d outgoing face -Y is %d\n", patch.id, minusYpartner);
  patch.outgoing[2].rank = minusYpartner;
  patch.incoming[3].rank = minusYpartner;

  //I have a plus Z partner
  int plusZ = (myZ + 1) % nPatchesZ;
  int plusZpartner = patchId(myX,myY,plusZ,nPatchesX,nPatchesY,nPatchesZ);
  debug("Rank %d outgoing face +Z is %d\n", patch.id, plusZpartner);
  patch.outgoing[5].rank = plusZpartner;
  patch.incoming[4].rank = plusZpartner;

  //I have a minus Z partner
  int minusZ = (myZ + nPatchesZ - 1) % nPatchesZ;
  int minusZpartner = patchId(myX,myY,minusZ,nPatchesX,nPatchesY,nPatchesZ);
  debug("Rank %d outgoing face -Z is %d\n", patch.id, minusZpartner);
  patch.outgoing[4].rank = minusZpartner;
  patch.incoming[5].rank = minusZpartner;
}

#define crash_main(rank,...) \
  if (rank == 0){ \
    fprintf(stderr, __VA_ARGS__); \
    fprintf(stderr, "\n"); \
    usage(); \
    return 1; \
  } else { \
    return 0; \
  }

double get_time()
{
  timeval t_st;
  gettimeofday(&t_st, 0);
  double t = t_st.tv_sec + 1e-6 * t_st.tv_usec;
  return t;
}

void usage()
{
  fprintf(stderr, "usage: run <nsteps> <ppc> <cells/x> <cells/y> <cells/z> "
          "<patches/x> <patches/y> <patches/z>\n");
  fflush(stderr);
}

int main(int argc, char** argv)
{
  MPI_Init(&argc, &argv);

  Patch myPatch;
  MPI_Comm_rank(MPI_COMM_WORLD, &myPatch.id);
  MPI_Comm_size(MPI_COMM_WORLD, &myPatch.nPatches);

  if (argc != 9){
    crash_main(myPatch.id, "bad number of arguments");
  }



  int nSteps = atoi(argv[1]);
  int ppc = atoi(argv[2]);
  //the number of cells locally
  myPatch.localGridDims[0] = atoi(argv[3]);
  myPatch.localGridDims[1]  = atoi(argv[4]);
  myPatch.localGridDims[2]  = atoi(argv[5]);
  myPatch.nCells = myPatch.localGridDims[0] * myPatch.localGridDims[1]
                    * myPatch.localGridDims[2];
  if (myPatch.nCells == 0){
    crash_main(myPatch.id, "either got zero cell dim or misformatted number")
  }

  //the number of patches locally
  int nPatchesX = atoi(argv[6]);
  int nPatchesY = atoi(argv[7]);
  int nPatchesZ = atoi(argv[8]);

  int nPatchesTotal = nPatchesX * nPatchesY * nPatchesZ;
  if (myPatch.nPatches != nPatchesTotal){
    crash_main(myPatch.id, "requested %d=%dx%dx%d patches, but have %d MPI ranks",
               nPatchesTotal, nPatchesX, nPatchesY, nPatchesZ, myPatch.nPatches);
  }

#pragma sst call skeletonInitOverdecomposition(myPatch,ppc)
  init(myPatch, ppc, nPatchesX, nPatchesY, nPatchesZ);
  for (int s=0; s < nSteps; ++s){
    double start = get_time();
    move(s,myPatch);
    double stop = get_time();
    if (myPatch.id == 0){
      double t_ms = (stop-start)*1e3;
      printf("Completed step %d in %10.1fms\n", s, t_ms);
    }
  }

  MPI_Finalize();
  return 0;
}

