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
#ifndef pic_skeleton_h
#define pic_skeleton_h

struct Particle {
  double x[3];
  double v[3];
  double m;
  double deltaT;
  int cell;
};

//0th incoming face is +X
static const char incomingChars[] = { '+', '-' };
//0th outgoing face is -X
static const char outgoingChars[] = { '-', '+' };
static const char faceChars[] = {'X', 'Y', 'Z' };

#define inChar(x) incomingChars[x%2]
#define outChar(x) outgoingChars[x%2]
#define dimChar(x) faceChars[x/2]

#define debug(...) //printf(__VA_ARGS__)

#define SKEL_MAX_OD 4
#define SKEL_NUM_FACES 6

static const int send_size_tag = 100;
static const int send_parts_tag = 101;

struct Migration {
#pragma sst null_type sstmac::vector size resize clear data
  std::vector<Particle> parts;
  int rank; 
};

static inline double dot(double a[3], double b[3]){
  double prod = 0;
  prod += a[0]*b[0];
  prod += a[1]*b[1];
  prod += a[2]*b[2];
  return prod;
}

struct Face {
  double n[3];
  double x[3];
  int dstCell;
  int dstRank;
};

struct Cell {
  std::vector<Face> faces;
};

struct Patch {
  int id;
  int nPatches;
  int nCells;
  double center[3];
  double spacing;
  double deltaT;
  int localGridDims[3];
  int gridPosition[3];
#pragma sst null_type sstmac::vector size resize 
  std::vector<Particle> local; 
#pragma sst null_type sstmac::vector size resize push_back
  std::vector<int> holes;
#pragma sst null_type sstmac::vector size resize
  std::vector<Cell> cells;
  std::vector<Migration> outgoing;
  std::vector<Migration> incoming;

  /** The extra variables for running the skeleton */
  int od[3]; //the od factor in each dim
  int uniformityFactory[3];
  //What decrease fraction per micro-iteration in # particles moving
  double microIterScale;
  //What is current fraction of particles migrating
  double migrateFraction;
  double minMigrateFraction;
  double maxMigrateDifference;
  bool scrambleMigration;
  int boxOcc[SKEL_MAX_OD][SKEL_MAX_OD][SKEL_MAX_OD]; //allow for max 4x overdecomp in each dim
};

static inline int patchId(int x, int y, int z, int nx, int ny, int nz){
  return z*nx*ny + y*nx + x;
}

static inline int cellId(Patch& p, int x, int y, int z){
  return z*p.localGridDims[0]*p.localGridDims[1] +
      y*p.localGridDims[0] + x;
}

void packMigrated(Patch& patch);
int exchange(Patch& patch, int substep);
void backfill(Patch& patch);
void moveParticle(int idx, Particle& part, Patch& patch);
void move(int step, Patch& patch);
void init(Patch& patch, int ppc, int nPatchesX, int nPatchesY, int nPatchesZ);

#ifdef SSTMAC
void skeletonInitOutgoing(int step, Patch& p);
void skeletonInitOverdecomposition(Patch& p, int ppc);
void skeletonPackMigrated(Patch& p);
int skeletonFillOutgoing(Patch& p);
#endif

#endif

