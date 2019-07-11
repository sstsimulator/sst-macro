#include <vector>
#include <cstdlib>
#include <mpi.h>
#include <cstring>
#include <sprockit/keyword_registration.h>
#include <iostream>
#include "pic.h"

RegisterKeywords(
 {"odx", "the level of intra-patch overdecomposition in X direction"},
 {"ody", "the level of intra-patch overdecomposition in Y direction"},
 {"odz", "the level of intra-patch overdecomposition in Z direction"},
 {"unitx", "the stride of X units with uniform particle move characteristics" },
 {"unity", "the stride of Y units with uniform particle move characteristics" },
 {"unitz", "the stride of Z units with uniform particle move characteristics" },
 {"scramble", "whether to constantly scramble particle movements or keep steady flow"},
 {"max_migration", "the max fraction of particles that can migrate"},
 {"min_migration", "the min fraction of particles that can migrate"},
);

static const double skelMaxMoveFraction=0.5;

static const int primes[] = {
 1571, 2459, 2801, 3559, 3079, 3019, 6269
};
static const int numPrimes = 7;//sizeof(primes) / sizeof(int);
static inline double getSkeletonFraction(int step, Patch& p, int dim)
{

  int myIdx = p.od[dim]*p.gridPosition[dim] / p.uniformityFactory[dim];
  if (p.scrambleMigration){
    myIdx = primes[(myIdx+step) % numPrimes] % numPrimes;
  }
  double myExtraFraction = p.maxMigrateDifference * myIdx / numPrimes;
  return p.minMigrateFraction + myExtraFraction;
}

void skeletonInitOutgoing(int step, Patch& p)
{
  double xInc = getSkeletonFraction(step, p, 0);
  double yInc = getSkeletonFraction(step, p, 1);
  double zInc = getSkeletonFraction(step, p, 2);

  p.microIterScale = p.migrateFraction = (xInc + yInc + zInc);
}

void skeletonInitOverdecomposition(Patch& p, int ppc){

  p.od[0] = sstmac::getParam<int>("odx", 1);
  p.od[1] = sstmac::getParam<int>("ody", 1);
  p.od[2] = sstmac::getParam<int>("odz", 1);
  p.uniformityFactory[0] = sstmac::getParam<int>("unitx", 1);
  p.uniformityFactory[1] = sstmac::getParam<int>("unity", 1);
  p.uniformityFactory[2] = sstmac::getParam<int>("unitz", 1);

  int numOdBoxes = p.od[0] * p.od[1] * p.od[2];
  int numPartsPerOdBox = (ppc*p.nCells) / numOdBoxes;
  for (int x=0; x < p.od[0]; ++x){
    for (int y=0; y < p.od[1]; ++y){
      for (int z=0; z < p.od[2]; ++z){
        p.boxOcc[x][y][z] = numPartsPerOdBox;
      }
    }
  }

  p.minMigrateFraction = sstmac::getParam<double>("min_migration", 0.02);
  double maxMig = sstmac::getParam<double>("max_migration", 0.06);
  p.maxMigrateDifference = maxMig - p.minMigrateFraction;
  if (p.maxMigrateDifference < 0){
    std::cerr << "Max migration=" << maxMig << " must be greater than min="
              << p.minMigrateFraction << std::endl;
    abort();
  }
  p.scrambleMigration = sstmac::getParam<bool>("scramble", false);
}

void skeletonPackMigrated(Patch& p){
  int totalIncoming = 0;
  { Migration& m = p.incoming[0];
  if (m.parts.size() > 0){
    //spread evenly across the boxes
    int numIncomingPerBox = m.parts.size() / (p.od[1] * p.od[2]);
    totalIncoming += m.parts.size();
    for (int y=0; y < p.od[1]; ++y){
      for (int z=0; z < p.od[2]; ++z){
        p.boxOcc[0][y][z] += numIncomingPerBox;
        debug("Rank %d box %d-%d-%d now at %d parts after its share of %d\n",
               p.id,0,y,z,p.boxOcc[0][y][z],int(m.parts.size()));
      }
    }
    m.parts.clear();
  } }

  { Migration& m = p.incoming[1];
  if (m.parts.size() > 0){
    //spread evenly across the boxes
    int numIncomingPerBox = m.parts.size() / (p.od[1] * p.od[2]);
    totalIncoming += m.parts.size();
    int lastX = p.od[0] - 1;
    for (int y=0; y < p.od[1]; ++y){
      for (int z=0; z < p.od[2]; ++z){
        p.boxOcc[lastX][y][z] += numIncomingPerBox;
      }
    }
    m.parts.clear();
  } }

  { Migration& m = p.incoming[2];
  if (m.parts.size() > 0){
    //spread evenly across the boxes
    int numIncomingPerBox = m.parts.size() / (p.od[0] * p.od[2]);
    totalIncoming += m.parts.size();
    for (int x=0; x < p.od[0]; ++x){
      for (int z=0; z < p.od[2]; ++z){
        p.boxOcc[x][0][z] += numIncomingPerBox;
      }
    }
    m.parts.clear();
  } }

  { Migration& m = p.incoming[3];
  if (m.parts.size() > 0){
    //spread evenly across the boxes
    int numIncomingPerBox = m.parts.size() / (p.od[0] * p.od[2]);
    totalIncoming += m.parts.size();
    int lastY = p.od[1] - 1;
    for (int x=0; x < p.od[0]; ++x){
      for (int z=0; z < p.od[2]; ++z){
        p.boxOcc[x][lastY][z] += numIncomingPerBox;
      }
    }
    m.parts.clear();
  } }

  { Migration& m = p.incoming[4];
  if (m.parts.size() > 0){
    //spread evenly across the boxes
    int numIncomingPerBox = m.parts.size() / (p.od[0] * p.od[1]);
    totalIncoming += m.parts.size();
    for (int x=0; x < p.od[0]; ++x){
      for (int y=0; y < p.od[1]; ++y){
        p.boxOcc[x][y][0] += numIncomingPerBox;
      }
    }
    m.parts.clear();
  } }

  { Migration& m = p.incoming[5];
  if (m.parts.size() > 0){
    //spread evenly across the boxes
    int numIncomingPerBox = m.parts.size() / (p.od[0] * p.od[1]);
    totalIncoming += m.parts.size();
    int lastZ = p.od[2] - 1;
    for (int x=0; x < p.od[0]; ++x){
      for (int y=0; y < p.od[1]; ++y){
        p.boxOcc[x][y][lastZ] += numIncomingPerBox;
      }
    }
    m.parts.clear();
  } }

  size_t oldSize = p.local.size();
  size_t newSize = oldSize + totalIncoming;
  p.local.resize(newSize);

  int boxSum = 0;
  for (int x=0; x < p.od[0]; ++x){
    for (int y=0; y < p.od[1]; ++y){
      for (int z=0; z < p.od[2]; ++z){
        boxSum += p.boxOcc[x][y][z];
        debug("Rank %d box %d-%d-%d now at %d parts\n",
               p.id,x,y,z,p.boxOcc[x][y][z]);
      }
    }
  }
  debug("Rank %d sum over boxes is %d\n", p.id, boxSum);
}


int skeletonFillOutgoing(Patch& p){
  int occIncrease[SKEL_MAX_OD][SKEL_MAX_OD][SKEL_MAX_OD];
  int numMovingPerFace[SKEL_NUM_FACES];

  ::memset(occIncrease, 0, sizeof(occIncrease));
  ::memset(numMovingPerFace, 0, sizeof(numMovingPerFace));

  int lastX = p.od[0] - 1;
  int lastY = p.od[1] - 1;
  int lastZ = p.od[2] - 1;
  for (int x=0; x < p.od[0]; ++x){
    for (int y=0; y < p.od[1]; ++y){
      for (int z=0; z < p.od[2]; ++z){
        double frac = p.migrateFraction;
        int boxMoves = frac * p.boxOcc[x][y][z];
        if (x==0 && x == lastX){
          numMovingPerFace[0] += boxMoves;
          numMovingPerFace[1] += boxMoves;
        } else if (x == 0) {
          numMovingPerFace[0] += boxMoves;
          occIncrease[x+1][y][z] += boxMoves;
        } else if (x == lastX) {
          numMovingPerFace[1] += boxMoves;
          occIncrease[x-1][y][z] += boxMoves;
        } else {
          occIncrease[x+1][y][z] += boxMoves;
          occIncrease[x-1][y][z] += boxMoves;
        }

        if (y==0 && y == lastY){
          numMovingPerFace[2] += boxMoves;
          numMovingPerFace[3] += boxMoves;
        } else if (y == 0) {
          numMovingPerFace[2] += boxMoves;
          occIncrease[x][y+1][z] += boxMoves;
        } else if (y == lastY) {
          numMovingPerFace[3] += boxMoves;
          occIncrease[x][y-1][z] += boxMoves;
        } else {
          occIncrease[x][y+1][z] += boxMoves;
          occIncrease[x][y-1][z] += boxMoves;
        }

        if (z==0 && z == lastZ){
          numMovingPerFace[4] += boxMoves;
          numMovingPerFace[5] += boxMoves;
        } else if (z == 0) {
          numMovingPerFace[4] += boxMoves;
          occIncrease[x][y][z+1] += boxMoves;
        } else if (z == lastZ) {
          numMovingPerFace[5] += boxMoves;
          occIncrease[x][y][z-1] += boxMoves;
        } else {
          occIncrease[x][y][z+1] += boxMoves;
          occIncrease[x][y][z-1] += boxMoves;
        }
        //we lose some out each face
        p.boxOcc[x][y][z] -= 6*boxMoves;
      }
    }
  }

  p.migrateFraction *= p.microIterScale;

  int totalLocalOcc = 0;
  for (int x=0; x < p.od[0]; ++x){
    for (int y=0; y < p.od[1]; ++y){
      for (int z=0; z < p.od[2]; ++z){
        int oldOcc = p.boxOcc[x][y][z];
        p.boxOcc[x][y][z] += occIncrease[x][y][z];
        debug("Rank %d box %d-%d-%d depleted to %d, but back up to %d\n",
               p.id, x,y,z, oldOcc, p.boxOcc[x][y][z]);
        totalLocalOcc += p.boxOcc[x][y][z];
      }
    }
  }

  int totalMoving = 0;
  for (int i=0; i < SKEL_NUM_FACES; ++i){
    debug("Rank %d produced %d outgoing on face %c%c\n",
           p.id, numMovingPerFace[i], outChar(i), dimChar(i));
    p.outgoing[i].parts.resize(numMovingPerFace[i]);
    totalMoving += numMovingPerFace[i];
  }

  debug("Total is %d + %d = %d -> %d per\n",
         totalLocalOcc, totalMoving, totalLocalOcc + totalMoving,
         (totalLocalOcc + totalMoving) / (p.od[0]*p.od[1]*p.od[2]));

  int newSize = p.local.size() - totalMoving;
  double moveFraction = totalMoving / double(p.local.size());
  if (moveFraction > skelMaxMoveFraction){
    std::cerr << "Rank " << p.id << " too many particles=" << totalMoving
              << " moving relative to total=" << int(p.local.size())
              << std::endl;
    abort();
  }
  p.local.resize(newSize);

  return totalMoving;
}





