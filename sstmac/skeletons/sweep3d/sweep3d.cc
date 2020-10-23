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
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sstmac/replacements/mpi.h>
#include <sstmac/replacements/sys/time.h>
#include <sstmac/replacements/time.h>

void get_position(const int rank, const int pex, const int pey, int* myX,
                  int* myY) {
  *myX = rank % pex;
  *myY = rank / pex;
}

void compute(long sleep) {
  struct timespec sleepTS;
  sleepTS.tv_sec = 0;
  sleepTS.tv_nsec = sleep;

  struct timespec remainTS;

  if (nanosleep(&sleepTS, &remainTS) == EINTR) {
    while (nanosleep(&remainTS, &remainTS) == EINTR)
      ;
  }
}

#define sstmac_app_name sweep3d
int USER_MAIN(int argc, char* argv[]) 
{
  MPI_Init(&argc, &argv);

  int rank = -1;
  int size = -1;

  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm sweep_comm = MPI_COMM_WORLD;

  int pex = -1;
  int pey = -1;
  int nx = 50;
  int ny = 50;
  int nz = 100;
  int kba = 10;
  int repeats = 1;

  int vars = 1;
  long sleep = 1000;
  int print = 0;

  for (int i = 0; i < argc; ++i) {
    if (strcmp("-pex", argv[i]) == 0) {
      pex = atoi(argv[i + 1]);
      i++;
    } else if (strcmp("-pey", argv[i]) == 0) {
      pey = atoi(argv[i + 1]);
      i++;
    } else if (strcmp("-iterations", argv[i]) == 0) {
      repeats = atoi(argv[i + 1]);
      i++;
    } else if (strcmp("-nx", argv[i]) == 0) {
      nx = atoi(argv[i + 1]);
      i++;
    } else if (strcmp("-ny", argv[i]) == 0) {
      ny = atoi(argv[i + 1]);
      i++;
    } else if (strcmp("-nz", argv[i]) == 0) {
      nz = atoi(argv[i + 1]);
      i++;
    } else if (strcmp("-sleep", argv[i]) == 0) {
      sleep = atol(argv[i + 1]);
      i++;
    } else if (strcmp("-vars", argv[i]) == 0) {
      vars = atoi(argv[i + 1]);
      i++;
    } else if (strcmp("-kba", argv[i]) == 0) {
      kba = atoi(argv[i + 1]);
      i++;
    }  else if (strcmp(argv[i], "-print") == 0){
      print = atoi(argv[i + 1]);
      i++;
    }
  }

  if (kba == 0) {
    if (rank == 0) {
      fprintf(stderr,
              "K-Blocking Factor must not be zero. Please specify -kba <value "
              "> 0>\n");
    }
    MPI_Barrier(MPI_COMM_WORLD); //needed to force correct printing
    exit(-1);
  }

  if (nz % kba != 0) {
    if (rank == 0) {
      fprintf(stderr,
              "KBA must evenly divide NZ, KBA=%d, NZ=%d, remainder=%d (must be "
              "zero)\n",
              kba, nz, (nz % kba));
    }
    MPI_Barrier(MPI_COMM_WORLD); //needed to force correct printing
    exit(-1);
  }

  if ((pex * pey) != size) {
    if (0 == rank) {
      fprintf(
          stderr,
          "Error: processor decomposition (%d x %d) != number of ranks (%d)\n",
          pex, pey, size);
    }
    MPI_Barrier(MPI_COMM_WORLD); //needed to force correct printing
    exit(-1);
  }

  if (rank == 0 && print) {
    printf("# Sweep3D Communication Pattern\n");
    printf("# Info:\n");
    printf("# Px:              %8d\n", pex);
    printf("# Py:              %8d\n", pey);
    printf("# Nx x Ny x Nz:    %8d x %8d x %8d\n", nx, ny, nz);
    printf("# KBA:             %8d\n", kba);
    printf("# Variables:       %8d\n", vars);
    printf("# Iterations:      %8d\n", repeats);
  }

  int myX = -1;
  int myY = -1;

  get_position(rank, pex, pey, &myX, &myY);

  const int xUp = (myX != (pex - 1)) ? rank + 1 : -1;
  const int xDown = (myX != 0) ? rank - 1 : -1;

  const int yUp = (myY != (pey - 1)) ? rank + pex : -1;
  const int yDown = (myY != 0) ? rank - pex : -1;

  MPI_Status status;

  double* xRecvBuffer = nullptr;
  double* xSendBuffer = nullptr;

  double* yRecvBuffer = nullptr;
  double* ySendBuffer = nullptr;

  struct timeval start;
  struct timeval end;

  gettimeofday(&start, NULL);

  // We repeat this sequence twice because there are really 8 vertices in the 3D
  // data domain and we sweep from each of them, processing the top four first
  // and then the bottom four vertices next.
  for (int i = 0; i < (repeats * 2); ++i) {
    // Recreate communication pattern of sweep from (0,0) towards (Px,Py)
    struct timeval iter_start;
    struct timeval iter_end;
    gettimeofday(&iter_start, NULL);
    for (int k = 0; k < nz; k += kba) {
      if (xDown > -1) {
        MPI_Recv(xRecvBuffer, (nx * kba * vars), MPI_DOUBLE, xDown, 1000,
                 sweep_comm, &status);
      }

      if (yDown > -1) {
        MPI_Recv(yRecvBuffer, (ny * kba * vars), MPI_DOUBLE, yDown, 1000,
                 sweep_comm, &status);
      }

      compute(sleep);

      if (xUp > -1) {
        MPI_Send(xSendBuffer, (nx * kba * vars), MPI_DOUBLE, xUp, 1000,
                 sweep_comm);
      }

      if (yUp > -1) {
        MPI_Send(ySendBuffer, (nx * kba * vars), MPI_DOUBLE, yUp, 1000,
                 sweep_comm);
      }
    }

    // Recreate communication pattern of sweep from (Px,0) towards (0,Py)
    for (int k = 0; k < nz; k += kba) {
      if (xUp > -1) {
        MPI_Recv(xRecvBuffer, (nx * kba * vars), MPI_DOUBLE, xUp, 2000,
                 sweep_comm, &status);
      }

      if (yDown > -1) {
        MPI_Recv(yRecvBuffer, (ny * kba * vars), MPI_DOUBLE, yDown, 2000,
                 sweep_comm, &status);
      }

      compute(sleep);

      if (xDown > -1) {
        MPI_Send(xSendBuffer, (nx * kba * vars), MPI_DOUBLE, xDown, 2000,
                 sweep_comm);
      }

      if (yUp > -1) {
        MPI_Send(ySendBuffer, (nx * kba * vars), MPI_DOUBLE, yUp, 2000,
                 sweep_comm);
      }
    }

    // Recreate communication pattern of sweep from (Px,Py) towards (0,0)
    for (int k = 0; k < nz; k += kba) {
      if (xUp > -1) {
        MPI_Recv(xRecvBuffer, (nx * kba * vars), MPI_DOUBLE, xUp, 3000,
                 sweep_comm, &status);
      }

      if (yUp > -1) {
        MPI_Recv(yRecvBuffer, (ny * kba * vars), MPI_DOUBLE, yUp, 3000,
                 sweep_comm, &status);
      }

      compute(sleep);

      if (xDown > -1) {
        MPI_Send(xSendBuffer, (nx * kba * vars), MPI_DOUBLE, xDown, 3000,
                 sweep_comm);
      }

      if (yDown > -1) {
        MPI_Send(ySendBuffer, (nx * kba * vars), MPI_DOUBLE, yDown, 3000,
                 sweep_comm);
      }
    }

    // Recreate communication pattern of sweep from (0,Py) towards (Px,0)
    for (int k = 0; k < nz; k += kba) {
      if (xDown > -1) {
        MPI_Recv(xRecvBuffer, (nx * kba * vars), MPI_DOUBLE, xDown, 4000,
                 sweep_comm, &status);
      }

      if (yUp > -1) {
        MPI_Recv(yRecvBuffer, (ny * kba * vars), MPI_DOUBLE, yUp, 4000,
                 sweep_comm, &status);
      }

      compute(sleep);

      if (xUp > -1) {
        MPI_Send(xSendBuffer, (nx * kba * vars), MPI_DOUBLE, xUp, 4000,
                 sweep_comm);
      }

      if (yDown > -1) {
        MPI_Send(ySendBuffer, (nx * kba * vars), MPI_DOUBLE, yDown, 4000,
                 sweep_comm);
      }
    }
    gettimeofday(&iter_end, NULL);
    const double timeTaken = (iter_end.tv_sec-iter_start.tv_sec) + (iter_end.tv_usec-iter_start.tv_usec)*1e-6;
    if (print){
      printf("Rank %d = [%d,%d] iteration %d: %12.8fs\n", rank, myX, myY, i, timeTaken);
    }
  }

  MPI_Barrier(MPI_COMM_WORLD);
  gettimeofday(&end, NULL);

  const double timeTaken =
      (((double)end.tv_sec) + ((double)end.tv_usec) * 1.0e-6) -
      (((double)start.tv_sec) + ((double)start.tv_usec) * 1.0e-6);

  if (rank == 0){
    if (print){
      printf("Total time = %20.6f\n", timeTaken);
    }
  }
  MPI_Finalize();
  return 0;
}
