/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/skeletons/test/test_onesided.h>
#include <sstmac/replacements/mpi.h>
#include <sprockit/errors.h>
#include <sprockit/sim_parameters.h>

#include <cstring>
#include <stdio.h>

namespace sstmac {
namespace sw {

SpktRegisterApp("test_onesided", test_onesided);

void
test_onesided::consume_params(sprockit::sim_parameters* params)
{
  iterations_ = params->get_int_param("test_onesided_iterations");
  msize_ = params->get_int_param("test_onesided_size");
  mode_ = params->get_int_param("test_onesided_mode");
}

//
// Go.
//
void
test_onesided::skeleton_main()
{
  MPI_Init(NULL, NULL);

  int myid;
  MPI_Comm_rank(MPI_COMM_WORLD, &myid);

  double* send = (double*) malloc(sizeof(double) * msize_);
  double* recv = (double*) malloc(sizeof(double) * msize_);

  if (mode_ == 0) {
    MPI_Status status;
    if (myid == 0) {
      double start, stop, diff;
      start = MPI_Wtime();
      for (int i = 0; i < iterations_; i++) {
        MPI_Send(send, msize_, MPI_DOUBLE, 1, 0, MPI_COMM_WORLD);
        MPI_Recv(recv, msize_, MPI_DOUBLE, 1, 1, MPI_COMM_WORLD, &status);
      }
      stop = MPI_Wtime();
      diff = stop - start;
      printf("%d %d-double messages took %8.8fs\n", iterations_, msize_,
             diff);
    }
    else {
      for (int i = 0; i < iterations_; i++) {
        MPI_Recv(send, msize_, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD, &status);
        MPI_Send(recv, msize_, MPI_DOUBLE, 0, 1, MPI_COMM_WORLD);
      }
    }
  }
  else if (mode_ == 1) {
    MPI_Win win;
    int soi = sizeof(double);
    if (myid == 0) {
      MPI_Win_create(send, soi * msize_, soi, MPI_INFO_NULL,
                     MPI_COMM_WORLD, &win);
    }
    else {
      MPI_Win_create(recv, soi * msize_, soi, MPI_INFO_NULL,
                     MPI_COMM_WORLD, &win);
    }

    MPI_Win_fence(MPI_MODE_NOPRECEDE, win);
    if (myid == 0) {
      double start, stop, diff;
      start = MPI_Wtime();
      for (int i = 0; i < iterations_; i++) {

        MPI_Get(recv, msize_, MPI_DOUBLE, 1, 0, msize_, MPI_DOUBLE, win);
        MPI_Win_fence(0, win);
      }
      stop = MPI_Wtime();
      diff = stop - start;
      printf("%d %d-double messages took %8.8fs\n", iterations_, msize_,
             diff);
    }
    else {
      for (int i = 0; i < iterations_; i++) {

        MPI_Get(send, msize_, MPI_DOUBLE, 0, 0, msize_, MPI_DOUBLE, win);
        MPI_Win_fence(0, win);
      }
    }
    MPI_Win_free(&win);
  }
  else if (mode_ == 2) {
    MPI_Win win;
    int soi = sizeof(double);
    if (myid == 0) {
      MPI_Win_create(send, soi * msize_, soi, MPI_INFO_NULL,
                     MPI_COMM_WORLD, &win);
    }
    else {
      MPI_Win_create(recv, soi * msize_, soi, MPI_INFO_NULL,
                     MPI_COMM_WORLD, &win);
    }

    MPI_Win_fence(MPI_MODE_NOPRECEDE, win);
    if (myid == 0) {
      double start, stop, diff;
      start = MPI_Wtime();
      for (int i = 0; i < iterations_; i++) {

        MPI_Put(recv, msize_, MPI_DOUBLE, 1, 0, msize_, MPI_DOUBLE, win);
        MPI_Win_fence(0, win);
      }
      stop = MPI_Wtime();
      diff = stop - start;
      printf("%d %d-double messages took %8.8fs\n", iterations_, msize_,
             diff);
    }
    else {
      for (int i = 0; i < iterations_; i++) {

        MPI_Put(send, msize_, MPI_DOUBLE, 0, 0, msize_, MPI_DOUBLE, win);
        MPI_Win_fence(0, win);
      }
    }
    MPI_Win_free(&win);
  }

  MPI_Finalize();
}

}
} // end of namespace sstmac.

