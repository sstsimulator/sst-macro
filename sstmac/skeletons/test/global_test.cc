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

#include <sstmac/skeleton.h>
#include <sstmac/compute.h>
#include <sstmac/util.h>
#include <sstmac/replacements/mpi.h>

#define sstmac_app_name global_test

global_int class_global_;

int USER_MAIN(int argc, char** argv)
{
  int rank, size;
  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  class_global_ = 0;
  printf("Rank %d got initial global %d\n", rank, int(class_global_));
  sstmac_sleep(1);
  class_global_ = class_global_ + 1;
  printf("Rank %d got final global %d\n", rank, int(class_global_));
  MPI_Finalize();

  return 0;
}

