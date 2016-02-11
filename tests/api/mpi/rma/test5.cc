/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#include <sstmac/replacements/mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include "mpitest.h"

namespace test5
{

  /* tests a series of Gets. Run on 2 processes. */

#define SIZE 2000

  int
  test5(int argc, char *argv[])
  {
    int rank, nprocs, i, A[SIZE], B[SIZE];
    MPI_Win win;
    int errs = 0;

    MTest_Init(&argc, &argv);

    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    MPI_Comm world;

    if (nprocs != 2)
    {
      MPI_Comm_split(MPI_COMM_WORLD, (rank < 2), rank, &world);
    }
    else
    {
      world = MPI_COMM_WORLD;
    }

    if (rank < 2)
    {

      if (rank == 0)
      {
        for (i = 0; i < SIZE; i++)
          B[i] = 500 + i;
        MPI_Win_create(B, SIZE * sizeof(int), sizeof(int), MPI_INFO_NULL,
            world, &win);
        MPI_Win_fence(0, win);
        for (i = 0; i < SIZE; i++)
        {
          A[i] = i + 100;
          MPI_Get(&A[i], 1, MPI_INT, 1, i, 1, MPI_INT, win);
        }
        MPI_Win_fence(0, win);
        for (i = 0; i < SIZE; i++)
          if (A[i] != 1000 + i)
          {
            printf("Rank 0: A[%d] is %d, should be %d\n", i, A[i], 1000 + i);
            errs++;
          }
      }

      if (rank == 1)
      {
        for (i = 0; i < SIZE; i++)
          A[i] = 1000 + i;
        MPI_Win_create(A, SIZE * sizeof(int), sizeof(int), MPI_INFO_NULL,
            world, &win);
        MPI_Win_fence(0, win);
        for (i = 0; i < SIZE; i++)
        {
          B[i] = i + 200;
          MPI_Get(&B[i], 1, MPI_INT, 0, i, 1, MPI_INT, win);
        }
        MPI_Win_fence(0, win);
        for (i = 0; i < SIZE; i++)
          if (B[i] != 500 + i)
          {
            printf("Rank 1: B[%d] is %d, should be %d\n", i, B[i], 500 + i);
            errs++;
          }
      }

      MPI_Win_free(&win);

    }
    MTest_Finalize(errs);
    MPI_Finalize();
    return 0;
  }

}
