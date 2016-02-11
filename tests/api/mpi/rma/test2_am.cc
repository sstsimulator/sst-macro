/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#include <sstmac/replacements/mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include "mpitest.h"

namespace test2_am
{

  /* tests put and get with post/start/complete/wait on 2 processes */

  /* same as test1.c but uses alloc_mem */

#define SIZE1 100
#define SIZE2 200

  int
  test2_am(int argc, char *argv[])
  {
    /*    int rank, destrank, nprocs, A[SIZE2], B[SIZE2], i; */
    int rank, destrank, nprocs, *A, *B, i;
    MPI_Group comm_group, group;
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

      i = MPI_Alloc_mem(SIZE2 * sizeof(int), MPI_INFO_NULL, &A);
      if (i)
      {
        printf("Can't allocate memory in test program\n");
        MPI_Abort(world, 1);
      }
      i = MPI_Alloc_mem(SIZE2 * sizeof(int), MPI_INFO_NULL, &B);
      if (i)
      {
        printf("Can't allocate memory in test program\n");
        MPI_Abort(world, 1);
      }

      MPI_Comm_group(world, &comm_group);

      if (rank == 0)
      {
        for (i = 0; i < SIZE2; i++)
          A[i] = B[i] = i;
        MPI_Win_create(NULL, 0, 1, MPI_INFO_NULL, world, &win);
        destrank = 1;
        MPI_Group_incl(comm_group, 1, &destrank, &group);
        MPI_Win_start(group, 0, win);
        for (i = 0; i < SIZE1; i++)
          MPI_Put(A + i, 1, MPI_INT, 1, i, 1, MPI_INT, win);
        for (i = 0; i < SIZE1; i++)
          MPI_Get(B + i, 1, MPI_INT, 1, SIZE1 + i, 1, MPI_INT, win);

        MPI_Win_complete(win);

        for (i = 0; i < SIZE1; i++)
          if (B[i] != (-4) * (i + SIZE1))
          {
            printf("Get Error: B[i] is %d, should be %d\n", B[i],
                (-4) * (i + SIZE1));
            errs++;
          }
      }

      else
      { /* rank=1 */
        for (i = 0; i < SIZE2; i++)
          B[i] = (-4) * i;
        MPI_Win_create(B, SIZE2 * sizeof(int), sizeof(int), MPI_INFO_NULL,
            world, &win);
        destrank = 0;
        MPI_Group_incl(comm_group, 1, &destrank, &group);
        MPI_Win_post(group, 0, win);
        MPI_Win_wait(win);

        for (i = 0; i < SIZE1; i++)
        {
          if (B[i] != i)
          {
            printf("Put Error: B[i] is %d, should be %d\n", B[i], i);
            errs++;
          }
        }
      }

      MPI_Group_free(&group);
      MPI_Group_free(&comm_group);
      MPI_Win_free(&win);
      MPI_Free_mem(A);
      MPI_Free_mem(B);

    }
    MTest_Finalize(errs);
    MPI_Finalize();
    return 0;
  }

}
