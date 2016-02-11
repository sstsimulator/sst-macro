/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#include <sstmac/replacements/mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include "mpitest.h"

namespace test3
{

  /* Tests the example in Fig 6.8, pg 142, MPI-2 standard. Process 1 has
   a blocking MPI_Recv between the Post and Wait. Therefore, this
   example will not run if the one-sided operations are simply
   implemented on top of MPI_Isends and Irecvs. They either need to be
   implemented inside the progress engine or using threads with Isends
   and Irecvs. In MPICH-2, they are implemented in the progress engine. */

#define SIZE 1048576

  int
  test3(int argc, char *argv[])
  {
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

      A = (int *) malloc(SIZE * sizeof(int));
      if (!A)
      {
        printf("Can't allocate memory in test program\n");
        MPI_Abort(world, 1);
      }
      B = (int *) malloc(SIZE * sizeof(int));
      if (!B)
      {
        printf("Can't allocate memory in test program\n");
        MPI_Abort(world, 1);
      }

      MPI_Comm_group(world, &comm_group);

      if (rank == 0)
      {
        for (i = 0; i < SIZE; i++)
        {
          A[i] = i;
          B[i] = SIZE + i;
        }
        MPI_Win_create(NULL, 0, 1, MPI_INFO_NULL, world, &win);
        destrank = 1;
        MPI_Group_incl(comm_group, 1, &destrank, &group);
        MPI_Win_start(group, 0, win);
        MPI_Put(A, SIZE, MPI_INT, 1, 0, SIZE, MPI_INT, win);
        MPI_Win_complete(win);
        MPI_Send(B, SIZE, MPI_INT, 1, 100, world);
      }

      else
      { /* rank=1 */
        for (i = 0; i < SIZE; i++)
          A[i] = B[i] = (-4) * i;
        MPI_Win_create(B, SIZE * sizeof(int), sizeof(int), MPI_INFO_NULL,
            world, &win);
        destrank = 0;
        MPI_Group_incl(comm_group, 1, &destrank, &group);
        MPI_Win_post(group, 0, win);
        MPI_Recv(A, SIZE, MPI_INT, 0, 100, world, MPI_STATUS_IGNORE);
        MPI_Win_wait(win);

        for (i = 0; i < SIZE; i++)
        {
          if (B[i] != i)
          {
            printf("Rank 1: Put Error: B[i] is %d, should be %d\n", B[i], i);
            errs++;
          }
          if (A[i] != SIZE + i)
          {
            printf("Rank 1: Send/Recv Error: A[i] is %d, should be %d\n", A[i],
                SIZE + i);
            errs++;
          }
        }
      }

      MPI_Group_free(&group);
      MPI_Group_free(&comm_group);
      MPI_Win_free(&win);
      free(A);
      free(B);
    }
    MTest_Finalize(errs);
    MPI_Finalize();
    return 0;
  }

}
