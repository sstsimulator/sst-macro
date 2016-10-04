#include <mpi.h>

#define sstmac_app_name sendrecv

int main(int argc, char** argv)
{
  MPI_Init(&argc, &argv);
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  int tag = 42;
  if (rank == 0){
    int partner = 1;
    MPI_Send(NULL, 1, MPI_INT, partner, tag, MPI_COMM_WORLD);
  } else {
    int partner = 0;
    MPI_Recv(NULL, 1, MPI_INT, partner, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  }

  MPI_Barrier(MPI_COMM_WORLD);

  if (rank == 0){
    printf("Rank 0 finished at t=%8.4f ms\n", MPI_Wtime()*1e3);
  }

  MPI_Finalize();

  return 0;
}

