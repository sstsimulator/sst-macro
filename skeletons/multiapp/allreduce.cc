#include <mpi.h>

#define sstmac_app_name allreduce_test

int main(int argc, char** argv)
{
  MPI_Init(&argc, &argv);
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  MPI_Allreduce(NULL, NULL, 100, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

  MPI_Finalize();
  return 0;
}

