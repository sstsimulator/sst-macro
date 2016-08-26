#include <mpi.h>

#define sstmac_app_name test

int main(int argc, char** argv)
{
  MPI_Init(&argc, &argv);
  MPI_Barrier(MPI_COMM_WORLD);
  MPI_Finalize();
  return 0;
}

