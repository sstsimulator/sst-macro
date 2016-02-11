#include <mpi.h>


int main(int argc, char** argv)
{
    MPI_Init(&argc, &argv);
    int count = 1000;
    MPI_Allreduce(NULL, NULL, count, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    MPI_Finalize();
    return 0;
}

