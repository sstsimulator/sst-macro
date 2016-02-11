#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv) 
{
    int message_size = 128;
    int me, nproc;
    int tag = 0;
    int dst = 1;
    int src = 0;
    MPI_Status stat;

    MPI_Init(&argc,&argv);
    MPI_Comm world = MPI_COMM_WORLD;
    MPI_Comm_rank(world,&me);
    MPI_Comm_size(world,&nproc);
    
    if (nproc != 2) {
        fprintf(stderr, "sendrecv should only be run with two processors\n");
        abort();
    }

    if (me == 0) {
        MPI_Send(NULL, message_size, MPI_INT, dst, tag, world);
        printf("rank %i sending a message\n", me);
    }
    else {
        MPI_Recv(NULL, message_size, MPI_INT, src, tag, world, &stat);
        printf("rank %i receiving a message\n", me);
    }

    MPI_Finalize();

    return 0;
}

