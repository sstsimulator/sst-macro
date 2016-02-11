#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv) 
{
    int message_size = 1024;
    int me, nproc;
    int tag = 0;
    MPI_Status stat;

    MPI_Init(&argc,&argv);
    MPI_Comm world = MPI_COMM_WORLD;
    MPI_Comm_rank(world,&me);
    MPI_Comm_size(world,&nproc);

    int last_proc = nproc - 1;
    int send_to = me == last_proc ? 0 : me + 1;
    int recv_fr = me ? me - 1 : last_proc ;

    MPI_Send(NULL, message_size, MPI_INT, send_to, tag, world);
    //printf("[%d] -> %d\n", me, send_to);

    MPI_Recv(NULL, message_size, MPI_INT, recv_fr, tag, world, &stat);
    //printf("           [%d]  <- %d\n", me, recv_fr);

    MPI_Finalize();

    return 0;
}

