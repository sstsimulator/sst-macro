#include <mpi.h>
#include <sprockit/mpi_parameters.h>
#include <sprockit/errors.h>
#include <sprockit/malloc.h>

using namespace sstmac;

int main(int argc, char **argv) {
    int me, nproc;
    int tag(0);
    int dst(1);
    int src(0);
    MPI_Status stat;

  
    MPI_Init(&argc,&argv);
    MPI_Comm world = MPI_COMM_WORLD;
    MPI_Comm_rank(world,&me);
    MPI_Comm_size(world,&nproc);

    sprockit::sim_parameters* params = sprockit::MPI_Bcast_params("app.ini");
    int message_size = params->get_int_param("sendrecv_message_size");
    
    if (me == 0 && nproc != 2)
        throw sprockit::input_error("sendrecv should only be run with two processors");

    void* buf = large_new(int, message_size);

    if (me == 0) {
        MPI_Send(buf, message_size, MPI_INT, dst, tag, world);
        printf("rank %i sending a message\n", me);
    }
    else {
        MPI_Recv(buf, message_size, MPI_INT, src, tag, world, &stat);
        printf("rank %i receiving a message\n", me);
    }

    MPI_Finalize();

    return 0;
}

