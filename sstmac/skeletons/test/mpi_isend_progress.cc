#include <sprockit/test/test.h>
#include <sstmac/util.h>
#include <sstmac/compute.h>
#include <sstmac/replacements/mpi.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/common/runtime.h>
#include <sstmac/software/process/backtrace.h>
#include <sumi-mpi/mpi_api.h>
#include <sstmac/skeleton.h>
#include <sprockit/keyword_registration.h>

#define sstmac_app_name mpi_progress


int USER_MAIN(int argc, char** argv)
{
  SSTMACBacktrace("main");
  MPI_Init(&argc, &argv);

  int me, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &me);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  int send_size = get_params()->get_byte_length_param("send_size");
  double send_delay = get_params()->get_time_param("send_delay");
  int num_sends = get_params()->get_int_param("num_sends");
  int send_to = (me + 1) % nproc;
  int recv_from = (me - 1 + nproc) % nproc;
  MPI_Request send_reqs[10];
  MPI_Request recv_reqs[10];
  for (int i=0; i < num_sends; ++i){
    MPI_Irecv(NULL, send_size, MPI_BYTE, recv_from, 42, MPI_COMM_WORLD, &recv_reqs[i]);
  }

  for (int i=0; i < num_sends; ++i){
    sstmac_compute(send_delay);
    MPI_Isend(NULL, send_size, MPI_BYTE, send_to, 42, MPI_COMM_WORLD, &send_reqs[i]);
  }

  MPI_Waitall(num_sends, recv_reqs, MPI_STATUSES_IGNORE);
  MPI_Waitall(num_sends, send_reqs, MPI_STATUSES_IGNORE);

  MPI_Finalize();
  return 0;
}



