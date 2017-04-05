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

#define sstmac_app_name mpi_delay_stats

RegisterKeywords("sync_delay");

int USER_MAIN(int argc, char** argv)
{
  //make sure everyone gets here at exatly the same time
  double now = MPI_Wtime();
  sstmac_compute(5e-5 - now);

  MPI_Init(&argc, &argv);

  int me, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &me);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  if (nproc < 8){
    spkt_abort_printf("Test must run with at least 8 ranks");
  }

  double send_delay = get_params()->get_time_param("send_delay");
  double recv_delay = get_params()->get_time_param("recv_delay");
  int send_size = get_params()->get_byte_length_param("message_size");


  int send_to = (me + 1) % nproc;
  int recv_from = (me - 1 + nproc) % nproc;

  if (me % 2 == 0){
    //all even ranks create some delay
    sstmac_compute(send_delay);
    MPI_Send(NULL, send_size, MPI_BYTE, send_to, 42, MPI_COMM_WORLD);
  } else {
    sstmac_compute(recv_delay);
    MPI_Recv(NULL, send_size, MPI_BYTE, recv_from, 42, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  }

  MPI_Finalize();
  return 0;
}



