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
  SSTMACBacktrace("main");
  MPI_Init(&argc, &argv);

  int me, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &me);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  if (nproc < 8){
    spkt_abort_printf("Test must run with at least 8 ranks");
  }

  double sync_delay = get_params()->get_time_param("sync_delay");


  if (me == 6){
    sstmac_compute(sync_delay);
  }
  MPI_Allreduce(1000, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);


  int send_to = (me + 1) % nproc;
  int recv_from = (me - 1 + nproc) % nproc;

  if (me % 2 == 0){
    //all even ranks create some delay
    sstmac_compute(0.5*sync_delay);
    MPI_Send(NULL, 100, MPI_DOUBLE, send_to, 42, MPI_COMM_WORLD);
  } else {
    MPI_Recv(NULL, 100, MPI_DOUBLE, recv_from, 42, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  }



  if (me % 2 == 0){
    //all even ranks create some delay
    MPI_Send(NULL, 100000, MPI_DOUBLE, send_to, 42, MPI_COMM_WORLD);
  } else {
    sstmac_compute(sync_delay);
    MPI_Recv(NULL, 100000, MPI_DOUBLE, recv_from, 42, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  }

  MPI_Finalize();
  return 0;
}



