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

  if (me == 3){
    sstmac_compute(sync_delay);
  }
  MPI_Allgather(100, MPI_INT, MPI_COMM_WORLD);

  if (me % 2 == 0){
    //all even ranks create some delay
    sstmac_compute(sync_delay);
  }

  int send_to = (me + 1) % nproc;
  int recv_from = (me - 1 + nproc) % nproc;
  MPI_Request reqs[2];
  MPI_Isend(NULL, 100, MPI_DOUBLE, send_to, 42, MPI_COMM_WORLD, &reqs[0]);
  MPI_Irecv(NULL, 100, MPI_DOUBLE, recv_from, 42, MPI_COMM_WORLD, &reqs[1]);

  if (me % 3 == 0){
    //all 3-div ranks overlap some computation
    sstmac_compute(sync_delay);
  }

  MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE);

  MPI_Finalize();
  return 0;
}



