#include <sstmac/replacements/mpi.h>
#include <sstmac/compute.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords("print_times",
                 "message_size",
                 "sleep_time");

#define sstmac_app_name test_compute_api

int USER_MAIN(int argc, char** argv)
{
  SSTMACBacktrace("main");
  MPI_Init(&argc, &argv);

  int me, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &me);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  int nloop = get_params()->get_optional_int_param("nloop", 100);

  double t_start = MPI_Wtime();

  sstmac_compute_loop(nloop,10,12,48);
  sstmac_compute_loop2(nloop,nloop,2,2,20);
  sstmac_compute_loop3(10,10,nloop,2,1,20);
  sstmac_compute_loop4(3,4,8,nloop,2,7,10);

  double t_stop = MPI_Wtime();
  double t_total = t_stop - t_start;
  ::printf("Rank %d = %8.4fms\n", me, t_total*1e3);

  MPI_Finalize();
  return 0;
}


