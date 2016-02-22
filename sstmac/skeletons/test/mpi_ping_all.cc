#include <sprockit/test/test.h>
#include <sstmac/util.h>
#include <sstmac/replacements/mpi.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/common/logger.h>
#include <sstmac/common/runtime.h>
#include <sstmac/software/process/backtrace.h>

using sstmac::sstmac_runtime;

namespace pingall {

sstmac_register_app(mpi_ping_all);

int
mpi_ping_all_main(int argc, char** argv)
{
  SSTMACBacktrace("main");
  MPI_Init(&argc, &argv);

  double t_start = MPI_Wtime();

  int me, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &me);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  //send/recv from all the other procs
  void* null_buffer = 0;
  sprockit::sim_parameters* params = get_params();
  int count = params->get_optional_byte_length_param("message_size", 100);
  int tag = 42;
  //one for each send, one for each recv
  MPI_Request* reqs = new MPI_Request[2*nproc];
  MPI_Request* reqptr = reqs;
  for (int i=0; i < nproc; ++i) {
    if (i == me) {
      continue;
    }

    MPI_Isend(null_buffer, count, MPI_INT, i, tag, MPI_COMM_WORLD, reqptr);
    ++reqptr;
    MPI_Irecv(null_buffer, count, MPI_INT, i, tag, MPI_COMM_WORLD, reqptr);
    ++reqptr;
  }
  int num_requests = 2*nproc - 2;

  // wait on the first quarter
  int quarter_size =  num_requests / 4;
  int remainder = num_requests % 4;

  sstmac::timestamp sleep_length = params->get_optional_time_param("sleep_time", 1);

  reqptr = reqs;
  for (int q=0; q < 4; ++q) {
    MPI_Waitall(quarter_size, reqptr, MPI_STATUSES_IGNORE);
    reqptr += quarter_size;
    sstmac::sw::operating_system::current_thread()->parent_app()->sleep(sleep_length);
  }

  if (remainder) {
    MPI_Waitall(remainder, reqptr, MPI_STATUSES_IGNORE);
    sstmac::sw::operating_system::current_thread()->parent_app()->sleep(sleep_length);
  }

  double t_stop = MPI_Wtime();
  double t_total = t_stop - t_start;
  if (me == 0){
    ::printf("Runtime = %8.4fms\n", t_total*1e3);
  }

  MPI_Finalize();
  return 0;
}

}

