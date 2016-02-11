#include <sprockit/test/test.h>
#include <sprockit/output.h>
#include <sstmac/common/logger.h>
#include <sstmac/common/runtime.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/util.h>
#include <sstmac/replacements/mpi.h>


namespace sstmac {

sstmac_register_app(pdes_test);

static void
do_sleep(timestamp t){
    sstmac::sw::operating_system::current_thread()->parent_app()->sleep(t);
}

int
pdes_test_main(int argc, char** argv)
{
  coutn << sprockit::printf("Starting app\n");
  MPI_Init(&argc, &argv);

  int me, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &me);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  //we should only have 2 procs
  if (nproc != 2){
    spkt_throw_printf(sprockit::illformed_error,
        "pdes_test: should have 2 procs, have %d",
        nproc);
  }

  timestamp sleep_time = sstmac::sstmac_env::params->get_optional_time_param("sleep_time", 10e-9);
  int num_sleeps = sstmac_env::params->get_optional_int_param("num_sleeps", 10);
  int niter = sstmac_env::params->get_optional_int_param("num_iter", 10);

  int message_count = sstmac_env::params->get_optional_int_param("message_count", 1000);
  int num_messages = sstmac_env::params->get_optional_int_param("num_messages", 3);

  MPI_Request* reqs = new MPI_Request[2*num_messages];
  MPI_Request* reqptr = reqs;

  int partner = me == 0 ? 1 : 0;

  for (int i=0; i < niter; ++i){
    int tag = i;
    //fire off a bunch of messages
    coutn << sprockit::printf("Rank %d starting iteration %d\n", me, i);

    reqptr = reqs;
    for (int m=0; m < num_messages; ++m){
      coutn << sprockit::printf("Rank %d, send-recv message %d\n", me, m);
      MPI_Isend(0, message_count, MPI_INT, partner, tag, MPI_COMM_WORLD, reqptr);
      ++reqptr;
      MPI_Irecv(0, message_count, MPI_INT, partner, tag, MPI_COMM_WORLD, reqptr);
      ++reqptr;
    }

    coutn << sprockit::printf("Rank %d done sending messages\n", me);

    for (int s=0; s < num_sleeps; ++s){
      coutn << sprockit::printf("Rank %d, sleep interval %d\n", me, s);
      do_sleep(sleep_time);
    }

    coutn << sprockit::printf("Rank %d done sleeping\n", me);

    MPI_Waitall(2*num_messages, reqs, MPI_STATUSES_IGNORE);
    coutn << sprockit::printf("Rank %d done waiting on messages\n", me);
  }

  MPI_Finalize();
  return 0;
}

}


