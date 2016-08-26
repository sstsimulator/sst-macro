#include <sprockit/test/test.h>
#include <sprockit/output.h>
#include <sstmac/util.h>
#include <sstmac/compute.h>
#include <sstmac/skeleton.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/libraries/sumi/sumi.h>
#include <sumi/dense_rank_map.h>
#include <sumi/transport.h>
#include <sumi/thread_safe_set.h>
#define sstmac_app_name user_app_cxx
using namespace sstmac;
using namespace sstmac::sw;
using namespace sstmac::hw;
using namespace sumi;


void
run_test(communicator* dom, int todie, int nproc_live, int context, int tag)
{
}

int
main(int argc, char **argv)
{
  comm_init();

  //now do a collective with payloads
  int rank = comm_rank();
  int nproc = comm_nproc();

  int start = 2, nsubrange = 4;
  int stop = start + nsubrange;

  if (rank >= start && rank < stop){
    communicator* dom = new subrange_communicator(rank, start, nsubrange);
    //test_allgather(dom, 0);
    //test_allreduce(dom, 1);
  }

  communicator* dom = new rotate_communicator(rank, nproc, 3);
  //test_allgather(dom, 2);
  //test_allreduce(dom, 3);


  run_test(dom, 1, 12, options::initial_context, 4);

  run_test(dom, 4, 11, 4, 5);

  run_test(dom, 7, 10, 5, 6);

  run_test(dom, 10, 9, 6, 7);

  comm_finalize();

  return 0;
}


