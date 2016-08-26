#include <sprockit/test/test.h>
#include <sprockit/output.h>
#include <sstmac/util.h>
#include <sstmac/compute.h>
#include <sstmac/common/runtime.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sumi/transport.h>
#include <sstmac/libraries/sumi/sumi.h>
#include <sstmac/skeleton.h>
#define sstmac_app_name user_app_cxx
using namespace sumi;

void
start_allreduce()
{
  int tag = 12;
  comm_allreduce<int,Add>(0, 0, 256, tag);
}

void
start_barrier()
{
  int tag = 20;
  //then execute barrier
  comm_barrier(tag);
}

int
main(int argc, char **argv)
{
  comm_init();

  sumi::transport* tport = sumi_api();
  sstmac::runtime::add_deadlock_check(
    sstmac::new_deadlock_check(tport, &sumi::transport::deadlock_check));

  int me = comm_rank();
  if (me == 0) sstmac::runtime::enter_deadlock_region();

  if (me != 5){
    start_barrier();
    start_allreduce();
  }
  message::ptr msg = comm_poll();
  msg = comm_poll();

  comm_finalize();

  if (me == 0) sstmac::runtime::exit_deadlock_region();

  return 0;
}

