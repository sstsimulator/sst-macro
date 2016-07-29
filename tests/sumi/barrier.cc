#include <sprockit/test/test.h>
#include <sprockit/output.h>
#include <sstmac/util.h>
#include <sstmac/skeleton.h>
#include <sstmac/compute.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/libraries/sumi/sumi.h>
#include <sstmac/libraries/sumi/sumi_transport.h>
#include <sstmac/common/runtime.h>
#include <sumi/transport.h>
#define sstmac_app_name user_app_cxx
using namespace sumi;

void
run_test(transport* tport, int tag)
{
  tport->barrier(tag);
  collective_done_message::ptr msg = tport->collective_block(collective::barrier, tag);
  if (tport->rank() == 0){
    printf("Cleared barrier %d\n", tag);
  }
}

int
main(int argc, char **argv)
{
  transport* tport = sumi_api();
  tport->init();

  sstmac::runtime::add_deadlock_check(
    sstmac::new_deadlock_check(sumi_api(), &sumi::transport::deadlock_check));

  sstmac::runtime::enter_deadlock_region();
  run_test(tport,0);
  run_test(tport,1);
  run_test(tport,2);
  tport->finalize();
  sstmac::runtime::exit_deadlock_region();
  return 0;
}

