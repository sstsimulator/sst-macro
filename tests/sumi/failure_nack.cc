#include <sprockit/test/test.h>
#include <sprockit/output.h>
#include <sstmac/util.h>
#include <sstmac/compute.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/libraries/sumi/sumi.h>
#include <sstmac/skeleton.h>
#define sstmac_app_name user_app_cxx
using namespace sumi;


int
main(int argc, char **argv)
{
  comm_init();

  int rank = comm_rank();
  int partner = (rank + 1) % comm_nproc();
  long bytes = 8192;
  for (int i=0; i < 10; i++){
    comm_rdma_get(partner, new message(bytes));
    sstmac_usleep(1);
  }

  for (int i=0; i < 10; i++){
    message::ptr next = comm_poll();
    double now = sstmac_now() * 1e3;
    coutn << sprockit::printf("Rank(%d): At t=%12.8f ms got message of type %s\n",
        rank, now, sumi::message::tostr(next->payload_type()));
  }

  comm_finalize();
  return 0;
}
