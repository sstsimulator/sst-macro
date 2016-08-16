#include <sprockit/test/test.h>
#include <sstmac/util.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/libraries/sumi/sumi.h>
#include <sumi/dense_rank_map.h>
#include <sumi/thread_safe_set.h>
#include <sstmac/skeleton.h>
#define sstmac_app_name user_app_cxx
using namespace sstmac;
using namespace sstmac::sw;
using namespace sstmac::hw;
using namespace sumi;


int first_failures[] = {1};
int second_failures[] = {4,7,9};
int third_failures[] = {3,10};
int* failures[] = {first_failures, second_failures, third_failures};
int nfailures[] = {1,3,2};

int
main(int argc, char **argv)
{
  comm_init();

  comm_start_heartbeat(1e-2);

  int me = comm_rank();
  //int nproc = comm_nproc();

  int nfailed = 0;
  int failure_num = 0;
  while (nfailed < 6){
    collective_done_message::ptr dmsg =
        ptr_safe_cast(collective_done_message, comm_poll());
    if (dmsg->type() != collective::heartbeat){
      spkt_throw_printf(sprockit::value_error,
        "got non-heartbeat message %s of type %s",
        dmsg->to_string().c_str(),
        sumi::collective::tostr(dmsg->type()));
    }

    const thread_safe_set<int>& failed = dmsg->failed_procs();

    std::cout << sprockit::printf("t=%8.4e: Rank %d got heartbeat %d with %d failures\n",
                  sstmac_now(), me, dmsg->tag(), failed.size());

    int idx = 0;
    int* correct_failures = failures[failure_num];
    if (nfailures[failure_num] != (int)(failed.size())){
      spkt_throw_printf(sprockit::value_error,
        "Got %d failures, but supposed to be %d failures",
        failed.size(), nfailures[failure_num]);
    }
    thread_safe_set<int>::iterator it, end = failed.start_iteration();
    for (it=failed.begin(); it != end; ++it, ++idx){
      int failed_rank = *it;
      if (correct_failures[idx] != failed_rank){
        failed.end_iteration();
        spkt_throw_printf(sprockit::value_error,
            "Got failure %d, but failure %d was supposed to be rank %d",
            failed_rank, idx, correct_failures[idx]);
      }
    }
    failed.end_iteration();
    ++failure_num;
    nfailed += failed.size();
  }

  comm_stop_heartbeat();

  comm_finalize();

  return 0;
}



