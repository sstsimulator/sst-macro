#include <sprockit/test/test.h>
#include <sstmac/util.h>
#include <sstmac/skeleton.h>
#include <sstmac/compute.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/libraries/sumi/sumi.h>
#include <sumi/dense_rank_map.h>
#include <sumi/thread_safe_set.h>
#include <sumi/transport.h>
#include <sstmac/skeleton.h>
#define sstmac_app_name user_app_cxx
using namespace sstmac;
using namespace sstmac::sw;
using namespace sstmac::hw;
using namespace sumi;

void
run_test()
{
  int tag = 444;
  comm_allgather(NULL, NULL, 1, sizeof(int), tag, true);

  collective_done_message::ptr dmsg = comm_collective_block(collective::allgather, tag);
}

void
run_test(int me, int todie, int nproc_live, int context, int tag)
{
  dense_rank_map rmap(comm_failed_ranks(context));
  int dense_me = rmap.dense_rank(me);
  if (me == 11){
    printf("Rank 11 maps to dense rank %d\n", dense_me);
  }

  collective_done_message::ptr dmsg;

  int src[] = { dense_me };
  int dst[nproc_live];
  comm_allgather(dst, src, 1, sizeof(int), tag, true, context);

  dmsg = comm_collective_block(collective::allgather, tag);
  if (!dmsg->succeeded()){
    spkt_throw(sprockit::illformed_error,
        "allgather collective failed - should always succeed");
  }

  for (int i=0; i < nproc_live; ++i){
    if (dst[i] != i){
      for (int j=0; j < nproc_live; ++j){
        std::cerr << sprockit::printf("A[%d] = %d\n", j, dst[j]);
      }
      spkt_throw_printf(sprockit::value_error,
        "Rank %d got value %d for index %d on test nproc=%d,tag=%d",
        me, dst[i], i, nproc_live, tag);
    }
  }
  printf("Rank %d passed allgather\n", me);


  sstmac_usleep(100);
  if (me == todie){
    printf("Rank %d going down!\n", me);
    comm_kill_node();
  }
  comm_vote<And>(1, tag, context);


  dmsg = comm_collective_block(collective::dynamic_tree_vote, tag);
  if (me == 0){
    const thread_safe_set<int>& failed = comm_failed_ranks();
    thread_safe_set<int>::iterator it, end = failed.start_iteration();
    std::stringstream sstr;
    sstr << "Failed = {";
    for (it = failed.begin(); it != end; ++it){
      sstr << " " << *it;
    }
    failed.end_iteration();
    sstr << " }";
    printf("%s\n", sstr.str().c_str());
  }

  sstmac_usleep(100);
}

int
try_main(int argc, char **argv)
{
  comm_init();

  int me = comm_rank();
  //int nproc = comm_nproc();

  sprockit::sim_parameters* params = sstmac::sw::app::get_params();
  bool heartbeat = params->get_optional_bool_param("heartbeat", false);
  if (heartbeat)
    comm_start_heartbeat(100e-3);

  run_test(me, 1, 12, options::initial_context, 0);

  run_test(me, 4, 11, 0, 1);

  run_test(me, 7, 10, 1, 2);

  run_test(me, 10, 9, 2, 3);

  if (heartbeat)
    comm_stop_heartbeat();

  comm_finalize();

  return 0;
}

#define sstmac_app_name user_app_cxx

int
main(int argc, char** argv)
{
  try {
    return try_main(argc, argv);
  } catch (sumi::terminate_exception& e){
    return 1;
  }
}

