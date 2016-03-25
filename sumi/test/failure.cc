#include <sprockit/test/test.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/stl_string.h>
#include <sumi/transport.h>
#include <sumi/dense_rank_map.h>
#include <unistd.h>

#define DEBUG 0

using namespace sumi;


void
run_test(int me, int todie, int nproc_live, int context, int tag, transport* t)
{
  if (t->is_dead())
    return;

  dense_rank_map rmap(t->failed_ranks(context));
  int dense_me = rmap.dense_rank(me);
  if (me == 11){
    printf("Rank 11 maps to dense rank %d\n", dense_me);
  }

  collective_done_message::ptr dmsg;

  int src[] = { dense_me };
  int dst[nproc_live];
  bool resilient = false;
  t->allgather(dst, src, 1, sizeof(int), tag, resilient, context);

  collective::type_t ty = collective::heartbeat;
  while (ty == collective::heartbeat){
    dmsg = ptr_safe_cast(collective_done_message, t->blocking_poll());
    ty = dmsg->type();
  }

  if (!dmsg->succeeded()){
    spkt_throw_printf(sprockit::illformed_error,
        "allgather collective tag=%d failed=%s, should always succeed",
        dmsg->tag(), dmsg->failed_procs().to_string().c_str());
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


  usleep(1000);
  if (me == todie){
    printf("Rank %d going down!\n", me);
    t->die();
  }
  t->vote<AndOp>(1, tag, context);


  ty = collective::heartbeat;
  while (ty == collective::heartbeat){
    dmsg = ptr_safe_cast(collective_done_message, t->blocking_poll());
    ty = dmsg->type();
  }

  if (me == 0){
    const thread_safe_set<int>& failed = t->failed_ranks();
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

  usleep(1000);
}

void
run_test(transport *t)
{
  int me = t->rank();
  //int nproc = t->nproc();

  t->start_heartbeat(100e-1);

  int nproc = t->nproc();
  run_test(me, 1, nproc, options::initial_context, 0, t);

  run_test(me, 4, 11, 0, 1, t);

  run_test(me, 7, 10, 1, 2, t);

  run_test(me, 10, 9, 2, 3, t);

  t->stop_heartbeat();
}

void
run_test()
{
  sprockit::sim_parameters params;
#if DEBUG
  //sprockit::debug::turn_on(DEFAULT_TRANSPORT);
  sprockit::debug::turn_on("sumi_collective");
  sprockit::debug::turn_on("sumi");
  //sprockit::debug::turn_on("sumi_ping");
#endif

  params["transport"] = DEFAULT_TRANSPORT;
  params["lazy_watch"] = "true";
  params["eager_cutoff"] = "0";
  params["use_put_protocol"] = "false";
  params["ping_timeout"] = "10ms";
  transport* t = sumi::transport_factory::get_param("transport", &params);

  t->init();
  int me = t->rank();
  //int nproc = t->nproc();

  try {
    run_test(t);
  } catch (terminate_exception& e) {
    //do nothing - must finalize
    t->block_until_message();
    printf("Rank %d is dead but exiting loop!\n", me);
  }

  printf("Rank %d finalizing\n", me);
  t->finalize();
  printf("Rank %d finalized\n", me);
}

int
main(int argc, char **argv)
{
  try {
    run_test();
  } catch (std::exception& e) {
    std::cerr << e.what() << std::endl;
    abort();
  }
  return 0;
}

