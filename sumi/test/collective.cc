
#include <pthread.h>
#include <sumi/transport.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>

#define DEBUG 0

using namespace sumi;

void*
msg_thread_run(void* args);

static inline int
val(int rank, int idx){
  return idx*10 + rank;
}

void
run_test()
{
  sprockit::sim_parameters params;
  params["ping_timeout"] = "100ms";
  params["transport"] = DEFAULT_TRANSPORT;
  params["eager_cutoff"] = "0";
  transport* t = transport_factory::get_param("transport", &params);

  t->init();

  int me = t->rank();
  int nproc = t->nproc();

  int* reduce_buf = new int[nproc];
  ::memset(reduce_buf, 0, nproc*sizeof(int));
  reduce_buf[me] = me;

  //printf("Rank %d initialized with reduce buffer %p\n",
  //  t->rank(), reduce_buf);

  t->allreduce<int,Add>(reduce_buf,reduce_buf,nproc,0);
  message::ptr msg = t->blocking_poll();

  int* gather_buf = new int[nproc];
  ::memset(gather_buf, 0, nproc*sizeof(int));
  t->allgather(gather_buf, &me, 1, sizeof(int), 1);
  msg = t->blocking_poll();

  for (int i=0; i < nproc; ++i){
    if (reduce_buf[i] != i){
      std::cerr << sprockit::printf("Rank %d: reduce buf[%d] = %d != %d\n",
        me, i, reduce_buf[i], i);
      abort();
    }
    if (gather_buf[i] != i){
      std::cerr << sprockit::printf("Rank %d: gather buf[%d] = %d != %d\n",
        me, i, gather_buf[i], i);
      abort();
    }
  }

  std::cout << "All tests passed on rank " << me << std::endl;

  //t->stop_heartbeat();
  t->finalize();
}

int main(int argc, char** argv)
{
  try {
#if DEBUG
  sprockit::debug::turn_on(DEFAULT_TRANSPORT);
  sprockit::debug::turn_on("sumi");
  sprockit::debug::turn_on("sumi_collective");
#endif
    run_test();
  } catch (std::exception& e) {
    std::cerr << e.what() << std::endl;
    abort();
  }

  return 0;
}



