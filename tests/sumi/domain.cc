#include <sprockit/test/test.h>
#include <sprockit/output.h>
#include <sstmac/util.h>
#include <sstmac/compute.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/libraries/sumi/sumi.h>
#include <sumi/dense_rank_map.h>
#include <sumi/transport.h>
#include <sumi/thread_safe_set.h>
#include <sstmac/skeleton.h>
#define sstmac_app_name user_app_cxx
using namespace sumi;

void
run_test(communicator* dom, int todie, int nproc_live, int context, int tag)
{
  int me = comm_rank();
  thread_safe_set<int> known_failures = comm_failed_ranks(context);
  dense_rank_map rmap(known_failures);
  int dense_me = rmap.dense_rank(me);
  if (me == 11){
    printf("Rank 11 maps to dense rank %d with failures = %s\n",
      dense_me, known_failures.to_string().c_str());
  }

  collective_done_message::ptr dmsg;

  int src[] = { dense_me };
  int dst[nproc_live];
  comm_allgather(dst, src, 1, sizeof(int), tag, true, context, dom);

  dmsg = comm_collective_block(collective::allgather, tag);
  if (!dmsg->succeeded()){
    spkt_throw_printf(sprockit::illformed_error,
        "allgather collective failed with failures %s, should always succeed",
        dmsg->failed_procs().to_string().c_str());
  }

  dense_rank_map domain_rmap(known_failures, dom);
  for (int i=0; i < nproc_live; ++i){
    int sparse_rank = domain_rmap.sparse_rank(i);
    int global_rank = dom->comm_to_global_rank(sparse_rank);
    int correct = rmap.dense_rank(global_rank);
    if (dst[i] != correct){
      for (int j=0; j < nproc_live; ++j){
        std::cerr << sprockit::printf("A[%d] = %d\n", j, dst[j]);
      }
      spkt_throw_printf(sprockit::value_error,
        "Rank %d A[%d] = %d != %d for global rank %d, sparse rank %d on test nproc=%d,tag=%d",
        me, i, dst[i], correct, global_rank, sparse_rank, nproc_live, tag);
    }
  }
  printf("Rank %d passed allgather\n", me);


  sstmac_usleep(100);
  if (me == todie){
    printf("Rank %d going down!\n", me);
    comm_kill_node();
  }
  comm_vote<And>(1, tag, context, dom);


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

void
test_allreduce(communicator* dom, int tag)
{
  //now do a collective with payloads
  int rank = dom->my_comm_rank();
  int nproc = dom->nproc();
  int nelems = 2*nproc;
  int numfill = 2*rank + 1;
  int* src_buffer = new int[nelems];
  ::memset(src_buffer, 0, nelems * sizeof(int));
  for (int i=0; i <= numfill; ++i){
    src_buffer[i] = 1;
  }
  int* dst_buffer = new int[nelems];
  comm_allreduce<int,Add>(dst_buffer, src_buffer, nelems, tag, false, options::initial_context, dom);

  message::ptr msg = comm_poll(); //wait on allreduce
  if (msg->class_type() != message::collective_done){
    spkt_throw_printf(sprockit::value_error,
      "allreduce test: expected collective message, but got %s",
      message::tostr(msg->class_type()));
  }

  if (rank == 0){
    printf("Testing allreduce with payload\n");
    for (int i=0; i < nelems; ++i){
      printf("test[%d] = %d\n", i, dst_buffer[i]);
    }
  }
}

void
test_allgather(communicator* dom, int tag)
{
  int nelems = 10;

  int rank = dom->my_comm_rank();
  int nproc = dom->nproc();

  int* src_buffer = new int[nelems];
  for (int i=0; i < nelems; ++i){
    src_buffer[i] = rank;
  }



  int* dst_buffer = new int[nproc*nelems];

  comm_allgather(dst_buffer, src_buffer, nelems, sizeof(int), tag, false, options::initial_context, dom);

  message::ptr msg = comm_poll(); //wait on allgather
  if (msg->class_type() != message::collective_done){
    spkt_throw_printf(sprockit::value_error,
      "allreduce test: expected collective message, but got %s",
      message::tostr(msg->class_type()));
  }

  if (rank == 0){
    std::cout << "Testing allgather payload with " << nelems << " elements\n";
  }

  int* bufptr = dst_buffer;
  int idx = 0;
  for (int p=0; p < nproc; ++p){
    for (int i=0; i < nelems; ++i, ++bufptr, ++idx){
      int test_elem = *bufptr;
      if (test_elem != p){
        std::cout << sprockit::printf("FAILED: allgather rank %d, section %d\n",
            rank, p);
      }
      //std::cout << sprockit::spktprintf("T[%d][%d] = %d\n", rank, idx, test_elem);
    }
  }

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


