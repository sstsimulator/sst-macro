#include <sprockit/test/test.h>
#include <sprockit/output.h>
#include <sstmac/util.h>
#include <sstmac/compute.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/libraries/sumi/sumi.h>
#include <sstmac/common/runtime.h>
#include <sumi/transport.h>
#include <sstmac/skeleton.h>

#define sstmac_app_name user_app_cxx

using namespace sumi;

void
test_bcast(int tag, int root)
{
  int nelems = 10;
  int rank = comm_rank();
  int nproc = comm_nproc();
  comm_bcast(root, NULL, nelems, sizeof(int), tag);

  message::ptr msg = comm_poll();
  if (msg->class_type() != message::collective_done){
    spkt_throw_printf(sprockit::value_error,
      "allreduce test: expected collective message, but got %s",
      message::tostr(msg->class_type()));
  }

  if (rank == root){
    //printf("Testing tiny allreduce\n");
    //for (int i=0; i < nelems; ++i){
    //  printf("test[%d] = %d\n", i, dst_buffer[i]);
    //}
  }
}

void
test_gather(int tag, int root)
{
  int nelems = 2;
  int rank = comm_rank();
  int nproc = comm_nproc();

  int* src_buffer = new int[nelems];
  for (int i=0; i < nelems; ++i){
    src_buffer[i] = rank;
  }

  int* dst_buffer = 0;
  if (rank == root) dst_buffer = new int[nproc*nelems];

  comm_gather(root, dst_buffer, src_buffer, nelems, sizeof(int), tag);

  message::ptr msg = comm_poll();
  if (msg->class_type() != message::collective_done){
    spkt_throw_printf(sprockit::value_error,
      "allreduce test: expected collective message, but got %s",
      message::tostr(msg->class_type()));
  }

  if (rank == root){
    printf("Testing gather on root=%d\n", root);

    int* bufptr = dst_buffer;
    int idx = 0;
    for (int p=0; p < nproc; ++p){
      for (int i=0; i < nelems; ++i, ++bufptr, ++idx){
        int test_elem = *bufptr;
        if (test_elem != p){
          std::cout << sprockit::printf("FAILED: allgather rank %d, section %d\n", rank, p);
        }
        std::cout << sprockit::printf("T[%d] = %d\n", idx, test_elem);
      }
    }
  }

}


void
test_scatter(int tag, int root)
{
  int nelems = 2;
  int rank = comm_rank();
  int nproc = comm_nproc();
  int ntotal = nelems*nproc;

  int *src_buffer = 0, *dst_buffer = 0;
  if (rank == root){
    src_buffer = new int[ntotal];
    for (int i=0; i < ntotal; ++i){
      src_buffer[i] = i / nelems;
    }
  }
  dst_buffer = new int[nelems];

  if (rank == root)
    printf("Testing scatter on root=%d\n", root);

  comm_scatter(root, dst_buffer, src_buffer, nelems, sizeof(int), tag);

  message::ptr msg = comm_poll();
  if (msg->class_type() != message::collective_done){
    spkt_throw_printf(sprockit::value_error,
      "allreduce test: expected collective message, but got %s",
      message::tostr(msg->class_type()));
  }

  for (int i=0; i < nelems; ++i){
    int test_elem = dst_buffer[i];
    if (test_elem != rank){
      std::cout << sprockit::printf("FAILED: scatter rank %d, A[%d] = %d\n", rank, i, test_elem);
    }
  }

}


void
test_tiny_allreduce(int tag)
{
  //now do a collective with payloads
  int rank = comm_rank();
  int nproc = comm_nproc();
  int nelems = nproc-1;
  int* src_buffer = new int[nelems];
  ::memset(src_buffer, 0, nelems * sizeof(int));
  int num_ones = std::min(nelems-1,rank);
  for (int i=0; i <= num_ones; ++i){
    src_buffer[i] = 1;
  }
  int* dst_buffer = new int[nelems];

  comm_allreduce<int,Add>(dst_buffer, src_buffer, nelems, tag);

  message::ptr msg = comm_poll(); //wait on allreduce
  if (msg->class_type() != message::collective_done){
    spkt_throw_printf(sprockit::value_error,
      "allreduce test: expected collective message, but got %s",
      message::tostr(msg->class_type()));
  }

  if (rank == 0){
    printf("Testing tiny allreduce\n");
    for (int i=0; i < nelems; ++i){
      printf("test[%d] = %d\n", i, dst_buffer[i]);
    }
  }
}

void
test_allreduce_payload(int tag)
{
  //now do a collective with payloads
  int rank = comm_rank();
  int nproc = comm_nproc();
  int nelems = 2*nproc;
  int numfill = 2*rank + 1;
  int* src_buffer = new int[nelems];
  ::memset(src_buffer, 0, nelems * sizeof(int));
  for (int i=0; i <= numfill; ++i){
    src_buffer[i] = 1;
  }
  int* dst_buffer = new int[nelems];

  comm_allreduce<int,Add>(dst_buffer, src_buffer, nelems, tag);

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
test_allgatherv_uneven(int tag)
{

  //now do a collective with payloads
  int rank = comm_rank();
  int nproc = comm_nproc();
  int nelems = rank + 1;
  int* src_buffer = new int[nelems];
  for (int i=0; i < nelems; ++i){
    src_buffer[i] = rank;
  }

  int* recv_counts = new int[nproc];
  for (int i=0; i < nproc; ++i){
    recv_counts[i] = i+1;
  }

  int ntotal = nproc*(nproc+1) / 2;
  int* dst_buffer = new int[ntotal];
  comm_allgatherv(dst_buffer, src_buffer, recv_counts, sizeof(int), tag);

  message::ptr msg = comm_poll(); //wait on allgather
  if (msg->class_type() != message::collective_done){
    spkt_throw_printf(sprockit::value_error,
      "allreduce test: expected collective message, but got %s",
      message::tostr(msg->class_type()));
  }

  int* bufptr = dst_buffer;
  int idx = 0;
  for (int p=0; p < nproc; ++p){
    for (int i=0; i < (p+1); ++i, ++bufptr, ++idx){
      int test_elem = *bufptr;
      if (test_elem != p){
        std::cout << sprockit::printf("FAILED: allgatherv rank %d, section %d\n", rank, p);
      }
      //std::cout << sprockit::printf("T[%d][%d] = %d\n", rank, idx, test_elem);
    }
  }

  if (rank == 0){
    printf("Finished uneven allgatherv on tag %d\n", tag);
  }
}

void
test_allgatherv_even(int tag)
{
  //now do a collective with payloads
  int rank = comm_rank();
  int nproc = comm_nproc();
  int nelems = 3;
  int* src_buffer = new int[nelems];
  for (int i=0; i < nelems; ++i){
    src_buffer[i] = rank;
  }

  int* recv_counts = new int[nproc];
  for (int i=0; i < nproc; ++i){
    recv_counts[i] = nelems;
  }

  int* dst_buffer = new int[nproc*nelems];
  comm_allgatherv(dst_buffer, src_buffer, recv_counts, sizeof(int), tag);

  message::ptr msg = comm_poll(); //wait on allgather
  if (msg->class_type() != message::collective_done){
    spkt_throw_printf(sprockit::value_error,
      "allreduce test: expected collective message, but got %s",
      message::tostr(msg->class_type()));
  }

  int* bufptr = dst_buffer;
  int idx = 0;
  for (int p=0; p < nproc; ++p){
    for (int i=0; i < nelems; ++i, ++bufptr, ++idx){
      int test_elem = *bufptr;
      if (test_elem != p){
        std::cout << sprockit::printf("FAILED: allgatherv rank %d, section %d\n", rank, p);
      }
      //std::cout << sprockit::printf("T[%d][%d] = %d\n", rank, idx, test_elem);
    }
  }

  if (rank == 0){
    printf("Finished even allgatherv on tag %d\n", tag);
  }
}

void
test_reduce(int tag, int root)
{
  //now do a collective with payloads
  int rank = comm_rank();
  int nproc = comm_nproc();
  int nelems = 2*nproc;
  int numfill = 2*rank + 1;

  int* src_buffer = new int[nelems];
  ::memset(src_buffer, 0, nelems * sizeof(int));
  for (int i=0; i <= numfill; ++i){
    src_buffer[i] = 1;
  }
  int* dst_buffer = 0;
  if (rank == root) dst_buffer = new int[nelems];

  comm_reduce<int,Add>(root, dst_buffer, src_buffer, nelems, tag);

  message::ptr msg = comm_poll(); //wait on reduce

  if (msg->class_type() != message::collective_done){
    spkt_throw_printf(sprockit::value_error,
      "allreduce test: expected collective message, but got %s",
      message::tostr(msg->class_type()));
  }

  if (rank == root){
    printf("Testing reduce root=%d with payload\n", root);
    for (int i=0; i < nelems; ++i){
      printf("test[%d] = %d\n", i, dst_buffer[i]);
    }
  }

}

void
test_allgather_payload(int tag)
{
  //now do a collective with payloads
  int rank = comm_rank();
  int nproc = comm_nproc();
  int nelems = 2;
  int* src_buffer = new int[nelems];
  for (int i=0; i < nelems; ++i){
    src_buffer[i] = rank;
  }

  int* dst_buffer = new int[nproc*nelems];
  comm_allgather(dst_buffer, src_buffer, nelems, sizeof(int), tag);

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
        std::cout << sprockit::printf("FAILED: allgather rank %d, section %d\n", rank, p);
      }
      //std::cout << sprockit::printf("T[%d][%d] = %d\n", rank, idx, test_elem);
    }
  }

}

void
test_allreduce(int tag)
{
  comm_allreduce<int,Add>(0, 0, 256, tag);

 message::ptr msg = comm_poll();
  std::cout << "Allreduce got " << msg->to_string() << std::endl;
}

void
test_barrier(int tag)
{
  int rank = comm_rank();
  //sleep as many seconds as my rank is
  sstmac_sleep(rank);
  //then execute barrier
  comm_barrier(tag);

  message::ptr msg = comm_poll();
  collective_done_message::ptr dmsg = ptr_safe_cast(collective_done_message, msg);
  if (dmsg->tag() != tag || dmsg->type() != collective::barrier){
    spkt_throw(sprockit::value_error,
      "barrier got invalid completion message");
  }

  printf("t=%4.2f finished barrier on rank %d\n", sstmac_now(), rank);
}


void
test_dynamic_tree_vote(int tag)
{
  int vote = comm_rank() * 2;
  int answer = (comm_nproc()-1) * 2;
  comm_vote<Max>(vote, tag);

  message::ptr msg = comm_poll();
  collective_done_message_ptr dmsg = ptr_safe_cast(collective_done_message, msg);
  if (dmsg->tag() != tag || dmsg->type() != collective::dynamic_tree_vote){
    spkt_throw(sprockit::value_error,
      "vote got invalid completion message");
  }

  if (dmsg->vote() != answer){
    cerrn << sprockit::printf("got final vote %d on rank %d, but answer is %d\n",
        dmsg->vote(), comm_rank(), answer);
  }

}

void
test_failed_collectives()
{

  int tag = 717;
  int rank = comm_rank();
  int nelems = 10000;
  void* null = 0;

  comm_allreduce<char,Add>(null,null,nelems,tag,true);
  comm_collective_block(collective::allreduce, tag);
  printf("t=%6.2f: passed failed allreduce on rank %d\n", sstmac_now(), rank);


  tag = 818;
  comm_allgather(null,null,nelems,sizeof(int),tag,true);
  comm_collective_block(collective::allgather, tag);
  printf("t=%6.2f: passed failed allgather on rank %d\n", sstmac_now(), rank);
}

void
test_alltoall(int tag)
{
  int nproc = comm_nproc();
  int me = comm_rank();
  int nelems = 2;
  int buffer_size = nelems*nproc;
  int* src_buffer = new int[buffer_size];
  int* dst_buffer = new int[buffer_size];
  for (int i=0; i < buffer_size; ++i){
    int partner = i / nelems;
    int elem = partner*100 + me;
    src_buffer[i] = elem;
  }

  //the all-to-all should accumulate it
  comm_alltoall(dst_buffer, src_buffer, nelems, sizeof(int), tag);

  message::ptr msg = comm_poll(); //wait on allgather
  if (msg->class_type() != message::collective_done){
    spkt_throw_printf(sprockit::value_error,
      "all-to-all test: expected collective message, but got %s",
      message::tostr(msg->class_type()));
  }

  for (int i=0; i < buffer_size; ++i){
    int partner = i / nelems;
    int elem = me*100 + partner;
    if (dst_buffer[i] != elem){
        std::cout << sprockit::printf("FAILED: all-to-all rank %d, partner %d\n", me, partner);
    }
  }

  if (me == 0){
    printf("Finished alltoall on tag %d\n", tag);
  }
}

int
main(int argc, char **argv)
{
  comm_init();

  sstmac::runtime::enter_deadlock_region();
  sstmac::runtime::add_deadlock_check(
    sstmac::new_deadlock_check(sumi_api(), &sumi::transport::deadlock_check));

  test_dynamic_tree_vote(1);

  test_allreduce(2);

  test_allreduce_payload(3);

  test_tiny_allreduce(4);

  test_allgather_payload(5);

  test_allgather_payload(6);

  test_barrier(8);

  test_bcast(9, 0);

  test_bcast(10, 3);

  test_gather(11, 0);

  test_gather(12, 3);

  test_reduce(13, 0);

  test_reduce(14, 3);

  test_scatter(15, 0);

  test_scatter(16, 3);

  test_allgatherv_even(17);
  test_allgatherv_uneven(18);

  test_alltoall(20);


  sstmac_sleep(100);
  //test_failed_collectives();

  comm_finalize();
  sstmac::runtime::exit_deadlock_region();
  return 0;
}

