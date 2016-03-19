#include <sprockit/test/test.h>
#include <sprockit/output.h>
#include <sstmac/util.h>
#include <sstmac/compute.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sst/sumi_api.h>
#include <sstmac/common/runtime.h>
#include <sumi/transport.h>

using namespace sumi;

void
test_tiny_allreduce()
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
  int tag = 14;
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
test_allreduce_payload()
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
  int tag = 13;
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
test_allgather_payload(int nelems)
{
  //now do a collective with payloads
  int rank = comm_rank();
  int nproc = comm_nproc();
  int* src_buffer = new int[nelems];
  for (int i=0; i < nelems; ++i){
    src_buffer[i] = rank;
  }

  int* dst_buffer = new int[nproc*nelems];
  int tag = 14 + nelems;
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
test_allreduce()
{
  int tag = 12;
  comm_allreduce<int,Add>(0, 0, 256, tag);

 message::ptr msg = comm_poll();
  std::cout << "Allreduce got " << msg->to_string() << std::endl;
}

void
test_barrier()
{
  int tag = 20;
  int rank = comm_rank();
  //sleep as many seconds as my rank is
  sstmac_sleep(rank);
  //then execute barrier
  comm_barrier(tag);

  message::ptr msg = comm_poll();
  collective_done_message::ptr dmsg = ptr_safe_cast(collective_done_message, msg);
  if (dmsg->tag() != 20 || dmsg->type() != collective::barrier){
    spkt_throw(sprockit::value_error,
      "barrier got invalid completion message");
  }

  std::cout << "t=" << sstmac_now() << ": finished barrier on rank "
    << rank << std::endl;
}


void
test_dynamic_tree_vote()
{
  int tag = 45;
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
  std::cout << "t=" << sstmac_now() << ": passed failed allreduce on rank "
    << rank << std::endl;


  tag = 818;
  comm_allgather(null,null,nelems,sizeof(int),tag,true);
  comm_collective_block(collective::allgather, tag);
  std::cout << "t=" << sstmac_now() << ": passed failed allgather on rank "
    << rank << std::endl;
}


int
main(int argc, char **argv)
{
  comm_init();

  sstmac::runtime::enter_deadlock_region();
  sstmac::runtime::add_deadlock_check(
    sstmac::new_deadlock_check(sumi_api(), &sumi::transport::deadlock_check));

  test_dynamic_tree_vote();

  test_allreduce();

  test_allreduce_payload();

  test_tiny_allreduce();

  test_allgather_payload(1);

  test_allgather_payload(4);

  test_barrier();

  sstmac_sleep(100);
  //test_failed_collectives();

  comm_finalize();
  sstmac::runtime::exit_deadlock_region();
  return 0;
}

