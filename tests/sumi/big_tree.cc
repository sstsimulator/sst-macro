/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sprockit/test/test.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/libraries/sumi/sumi.h>
#define sstmac_app_name user_app_cxx
using namespace sumi;

#include <sprockit/output.h>
#include <sstmac/util.h>
#include <sstmac/compute.h>
#include <sstmac/skeleton.h>

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
test_bcast_payload()
{
  int tag = 718;
  int rank = comm_rank();
  int nproc = comm_nproc();
  int nelems = 100;
  int* buffer = new int[nelems];
  if (rank == 0){
    for (int i=0; i < nelems; ++i){
      buffer[i] = i;
    }
  } else {
    ::memset(buffer, 0, sizeof(int)*nelems);
  }

  int root = 0;
  comm_bcast(root, buffer, nelems, sizeof(int), tag);
  comm_collective_block(collective::bcast, tag);


  bool failed = false;
  for (int i=0; i < nelems; ++i){
    if (buffer[i] != i){
      printf("Rank %d failed: A[%d] = %d\n", rank, i, buffer[i]);
      failed = true;
      break;
    }
  }

  if (!failed){
    printf("Rank %d passed bcast payload test\n", rank);
  }
}

void
test_bcast()
{
  int tag = 717;
  int rank = comm_rank();
  int nproc = comm_nproc();
  int nelems = 10000;
  void* null = 0;

  int root = 0;
  comm_bcast(root, null, nelems, sizeof(int), tag);
  comm_collective_block(collective::bcast, tag);

  std::cout << "t=" << sstmac_now() << ": passed bcast on rank "
      << rank << std::endl;

}


int
main(int argc, char **argv)
{
  comm_init();

  test_bcast();

  test_bcast_payload();

  comm_finalize();

  return 0;
}

