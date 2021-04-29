/**
Copyright 2009-2021 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2021, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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
#include <sprockit/output.h>
#include <sstmac/util.h>
#include <sstmac/compute.h>
#include <sstmac/common/runtime.h>
#include <sstmac/skeleton.h>
#include <sumi/sumi.h>
#include <sumi/transport.h>

#define sstmac_app_name user_app_cxx

using namespace sumi;

void
test_bcast(int tag, int root)
{
  int nelems = 10;
  int rank = comm_rank();
  int nproc = comm_nproc();
  auto* dmsg = comm_bcast(root, NULL, nelems, sizeof(int), tag, sumi::Message::default_cq);
  if (!dmsg){
    dmsg = sumi_engine()->blockUntilNext(Message::default_cq);
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

  auto* dmsg = comm_gather(root, dst_buffer, src_buffer, nelems, sizeof(int), tag, Message::default_cq);
  if (!dmsg){
    dmsg = sumi_engine()->blockUntilNext(Message::default_cq);
  }


  if (rank == root){
    printf("Testing gather on root=%d\n", root);

    int* bufptr = dst_buffer;
    int idx = 0;
    for (int p=0; p < nproc; ++p){
      for (int i=0; i < nelems; ++i, ++bufptr, ++idx){
        int test_elem = *bufptr;
        if (test_elem != p){
          std::cout << sprockit::sprintf("FAILED: allgather rank %d, section %d\n", rank, p);
        }
        std::cout << sprockit::sprintf("T[%d] = %d\n", idx, test_elem);
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

  auto* dmsg = comm_scatter(root, dst_buffer, src_buffer, nelems, sizeof(int), tag, sumi::Message::default_cq);
  if (!dmsg){
    dmsg = sumi_engine()->blockUntilNext(Message::default_cq);
  }

  for (int i=0; i < nelems; ++i){
    int test_elem = dst_buffer[i];
    if (test_elem != rank){
      std::cout << sprockit::sprintf("FAILED: scatter rank %d, A[%d] = %d\n", rank, i, test_elem);
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

  auto* dmsg = comm_allreduce<int,Add>(dst_buffer, src_buffer, nelems, tag, Message::default_cq);
  if (!dmsg){
    dmsg = sumi_engine()->blockUntilNext(Message::default_cq);
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

  auto* dmsg = comm_allreduce<int,Add>(dst_buffer, src_buffer, nelems, tag, Message::default_cq);
  if (!dmsg){
   dmsg = sumi_engine()->blockUntilNext(Message::default_cq);
  }


  if (rank == 0){
    printf("Testing allreduce with payload\n");
    for (int i=0; i < nelems; ++i){
      printf("test[%d] = %d\n", i, dst_buffer[i]);
    }
  }
}

void
test_scan_payload(int tag)
{
  //now do a collective with payloads
  int rank = comm_rank();
  int nproc = comm_nproc();
  int nelems = 3;
  int* src_buffer = new int[nelems];
  ::memset(src_buffer, 0, nelems * sizeof(int));
  for (int i=0; i < nelems; ++i){
    src_buffer[i] = 1;
  }
  int* dst_buffer = new int[nelems];

  auto* dmsg = comm_scan<int,Add>(dst_buffer, src_buffer, nelems, tag, Message::default_cq);
  if (!dmsg){
   dmsg = sumi_engine()->blockUntilNext(Message::default_cq);
  }

  int correct = rank + 1;
  for (int i=0; i < nelems; ++i){
    printf("test[%d][%d] = %d\n", rank, i, dst_buffer[i]);
    if (correct != dst_buffer[i]){
      //spkt_throw_printf(sprockit::value_error,
      //  "scan test: got incorrect value");
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
  auto* dmsg = comm_allgatherv(dst_buffer, src_buffer, recv_counts, sizeof(int), tag, Message::default_cq);
  if (!dmsg){
    dmsg = sumi_engine()->blockUntilNext(Message::default_cq);
  }

  int* bufptr = dst_buffer;
  int idx = 0;
  for (int p=0; p < nproc; ++p){
    for (int i=0; i < (p+1); ++i, ++bufptr, ++idx){
      int test_elem = *bufptr;
      if (test_elem != p){
        std::cout << sprockit::sprintf("FAILED: allgatherv rank %d, section %d\n", rank, p);
      }
      //std::cout << sprockit::sprintf("T[%d][%d] = %d\n", rank, idx, test_elem);
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
  auto* dmsg = comm_allgatherv(dst_buffer, src_buffer, recv_counts, sizeof(int), tag, Message::default_cq);
  if (!dmsg){
    dmsg = sumi_engine()->blockUntilNext(Message::default_cq);
  }

  int* bufptr = dst_buffer;
  int idx = 0;
  for (int p=0; p < nproc; ++p){
    for (int i=0; i < nelems; ++i, ++bufptr, ++idx){
      int test_elem = *bufptr;
      if (test_elem != p){
        std::cout << sprockit::sprintf("FAILED: allgatherv rank %d, section %d\n", rank, p);
      }
      //std::cout << sprockit::sprintf("T[%d][%d] = %d\n", rank, idx, test_elem);
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

  auto* dmsg = comm_reduce<int,Add>(root, dst_buffer, src_buffer, nelems, tag, Message::default_cq);
  if (!dmsg){
    dmsg = sumi_engine()->blockUntilNext(Message::default_cq);
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
  auto* dmsg = comm_allgather(dst_buffer, src_buffer, nelems, sizeof(int), tag, Message::default_cq);
  if (!dmsg){
    dmsg = sumi_engine()->blockUntilNext(Message::default_cq);
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
        std::cout << sprockit::sprintf("FAILED: allgather rank %d, section %d\n", rank, p);
      }
      //std::cout << sprockit::sprintf("T[%d][%d] = %d\n", rank, idx, test_elem);
    }
  }

}

void
test_allreduce(int tag)
{
  auto* dmsg = comm_allreduce<int,Add>(0, 0, 256, tag, Message::default_cq);
  if (!dmsg){
    dmsg = sumi_engine()->blockUntilNext(Message::default_cq);
  }
}

void
test_barrier(int tag)
{
  int rank = comm_rank();
  //sleep as many seconds as my rank is
  sstmac_sleep(rank);
  //then execute barrier
  auto* dmsg = comm_barrier(tag, Message::default_cq);
  if (!dmsg){
    dmsg = sumi_engine()->blockUntilNext(Message::default_cq);
  }
  printf("t=%4.2f finished barrier on rank %d\n", sstmac_now(), rank);
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
  auto* dmsg = comm_alltoall(dst_buffer, src_buffer, nelems, sizeof(int), tag, Message::default_cq);
  if (!dmsg){
    dmsg = sumi_engine()->blockUntilNext(Message::default_cq);
  }

  for (int i=0; i < buffer_size; ++i){
    int partner = i / nelems;
    int elem = me*100 + partner;
    if (dst_buffer[i] != elem){
        std::cout << sprockit::sprintf("FAILED: all-to-all rank %d, partner %d\n", me, partner);
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
  //test_allgatherv_uneven(18);

  test_alltoall(20);

  test_scan_payload(21);

  sstmac_sleep(100);

  comm_finalize();

  return 0;
}
