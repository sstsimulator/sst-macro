/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

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
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/skeleton.h>
#include <sumi/dense_rank_map.h>
#include <sumi/transport.h>
#include <sumi/sumi.h>

#define sstmac_app_name user_app_cxx
using namespace sumi;

void
run_test(Communicator* dom, int todie, int nproc_live, int context, int tag)
{
  int me = comm_rank();
  auto known_failures = comm_failed_ranks(context);
  DenseRankMap rmap(known_failures);
  int dense_me = rmap.denseRank(me);
  if (me == 11){
    printf("Rank 11 maps to dense rank %d with failures = %s\n",
      dense_me, known_failures.toString().c_str());
  }

  CollectiveDoneMessage* dmsg;

  int src[] = { dense_me };
  int dst[nproc_live];
  comm_allgather(dst, src, 1, sizeof(int), tag, true, context, dom);

  dmsg = comm_collective_block(Collective::allgather, tag);
  if (!dmsg->succeeded()){
    spkt_throw_printf(sprockit::IllformedError,
        "allgather collective failed with failures %s, should always succeed",
        dmsg->failed_procs().toString().c_str());
  }

  DenseRankMap domain_rmap(known_failures, dom);
  for (int i=0; i < nproc_live; ++i){
    int sparse_rank = domain_rmap.sparseRank(i);
    int global_rank = dom->commToGlobalRank(sparse_rank);
    int correct = rmap.denseRank(global_rank);
    if (dst[i] != correct){
      for (int j=0; j < nproc_live; ++j){
        std::cerr << sprockit::sprintf("A[%d] = %d\n", j, dst[j]);
      }
      spkt_throw_printf(sprockit::ValueError,
        "Rank %d A[%d] = %d != %d for global rank %d, sparse rank %d on test nproc=%d,tag=%d",
        me, i, dst[i], correct, global_rank, sparse_rank, nproc_live, tag);
    }
  }
  printf("Rank %d passed allgather\n", me);

  sstmac_usleep(100);
}

void
test_allreduce(Communicator* dom, int tag)
{
  //now do a collective with payloads
  int rank = dom->myCommRank();
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

  Message* msg = comm_poll(); //wait on allreduce
  if (msg->classType() != Message::collective_done){
    spkt_throw_printf(sprockit::ValueError,
      "allreduce test: expected collective message, but got %s",
      Message::tostr(msg->classType()));
  }

  if (rank == 0){
    printf("Testing allreduce with payload\n");
    for (int i=0; i < nelems; ++i){
      printf("test[%d] = %d\n", i, dst_buffer[i]);
    }
  }
}

void
test_allgather(Communicator* dom, int tag)
{
  int nelems = 10;

  int rank = dom->myCommRank();
  int nproc = dom->nproc();

  int* src_buffer = new int[nelems];
  for (int i=0; i < nelems; ++i){
    src_buffer[i] = rank;
  }



  int* dst_buffer = new int[nproc*nelems];

  comm_allgather(dst_buffer, src_buffer, nelems, sizeof(int), tag, false, options::initial_context, dom);

  Message* msg = comm_poll(); //wait on allgather
  if (msg->classType() != Message::collective_done){
    spkt_throw_printf(sprockit::ValueError,
      "allreduce test: expected collective message, but got %s",
      Message::tostr(msg->classType()));
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
        std::cout << sprockit::sprintf("FAILED: allgather rank %d, section %d\n",
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
    Communicator* dom = new SubrangeCommunicator(rank, start, nsubrange);
    //test_allgather(dom, 0);
    //test_allreduce(dom, 1);
  }

  Communicator* dom = new RotateCommunicator(rank, nproc, 3);
  //test_allgather(dom, 2);
  //test_allreduce(dom, 3);


  run_test(dom, 1, 12, options::initial_context, 4);

  run_test(dom, 4, 11, 4, 5);

  run_test(dom, 7, 10, 5, 6);

  run_test(dom, 10, 9, 6, 7);

  comm_finalize();

  return 0;
}

