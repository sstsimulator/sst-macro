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
#include <sumi/dense_rank_map.h>
#include <sumi/transport.h>
#include <sumi/thread_safe_set.h>

#include <sstmac/util.h>
#include <sstmac/compute.h>
#include <sstmac/skeleton.h>
#include <sprockit/output.h>

#define sstmac_app_name user_app_cxx
using namespace sstmac;
using namespace sstmac::sw;
using namespace sstmac::hw;
using namespace sumi;


void
run_test(communicator* dom, int todie, int nproc_live, int context, int tag)
{
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

