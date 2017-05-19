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
#include <sprockit/output.h>
#include <sstmac/util.h>
#include <sstmac/compute.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/libraries/sumi/sumi.h>
#include <sstmac/skeleton.h>
#define sstmac_app_name user_app_cxx
using namespace sumi;

int indices[] = { 0,1,0,1,2,1 };
int ndomain = 6;

void
test_allreduce(int cm_rank)
{
  //now do a collective with payloads
  int rank = comm_rank();
  printf("Starting allreduce collective for domain rank %d, physical rank %d\n",
    cm_rank, rank);
  int nproc = comm_nproc();
  int nelems = 2*nproc;
  int numfill = 2*rank + 1;
  int* src_buffer = new int[nelems];
  ::memset(src_buffer, 0, nelems * sizeof(int));
  src_buffer[cm_rank] = rank;
  int* dst_buffer = new int[nelems];
  int tag = 13;
  communicator* dom = new index_communicator(cm_rank, ndomain, indices);
  bool fault_aware = false;
  int context = options::initial_context;
  comm_allreduce<int,Add>(dst_buffer, src_buffer, nelems, tag, fault_aware, context, dom);
}


void
wait_allreduce(int cm_rank)
{
  printf("Waiting on allreduce collective for domain rank %d, physical rank %d\n",
    cm_rank, comm_rank());

  message::ptr msg = comm_poll(); //wait on allreduce
  if (msg->class_type() != message::collective_done){
    spkt_throw_printf(sprockit::value_error,
      "allreduce test: expected collective message, but got %s",
      message::tostr(msg->class_type()));
  }

  collective_done_message::ptr dmsg = ptr_safe_cast(collective_done_message, msg);
  int* dst_buffer = (int*) dmsg->result();
  int nelems = 2*comm_nproc();
  if (cm_rank == 0){
    for (int i=0; i < nelems; ++i){
      printf("test[%d] = %d\n", i, dst_buffer[i]);
    }
  }
}
      


int
main(int argc, char **argv)
{
  comm_init();

  int me = comm_rank();
  for (int i=0; i < ndomain; ++i){
    if (indices[i] == me){
      test_allreduce(i);
    }
  }

  for (int i=0; i < ndomain; ++i){
    if (indices[i] == me){
      wait_allreduce(i);
    }
  }

  comm_finalize();

  return 0;
}
