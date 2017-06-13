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
#include <sstmac/util.h>
#include <sstmac/compute.h>
#include <sstmac/replacements/mpi.h>
#include <sstmac/common/runtime.h>
#include <sstmac/software/process/backtrace.h>
#include <sumi-mpi/mpi_api.h>
#include <sstmac/skeleton.h>
#include <sprockit/keyword_registration.h>

#define sstmac_app_name mpi_progress


int USER_MAIN(int argc, char** argv)
{
  SSTMACBacktrace("main");
  MPI_Init(&argc, &argv);

  int me, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &me);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  int send_size = get_params()->get_byte_length_param("send_size");
  double send_delay = get_params()->get_time_param("send_delay");
  int num_sends = get_params()->get_int_param("num_sends");
  int send_to = (me + 1) % nproc;
  int recv_from = (me - 1 + nproc) % nproc;
  MPI_Request send_reqs[10];
  MPI_Request recv_reqs[10];
  for (int i=0; i < num_sends; ++i){
    MPI_Irecv(NULL, send_size, MPI_BYTE, recv_from, 42, MPI_COMM_WORLD, &recv_reqs[i]);
  }

  for (int i=0; i < num_sends; ++i){
    sstmac_compute(send_delay);
    MPI_Isend(NULL, send_size, MPI_BYTE, send_to, 42, MPI_COMM_WORLD, &send_reqs[i]);
  }

  MPI_Waitall(num_sends, recv_reqs, MPI_STATUSES_IGNORE);
  MPI_Waitall(num_sends, send_reqs, MPI_STATUSES_IGNORE);

  MPI_Finalize();
  return 0;
}