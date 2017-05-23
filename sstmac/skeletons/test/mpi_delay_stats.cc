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

#define sstmac_app_name mpi_delay_stats

RegisterKeywords("sync_delay");

int USER_MAIN(int argc, char** argv)
{
  //make sure everyone gets here at exatly the same time
  double now = MPI_Wtime();
  sstmac_compute(5e-5 - now);

  MPI_Init(&argc, &argv);

  int me, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &me);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  if (nproc < 8){
    spkt_abort_printf("Test must run with at least 8 ranks");
  }

  double send_delay = get_params()->get_time_param("send_delay");
  double send_compute = get_params()->get_time_param("send_compute");
  double recv_delay = get_params()->get_time_param("recv_delay");
  double recv_compute = get_params()->get_time_param("recv_compute");
  int send_size = get_params()->get_byte_length_param("message_size");


  int send_to = (me + 1) % nproc;
  int recv_from = (me - 1 + nproc) % nproc;

  if (me % 2 == 0){
    //all even ranks create some delay
    sstmac_compute(send_delay);
    MPI_Send(NULL, send_size, MPI_BYTE, send_to, 42, MPI_COMM_WORLD);
    sstmac_compute(send_delay);
    MPI_Request sendreq;
    MPI_Isend(NULL, send_size, MPI_BYTE, send_to, 42, MPI_COMM_WORLD, &sendreq);
    sstmac_compute(send_compute);
    MPI_Wait(&sendreq, MPI_STATUS_IGNORE);
    sstmac_compute(send_delay);
    MPI_Allreduce(send_size, MPI_CHAR, MPI_MAX, MPI_COMM_WORLD);
  } else {
    sstmac_compute(recv_delay);
    MPI_Recv(NULL, send_size, MPI_BYTE, recv_from, 42, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    sstmac_compute(recv_delay);
    MPI_Request recvreq;
    MPI_Irecv(NULL, send_size, MPI_BYTE, recv_from, 42, MPI_COMM_WORLD, &recvreq);
    sstmac_compute(recv_compute);
    MPI_Wait(&recvreq, MPI_STATUS_IGNORE);
    sstmac_compute(recv_delay);
    MPI_Allreduce(send_size, MPI_CHAR, MPI_MAX, MPI_COMM_WORLD);
  }

  MPI_Finalize();
  return 0;
}