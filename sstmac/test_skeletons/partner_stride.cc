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

#include <sstmac/util.h>
#include <sstmac/replacements/mpi.h>
#include <sstmac/common/runtime.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/replacements/mpi.h>
#include <sstmac/skeleton.h>
#include <sstmac/compute.h>
#include <sprockit/keyword_registration.h>

#define sstmac_app_name partner_stride

int USER_MAIN(int argc, char** argv)
{
  MPI_Init(&argc, &argv);

  int me, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &me);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  //send/recv from all the other procs
  void* null_buffer = nullptr;
  int count = sstmac::getUnitParam<int>("message_size", "1MB");
  int stride = sstmac::getParam<int>("stride");
  int nrepeat = sstmac::getParam<int>("repeat", 5);
  int send_partner = (me + stride) % nproc;
  int recv_partner = (me - stride + nproc) % nproc;
  double t_start = MPI_Wtime();
  int tag = 42;
  MPI_Request reqs[2];
  for (int i=0; i < nrepeat; ++i){
    int reqidx = 0;
    if (me < send_partner){
      MPI_Isend(null_buffer, count, MPI_BYTE, send_partner, tag, MPI_COMM_WORLD, &reqs[reqidx++]);
    } 
    if (me > recv_partner){
      MPI_Irecv(null_buffer, count, MPI_BYTE, recv_partner, tag, MPI_COMM_WORLD, &reqs[reqidx++]);
    }
    MPI_Waitall(reqidx, reqs, MPI_STATUSES_IGNORE);
  }
  double t_stop = MPI_Wtime();
  if (me < send_partner){
    double tput = count*nrepeat/(t_stop-t_start);
    printf("Rank %2d->%2d: %12.8f GB/s\n", me, send_partner, tput/1e9);
  }
  MPI_Finalize();
  return 0;
}
