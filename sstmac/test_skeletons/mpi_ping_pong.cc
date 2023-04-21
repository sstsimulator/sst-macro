/**
Copyright 2009-2023 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2023, NTESS

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
#include <mpi.h>
#include <stddef.h>
#include <stdio.h>
#include <vector>
#include <sstmac/skeleton.h>
#include <sprockit/keyword_registration.h>

#define sstmac_app_name mpi_ping_pong

RegisterKeywords(
 { "sources", "ranks of senders" },
 { "destinations", "ranks of recvers" },
);

static void sendrecv(int rank, int src, int dst, int msize) {
  if (rank == src) {
    MPI_Send(NULL, msize, MPI_INT, dst, 0, MPI_COMM_WORLD);
  } else if (rank == dst) {
    MPI_Recv(NULL, msize, MPI_INT, src, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  }
}

int USER_MAIN(int argc, char** argv)
{
  int sizes[18] = {1,2,4,8,16,32,128,256,512,1024,2048,5096,10192,20384,40768,81536,163072,326144};
 
  std::vector<int> src = sstmac::getArrayParam<int>("sources");
  std::vector<int> dst = sstmac::getArrayParam<int>("destinations");
  bool pong = sstmac::getParam<bool>("pong", true);
  bool ping = sstmac::getParam<bool>("ping", true);
  bool barrier = sstmac::getParam<bool>("barrier", true);

  MPI_Init(&argc, &argv);
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  for (int i=0; i < src.size(); ++i) {
    if (rank == 0)
      printf("ping-pong between %i and %i\n",src[i],dst[i]);
    for (int s : sizes) { 
      double begin,end;
      if (barrier) MPI_Barrier(MPI_COMM_WORLD);
      if (rank == src[i])
        begin = MPI_Wtime();
      if (ping) sendrecv(rank,src[i],dst[i],s);
      if (pong) sendrecv(rank,dst[i],src[i],s);
      if (rank == src[i]) {
        end = MPI_Wtime();
        double time = (end - begin);
        int bytes = s * sizeof(int);
        double bw = bytes / time / 1e9;
        printf("%i: %8.4f GB/s\n", bytes, bw);
      }
    }
  }

  MPI_Finalize();

  return 0;
}
