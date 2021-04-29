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
#include <sstmac/util.h>
#include <sstmac/replacements/mpi/mpi.h>
#include <sstmac/common/runtime.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/skeleton.h>
#include <sstmac/compute.h>
#include <sprockit/keyword_registration.h>

#define sstmac_app_name mpi_tournament

int rank_in_round(int round, int rank, int nproc)
{
  int mod = nproc - 1;
  if (rank == 0){
    return 0;
  } else {
    int wrapped_rank = (rank + round - 1) % mod;
    return wrapped_rank + 1;
  }
}

void make_progress(MPI_Request* req, const char* action,
                   int src, int dest)
{
  double now = MPI_Wtime();
  double stop = now + 10e-3;
  while (now < stop){
    int done = 0;
    MPI_Test(req, &done, MPI_STATUS_IGNORE);
    if (done){
      return;
    }
    now = MPI_Wtime();
    sstmac_fsleep(1e-3); //sleep for a ms
  }
  //if we got here, we were unable to complete
  spkt_abort_printf("failed to make progress on %s %d->%d",
                    action, src, dest);
}

int USER_MAIN(int argc, char** argv)
{
  MPI_Init(&argc, &argv);

  int me, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &me);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  //send/recv from all the other procs
  void* null_buffer = nullptr;
  int count = sstmac::getUnitParam<int>("message_size", "1KB");

  std::vector<int> pairings(nproc);

  int num_rounds = nproc - 1;
  for (int r=0; r < num_rounds; ++r){
    for (int p=0; p < nproc; ++p){
      int rrank = rank_in_round(r, p, nproc);
      pairings[rrank] = p;
    }

    if (me == 0){
      printf("Round %d\n", r);
      int half = nproc / 2;
      for (int pair=0; pair < half; ++pair){
        printf("Pair %4d: %4d -> %4d\n",
               pair, pairings[pair], pairings[nproc - pair - 1]);
      }
    }

    int my_rrank = rank_in_round(r, me, nproc);
    int my_partner = pairings[nproc - my_rrank - 1];

    MPI_Request send_req;
    MPI_Request recv_req;
    MPI_Isend(null_buffer, count, MPI_BYTE, my_partner, r, MPI_COMM_WORLD, &send_req);
    MPI_Irecv(null_buffer, count, MPI_BYTE, my_partner, r, MPI_COMM_WORLD, &recv_req);

    make_progress(&send_req, "send", me, my_partner);
    make_progress(&recv_req, "recv", my_partner, me);
    MPI_Barrier(MPI_COMM_WORLD);
  }
  if (me == 0){
    std::cout << "Done!" << std::endl;
  }
  MPI_Finalize();
  return 0;
}
