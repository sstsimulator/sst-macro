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

#define sstmac_app_name memory_leak_test

#include <sstmac/compute.h>
#include <sstmac/replacements/mpi.h>
#include <sprockit/sim_parameters.h>
#include <math.h>

static int max_pt2pt_count = 64000;
static int max_all_count = 4096;


static void test_pt2pt(MPI_Comm comm);
static void test_allgather(MPI_Comm comm);

#define finish_test() \
  if (rank==0) printf("Rank %d on MPI_Comm %ld passed test: %8.2e\n", rank, comm, MPI_Wtime());

int USER_MAIN(int argc, char** argv)
{
  MPI_Init(&argc, &argv);
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  sprockit::sim_parameters* params = get_params();
  max_pt2pt_count = params->get_optional_int_param("max_pt2pt_count", max_pt2pt_count);
  max_all_count = params->get_optional_int_param("max_all_count", max_all_count);

  test_pt2pt(MPI_COMM_WORLD);
  test_allgather(MPI_COMM_WORLD);

  MPI_Finalize();
  return 0;
}

void
test_pt2pt(MPI_Comm comm)
{
  int rank, size;
  int worldRank;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  static const int num_sendrecvs = 4;
  MPI_Request reqs[8];
  int disp = 1;
  int count = max_pt2pt_count; 
  int tag = 100;
  int reqIdx = 0;
  for (int i=1; i < num_sendrecvs; ++i){
    disp = disp % size;
    int send_to = (rank + disp) % size;
    int recv_from = (rank + size - disp) % size;
    disp *= 2;
    MPI_Isend(NULL, count, MPI_INT, send_to, tag, comm, &reqs[reqIdx]);
    ++reqIdx;
    MPI_Irecv(NULL, count, MPI_INT, recv_from,
              tag, comm, &reqs[reqIdx]);
    ++reqIdx;
  }
  MPI_Waitall(reqIdx, reqs, MPI_STATUSES_IGNORE);
  finish_test();
}


void
test_allgather(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  int count = max_all_count; 
  MPI_Allgather(NULL, count, MPI_INT, NULL, count, MPI_INT, comm);
  finish_test();
}