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

#define sstmac_app_name mpi_coverage

#include <sstmac/compute.h>
#include <sstmac/replacements/mpi.h>
#include <sprockit/sim_parameters.h>
#include <math.h>

static int max_pt2pt_count = 64000;
static int max_root_count = 8192;
static int min_root_count = 1;
static int max_all_count = 4096;


static void test_pt2pt(MPI_Comm comm);
static void test_root_collectives(MPI_Comm comm);
static void test_all_collectives(MPI_Comm comm);
static void test_rootv_collectives(MPI_Comm comm);
static void test_allv_collectives(MPI_Comm comm);

#define finish_test() \
  if (rank==0) printf("Rank %d on MPI_Comm %ld passed test: %8.2e\n", rank, comm, MPI_Wtime());

static void test_all(MPI_Comm comm)
{
  test_pt2pt(comm);
  test_root_collectives(comm);
  test_all_collectives(comm);
  test_rootv_collectives(comm);
  test_allv_collectives(comm);
}

int USER_MAIN(int argc, char** argv)
{
  MPI_Init(&argc, &argv);
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  sprockit::sim_parameters* params = get_params();
  max_pt2pt_count = params->get_optional_int_param("max_pt2pt_count", max_pt2pt_count);
  max_all_count = params->get_optional_int_param("max_all_count", max_all_count);
  max_root_count = params->get_optional_int_param("max_root_count", max_root_count);
  min_root_count = params->get_optional_int_param("min_root_count", min_root_count);

  test_all(MPI_COMM_WORLD);

  //do a comm split to make a new communicator
  MPI_Comm splitComm;
  MPI_Comm_split(MPI_COMM_WORLD, rank % 6, (rank*7)<<3, &splitComm);
  test_all(splitComm);

  int splitRank;
  MPI_Comm_rank(splitComm, &splitRank);
  MPI_Comm splitCommX2;
  MPI_Comm_split(splitComm, splitRank % 3, (rank +123)<<4, &splitCommX2);
  test_all(splitCommX2);

  MPI_Comm_free(&splitComm);
  MPI_Comm_free(&splitCommX2);

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
  MPI_Request* reqs = new MPI_Request[num_sendrecvs*2];
  int disp = 1;
  for (int count=1; count < max_pt2pt_count; count *= 2){
    int tag = count / 2;
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
  }
  delete [] reqs;
  finish_test();
}

void
test_root_collectives(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  MPI_Request* reqs = new MPI_Request[4*size];
  for (int count=1; count < max_root_count; count *= 2){
    for (int root=0; root < size; ++root){
      MPI_Gather(NULL, count, MPI_INT, NULL, count, MPI_INT, root, comm);
      MPI_Scatter(NULL, count, MPI_INT, NULL, count, MPI_INT, root, comm);
      MPI_Reduce(NULL, NULL, count, MPI_INT, MPI_SUM, root, comm);
      MPI_Bcast(NULL, count, MPI_INT, root, comm);
    }

    int reqIdx = 0;
    for (int root=0; root < size; ++root){
      MPI_Igather(NULL, count, MPI_INT, NULL, count, MPI_INT, root, comm, &reqs[reqIdx]);
      ++reqIdx;
      MPI_Iscatter(NULL, count, MPI_INT, NULL, count, MPI_INT, root, comm, &reqs[reqIdx]);
      ++reqIdx;
      MPI_Ireduce(NULL, NULL, count, MPI_INT, MPI_SUM, root, comm, &reqs[reqIdx]);
      ++reqIdx;
      MPI_Ibcast(NULL, count, MPI_INT, root, comm, &reqs[reqIdx]);
      ++reqIdx;
    }
    MPI_Waitall(reqIdx, reqs, MPI_STATUSES_IGNORE);
  }
  delete[] reqs;
  finish_test();
}

void
test_rootv_collectives(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  int* sizes = new int[size];
  MPI_Request* reqs = new MPI_Request[2*size];
  for (int count=min_root_count; count < max_root_count; count *= 2){
    for (int i=0; i < size; ++i){
      sizes[i] = count;
    }

    MPI_Barrier(comm);

    int reqIdx = 0;
    for (int root=0; root < 1; ++root){
      //MPI_Igatherv(NULL, count, MPI_INT,
      //               NULL, sizes, displs, MPI_INT,
      //               root, comm, &reqs[reqIdx]);
      //++reqIdx;
      MPI_Iscatterv(NULL, sizes, NULL, MPI_INT,
                    NULL, count, MPI_INT,
                    root, comm, &reqs[reqIdx]);
      ++reqIdx;
    }
    MPI_Waitall(reqIdx, reqs, MPI_STATUSES_IGNORE);
  }
  delete[] sizes;
  delete[] reqs;
  finish_test();
}

void
test_all_collectives(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  for (int count=1; count < max_all_count; count *= 2){

    MPI_Allgather(NULL, count, MPI_INT, NULL, count, MPI_INT, comm);
    MPI_Alltoall(NULL, count, MPI_INT, NULL, count, MPI_INT, comm);
    MPI_Allreduce(NULL, NULL, count, MPI_INT, MPI_SUM, comm);

    MPI_Request reqs[3];
    MPI_Iallgather(NULL, count, MPI_INT, NULL, count, MPI_INT, comm, &reqs[0]);
    MPI_Ialltoall(NULL, count, MPI_INT, NULL, count, MPI_INT, comm, &reqs[1]);
    MPI_Iallreduce(NULL, NULL, count, MPI_INT, MPI_SUM, comm, &reqs[2]);
    MPI_Wait(&reqs[2], MPI_STATUS_IGNORE);
    MPI_Wait(&reqs[1], MPI_STATUS_IGNORE);
    MPI_Wait(&reqs[0], MPI_STATUS_IGNORE);
  }
  finish_test();
}

void
test_allv_collectives(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  int* sizes = new int[size];
  for (int count=1; count < max_all_count; count *= 2){
    for (int i=0; i < size; ++i){
      sizes[i] = count;
    }
    MPI_Barrier(comm);

    MPI_Request reqs[2];
    MPI_Iallgatherv(NULL, count, MPI_INT, NULL, sizes, NULL, MPI_INT, comm, &reqs[0]);
    MPI_Ialltoallv(NULL, sizes, NULL, MPI_INT, NULL, sizes, NULL, MPI_INT, comm, &reqs[1]);
    MPI_Wait(&reqs[1], MPI_STATUS_IGNORE);
    MPI_Wait(&reqs[0], MPI_STATUS_IGNORE);
  }
  delete[] sizes;
  finish_test();
}