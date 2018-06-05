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

#define sstmac_app_name sstmac_mpi_testall

#include <sstmac/compute.h>
#include <sstmac/replacements/mpi.h>
#include <sprockit/sim_parameters.h>
#include <math.h>

#define heisenbug printf("%s: %d\n", __FILE__, __LINE__); fflush(stdout)

#define failure_printf(str, ...) \
  fprintf(stderr, str "\n%s:%d\n", __VA_ARGS__, __FILE__, __LINE__)
#define failure(str) \
  fprintf(stderr, str "\n%s:%d\n", __FILE__, __LINE__)


static int errors_ = 0;

static void test_sendrecv();

static void test_barrier();

static void test_asynch();

static void test_bcast();

static void test_scatter();

static void test_gather();

static void test_gatherv();

static void test_scan();

static void test_send();

static void test_isend();

static void test_allgather();

static void test_reduce();

static void test_allreduce();

static void test_alltoall();

static void test_comms();

static void test_wait();

static void test_reducescatter();

static void test_probe();

static void test_persistent();

static void test_reduce_scatter();

int USER_MAIN(int argc, char** argv)
{
  MPI_Init(&argc, &argv);

  double start = MPI_Wtime();

  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  if (size < 8) {
    spkt_throw_printf(sprockit::range_error,
       "sstmac_mpi_testall needs at least 8 nodes to run - only have %d",
       size);
  }

//  //----------first, lets do a ping pong with payload
//  test_sendrecv();
//  test_send();
//  test_isend();

//  // ---- test nonblocking recv
//  test_asynch();

//  // ---- alright, let's try a broadcast
//  test_bcast();

//  // --- try a reduce
//  test_reduce();

//  // ---- sync up with barrier
//  test_barrier();

//  // ---- test allreduce
//  test_allreduce();

//  // ------ test scatter
//  test_scatter();

//  // ------ test gather
//  test_gather();

//  test_scan();

  //test_reduce_scatter();

//  //------ test communicator functions
  test_comms();

//  // ------ test wait functions
  test_wait();

  // ------ test allgather
//  test_allgather();

//  // ------ test alltoall
  test_alltoall();

//  // ------ test probing
  //test_probe();

//  // ------- test persistent
//  test_persistent();

  // ------ test reduce_scatter
  // test_reducescatter();

  // ----- finalize
  MPI_Barrier(MPI_COMM_WORLD);

  if (rank == 0 && errors_ == 0) {
    std::cout << "- Finished testing! test successful \n";
  }
  else if (rank == 0 && errors_ != 0) {
    std::cerr << "- Finished testing! " << errors_
                 << " ERRORS - test not successful \n";
  }

  double stop = MPI_Wtime();

  MPI_Finalize();

  if (rank == 0){
    double t_total = stop - start;
    ::printf("Total runtime %8.4fms\n", t_total*1e3);
  }

  return 0;
}

void
test_scan()
{
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  int count = 1;
  int send_pay = rank;
  int recv_pay = 0;
  MPI_Scan(&send_pay, &recv_pay, count, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

#if 0
  mpi_comm* world = mpi()->comm_world();
  int rank = world->rank();
  int size = world->size();

  int count = 1;
  int send_pay = rank;
  int recv_pay = 0;
  MPI_Scan(&send_pay, &recv_pay, count, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
#endif
}

void
test_reduce_scatter()
{
#if 0
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  int* sends = (int*) malloc(sizeof(int) * (size * 2));
  int* recvs = (int*) malloc(sizeof(int) * 2);

  int* recvcnts = (int*) malloc(sizeof(int) * size);
  for (int i = 0; i < size; i++) {
    recvcnts[i] = 2;
    sends[2*i] = 1;
    sends[2*i+1] = 2;
  }

  MPI_Reduce_scatter(sends, recvs, recvcnts, MPI_INT, MPI_SUM,
                     MPI_COMM_WORLD);

  if (recvs[0] != size || recvs[1] != size * 2) {
    errors_++;

    if (stop_at_errors_) {
      throw sprockit::spkt_error("an error occurred in the application");
    }
  }
#endif
}

void
test_send()
{
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  int src = (rank + size - 1) % size;
  int dst = (rank + size + 1) % size;

  if (rank % 2 == 0) {
    MPI_Send(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, MPI_COMM_WORLD);
    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, MPI_COMM_WORLD,
             MPI_STATUS_IGNORE);

    MPI_Bsend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, MPI_COMM_WORLD);
    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, MPI_COMM_WORLD,
             MPI_STATUS_IGNORE);

    MPI_Rsend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, MPI_COMM_WORLD);
    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, MPI_COMM_WORLD,
             MPI_STATUS_IGNORE);

    MPI_Ssend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, MPI_COMM_WORLD);
    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, MPI_COMM_WORLD,
             MPI_STATUS_IGNORE);
  }
  else {
    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, MPI_COMM_WORLD,
             MPI_STATUS_IGNORE);
    MPI_Send(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, MPI_COMM_WORLD);

    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, MPI_COMM_WORLD,
             MPI_STATUS_IGNORE);
    MPI_Bsend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, MPI_COMM_WORLD);

    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, MPI_COMM_WORLD,
             MPI_STATUS_IGNORE);
    MPI_Rsend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, MPI_COMM_WORLD);

    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, MPI_COMM_WORLD,
             MPI_STATUS_IGNORE);
    MPI_Ssend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, MPI_COMM_WORLD);
  }
}

void
test_isend()
{
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  int src = (rank + size - 1) % size;
  int dst = (rank + size + 1) % size;

  MPI_Request reqs[8];
  MPI_Isend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, MPI_COMM_WORLD,
            &reqs[0]);
  MPI_Ibsend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, MPI_COMM_WORLD,
             &reqs[2]);
  MPI_Irsend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, MPI_COMM_WORLD,
             &reqs[4]);
  MPI_Issend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, MPI_COMM_WORLD,
             &reqs[6]);

  MPI_Irecv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, MPI_COMM_WORLD,
            &reqs[1]);
  MPI_Irecv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, MPI_COMM_WORLD,
            &reqs[3]);
  MPI_Irecv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, MPI_COMM_WORLD,
            &reqs[5]);
  MPI_Irecv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, MPI_COMM_WORLD,
            &reqs[7]);

  MPI_Waitall(8, reqs, MPI_STATUSES_IGNORE);
}

void
test_sendrecv()
{
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  int count = 1; //arbitrary

  int tag(0);
  bool participant = true;
  if ((size % 2) && (rank + 1 >= size)) {
    // This is the odd-node-out -- communicating with no-one.
    participant = false;
  }
  if (participant) {
    int buddy(rank ^ 1); // 0<=>1, 2<=>3, etc.

    if ((rank) & 1) {
      // even values of half-cycle plus rank.

      int pay = rank * 1000;
      MPI_Send(&pay, count, MPI_INT, buddy, tag, MPI_COMM_WORLD);

    }
    else {
      MPI_Status stat;
      int recvdata;
      MPI_Recv(&recvdata, count, MPI_INT, buddy, tag, MPI_COMM_WORLD,
               &stat);

      if (recvdata != (buddy * 1000)) {
        errors_++;
        failure("did not receive buddy*1000");
      }

    }

  }
}

void
test_barrier()
{
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  if (rank % 2 == 0) {
    sstmac_compute(2e-3);
  }

  MPI_Barrier(MPI_COMM_WORLD);
}

void
test_reduce()
{
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  int count = 100; //arbitrary

  int var = rank;
  int root = 2 % size;

  int result = 0;
  MPI_Reduce(&var, &result, 1, MPI_INT, MPI_SUM, root, MPI_COMM_WORLD);

  if (rank == root){
    int correct = size*(size-1) / 2;
    if (result != correct){
      failure_printf("MPI_Reduce expected %d, got %d", correct, result);
      ++errors_;
    }
  }
}

void
test_asynch()
{
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  int count = 100; //arbitrary

  MPI_Status stat;
  if (rank == 0) {
    int r(1);
    int t(1);
    MPI_Send(NULL, count, MPI_INT, r, t, MPI_COMM_WORLD);

  }
  else if (rank == 1) {
    MPI_Request req;
    int r(0);
    int t(1);
    MPI_Irecv(NULL, count, MPI_INT, r, t, MPI_COMM_WORLD, &req);
    MPI_Wait(&req, &stat);
  }
}

void
test_allreduce()
{
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  int count = 1; //arbitrary

  int pay = 1 << rank;
  ;
  int allrecvdata;
  MPI_Allreduce(&pay, &allrecvdata, count, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

  int expected = (int) pow(2.0, size) - 1;
  if (allrecvdata != (expected)) {
    errors_++;
    failure_printf("allreduce expected %d, got %d", expected, allrecvdata);
  }
}

void
test_bcast()
{
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  int count = 1; //arbitrary

  int root(0);
  int pay;
  if (rank == root) {
    pay = 1234;
  }

  MPI_Bcast(&pay, count, MPI_DOUBLE, root, MPI_COMM_WORLD);
  int recvdata = pay;

  if (recvdata != (1234)) {
    errors_++;
    failure_printf("bcast expected 1234, got %d", recvdata);
  }

}

void
test_scatter()
{
  int size, rank, recv = 0;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  std::vector<int> send(size);
  for(int i = 0; i < size; i++) send[i] = i;

  MPI_Scatter(send.data(), 1, MPI_INT, &recv, 1, MPI_INT, 0, MPI_COMM_WORLD);

  // Each rank should receive an integer matching its rank
  if(recv != rank) {
    failure_printf("Error: Unexpected value from MPI_Scatter on rank %i\n", rank);
  }

  /*  int rank, size;
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);
   MPI_Comm_size(MPI_COMM_WORLD, &size);
   int count = 100; //arbitrary

   SSTMAC_DEBUG << "testall[" << rank << "] -- Testing scatter " << " \n";

   std::vector<payload::const_ptr> vals;
   std::vector<int> vcounts;

   if (rank == 0)
   {
   for (int i = 0; i < size; i++)
   {
   int val = 1 << i;
   vals.push_back(valuepayload<int>::construct(val));
   vcounts.push_back(count);
   }
   }

   payload::const* result;

   mpi()->scatter(count, MPI_DOUBLE, count, MPI_DOUBLE, int(0),
   MPI_COMM_WORLD, vals, result);

   const valuepayload<int>::const_ptr scatterdata = ptr_safe_cast<
   const valuepayload<int> >(result);

   if (!scatterdata)
   {
   errors_++;
   SSTMAC_DEBUG << "ERROR at rank " << rank
   << ": NULL payload recv'd for SCATTER \n";
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }
   else
   {
   int expected = (int) (1 << rank);
   if (scatterdata->data() != (expected))
   {
   errors_++;
   SSTMAC_DEBUG << "ERROR at rank " << rank << ": SCATTER: received value "
   << scatterdata->data() << " and we should have got " << expected << "\n";
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }
   }

   SSTMAC_DEBUG << "testall[" << rank << "] -- Testing scatterv " << " \n";
   mpi()->scatterv(vcounts, MPI_DOUBLE, count, MPI_DOUBLE,
   int(0), MPI_COMM_WORLD, vals, result);

   const valuepayload<int>::const_ptr scatterdata2 = ptr_safe_cast<
   const valuepayload<int> >(result);

   if (!scatterdata2)
   {
   errors_++;
   SSTMAC_DEBUG << "ERROR at rank " << rank
   << ": NULL payload recv'd for SCATTERV \n";
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }
   else
   {
   int expected = (int) (1 << rank);
   if (scatterdata2->data() != (expected))
   {
   errors_++;
   SSTMAC_DEBUG << "ERROR at rank " << rank << ": SCATTERV: received value "
   << scatterdata2->data() << " and we should have got " << expected << "\n";
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }
   }*/

}

void
test_gather()
{
  const int root = 0;
  int size, rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  std::vector<int> recv;
  if (rank == root) recv.resize(size);

  MPI_Gather(&rank, 1, MPI_INT, recv.data(), 1, MPI_INT, 0, MPI_COMM_WORLD);

  // Should have an array with values 0 to (size - 1)
  if(rank == root) {
    for(int i = 0; i < recv.size(); i++)
      if(recv[i] != i)
        failure_printf("Error: MPI_Gather expected result with value %i at index %i but got %i", i, i, recv[i]);
  }
}

void
test_allgather()
{
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  int count = 1; //arbitrary

  MPI_Allgather(NULL, count, MPI_INT, NULL, count, MPI_INT, MPI_COMM_WORLD);

  /*  int rank, size;
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);
   MPI_Comm_size(MPI_COMM_WORLD, &size);
   int count = 100; //arbitrary

   SSTMAC_DEBUG << "testall[" << rank << "] -- Testing allgather " << " \n";

   std::vector<payload::const_ptr> vals;

   payload::const_ptr load = valuepayload<int>::construct(1 << rank);

   mpi()->allgather(count, MPI_DOUBLE, count, MPI_DOUBLE, MPI_COMM_WORLD,
   load, vals);

   std::vector::iterator<payload::const*>:: it, end = vals.end();

   if (vals.size() != MPI_COMM_WORLD.size().id)
   {
   errors_++;
   SSTMAC_DEBUG << "ERROR at rank " << rank
   << ": ALLGATHER: result vector size (" << vals.size()
   << ") does not match MPI_COMM_WORLD comm size (" << MPI_COMM_WORLD.size().id << ") \n";
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }

   int i = 0;
   for (it = vals.begin(); it != end; it++)
   {
   if (*it)
   {
   int expected = (int) (1 << i);
   const valuepayload<int>::const_ptr scatterdata = ptr_safe_cast<
   const valuepayload<int> >(*it);

   if (scatterdata->data() != (expected))
   {
   errors_++;
   SSTMAC_DEBUG << "ERROR at rank " << rank << ": ALLGATHER: received value "
   << scatterdata->data() << " and we should have got " << expected << "\n";
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }
   }
   else
   {
   errors_++;
   SSTMAC_DEBUG << "ERROR at rank " << rank
   << ": NULL payload recv'd for ALLGATHER \n";
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }

   i++;

   }

   SSTMAC_DEBUG << "testall[" << rank << "] -- Testing allgatherv " << " \n";

   std::vector<payload::const_ptr> vals2;

   payload::const_ptr load2 = valuepayload<int>::construct(1 << rank);
   std::vector<int> vcounts;

   for (int i = 0; i < size; i++)
   {
   vcounts.push_back(count);
   }

   mpi()->allgatherv(count, MPI_DOUBLE, vcounts, MPI_DOUBLE,
   MPI_COMM_WORLD, load2, vals2);

   std::vector::iterator<payload::const*>:: it2, end2 = vals2.end();

   if (vals2.size() != MPI_COMM_WORLD.size().id)
   {
   errors_++;
   SSTMAC_DEBUG << "ERROR at rank " << rank
   << ": ALLGATHERV: result vector size (" << vals.size()
   << ") does not match MPI_COMM_WORLD comm size (" << MPI_COMM_WORLD.size().id << ") \n";
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }

   i = 0;
   for (it2 = vals2.begin(); it2 != end2; it2++)
   {
   if (*it2)
   {
   int expected = (int) (1 << i);
   const valuepayload<int>::const_ptr scatterdata = ptr_safe_cast<
   const valuepayload<int> >(*it2);

   if (scatterdata->data() != (expected))
   {
   errors_++;
   SSTMAC_DEBUG << "ERROR at rank " << rank << ": ALLGATHERV: received value "
   << scatterdata->data() << " and we should have got " << expected << "\n";
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }
   }
   else
   {
   errors_++;
   SSTMAC_DEBUG << "ERROR at rank " << rank
   << ": NULL payload recv'd for ALLGATHERV \n";
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }

   i++;

   }*/

}

void
test_alltoall()
{
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  {
    // Simple example with null buffer
    MPI_Alltoall(NULL, 1, MPI_INT, NULL, 1, MPI_INT, MPI_COMM_WORLD);
  }

  {
    // Another example with real data
    if (size > 512)
      std::cout << "skipping part of MPI_Alltoall test due to Comm size(" << size << " > 512" << std::endl;

    std::vector<int> send_vect(size), recv_vect(size);

    // Encode the originating processor and index
    for(int i = 0; i < size; i++)
      send_vect[i] = (rank * 100) + i;

    return;
    MPI_Alltoall(send_vect.data(), size, MPI_INT, recv_vect.data(), size, MPI_INT, MPI_COMM_WORLD);

    // checking the result, should look like a matrix transpose
    for(int i = 0; i < size; i++)
      if(recv_vect[i] != (i * 100) + rank)
        throw sprockit::spkt_error("Unexpected value in MPI_Alltoall recv buffer! Check your implementation");
  }
}

void
test_comms()
{
  // A very limited subset of MPI_Comm_* and MPI_Group_* are currently implemented.
  int rank, size, new_size, new_rank;
  MPI_Group world_group;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_group(MPI_COMM_WORLD, &world_group);

  MPI_Barrier(MPI_COMM_WORLD);

  // MPI_Comm_split
  MPI_Comm new_comm;
  MPI_Comm_split(MPI_COMM_WORLD, rank %2, rank, &new_comm);

  MPI_Barrier(MPI_COMM_WORLD);
  MPI_Group new_group;
  MPI_Comm_group(new_comm, &new_group);

  MPI_Comm_rank(new_comm, &new_rank);
  MPI_Comm_size(new_comm, &new_size);

  if (new_size != size/2 && new_size != (size/2 + 1))
    throw sprockit::spkt_error("MPI comm size missmatch, check MPI_Comm_split");
}

void
test_wait()
{
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  int count = 100;
  int tag(698);

  int sender(5);

  if (rank == 0) {
    MPI_Request* reqs = new MPI_Request[size - 1];

    for (int i = 1; i < size; i++) {
      MPI_Request req;
      MPI_Irecv(NULL, count, MPI_DOUBLE, int(i), tag, MPI_COMM_WORLD,
                &req);
      reqs[i - 1] = req;

    }

    MPI_Status* stat = new MPI_Status[size - 1];
    MPI_Waitall(size - 1, reqs, stat);

    delete[] stat;
    delete[] reqs;
  }
  else {
    MPI_Send(NULL, count, MPI_DOUBLE, int(0), tag, MPI_COMM_WORLD);
  }

  MPI_Barrier(MPI_COMM_WORLD);

}

void
test_reducescatter()
{
  int rank, size, recv = 0;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  std::vector<int> send(size), recv_counts(size);
  for(int i = 0; i < send.size(); i++) send[i] = i;
  for(int i = 0; i < recv_counts.size(); i++) recv_counts[i] = 1;

  MPI_Reduce_scatter(send.data(), &recv, recv_counts.data(), MPI_INT, MPI_MAX, MPI_COMM_WORLD);

  // the result should be equivalent to the rank
  if (rank != recv)
    failure_printf("Rank %i expected %i, but got, %i on MPI_Reduce_scatter", rank, rank, recv);

  MPI_Barrier(MPI_COMM_WORLD);
}

void
test_probe()
{
  /*int rank, size;
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);
   MPI_Comm_size(MPI_COMM_WORLD, &size);

   int count = 100;
   int tag(713);
   int tag2(714);

   mpi()->barrier(MPI_COMM_WORLD);

   int sender(5);

   SSTMAC_DEBUG << "testall[" << rank << "] -- Testing probe \n";

   if (rank == 0)
   {

   std::vector<MPI_Request> reqs;

   for (int i = 1; i < size; i++)
   {
   MPI_Request req;
   mpi()->irecv(count, MPI_DOUBLE, int(i), tag, MPI_COMM_WORLD, req);
   reqs.push_back(req);
   }

   MPI_Status stat;
   int index;
   bool flag;
   std::vector<int> indices;
   std::vector<MPI_Status> statuses;

   mpi()->test(reqs.at(sender.id), flag, stat);

   if (flag)
   {
   SSTMAC_DEBUG << "ERROR at rank " << rank
   << ": test returned completed, and it probably should not have \n";
   errors_++;
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }

   mpi()->testsome(reqs, indices, statuses);

   if (indices.size() > 0)
   {
   SSTMAC_DEBUG << "ERROR at rank " << rank
   << ": testsome returned completed, and it probably should not have \n";
   errors_++;
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }

   mpi()->testany(reqs, index, flag, stat);

   if (flag)
   {
   SSTMAC_DEBUG << "ERROR at rank " << rank
   << ": testany returned completed, and it probably should not have \n";
   errors_++;
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }

   mpi()->iprobe(sender, tag, MPI_COMM_WORLD, flag, stat);

   if (flag)
   {
   SSTMAC_DEBUG << "ERROR at rank " << rank
   << ": iprobe returned completed, and it probably should not have \n";
   errors_++;
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }

   // -------  here is the blocking probe ----------- //
   mpi()->probe(sender, tag, MPI_COMM_WORLD, stat);

   if (!stat)
   {
   SSTMAC_DEBUG << "ERROR at rank " << rank
   << ": blocking probe did not return a valid mpistatus \n";
   errors_++;
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }

   mpi()->test(reqs.at(sender.id - 1), flag, stat);

   if (!flag)
   {
   SSTMAC_DEBUG << "ERROR at rank " << rank
   << ": test returned NOT completed, and it should not have \n";
   errors_++;
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }

   mpi()->testsome(reqs, indices, statuses);

   if (indices.size() != 1)
   {
   SSTMAC_DEBUG << "ERROR at rank " << rank
   << ": testsome returned indices with size " << indices.size()
   << ", and it should have been 1 \n";
   errors_++;
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }

   mpi()->testany(reqs, index, flag, stat);

   if (index != (sender.id - 1))
   {
   SSTMAC_DEBUG << "ERROR at rank " << rank << ": testany returned index "
   << index << ", and it should not have been " << (sender.id - 1) << " \n";
   errors_++;
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }

   sleep(timestamp(1));

   mpi()->iprobe(sender, tag2, MPI_COMM_WORLD, flag, stat);

   if (!flag)
   {
   SSTMAC_DEBUG << "ERROR at rank " << rank
   << ": iprobe returned NOT completed, and it should not have \n";
   errors_++;
   if (stop_at_errors_)
   throw sprockit::spkt_error("an error occurred in the application");
   }

   mpi()->recv(count, MPI_DOUBLE, sender, tag2, MPI_COMM_WORLD, stat);

   }
   else if (rank == sender)
   {
   compute(timestamp(2));
   mpi()->send(count, MPI_DOUBLE, int(0), tag, MPI_COMM_WORLD);

   mpi()->send(count, MPI_DOUBLE, int(0), tag2, MPI_COMM_WORLD);
   }

   mpi()->barrier(MPI_COMM_WORLD);*/

}

void
test_persistent()
{
#if 0
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  int src = (rank + size - 1) % size;
  int dst = (rank + size + 1) % size;

  int count = 10;
  int tag = 0;
  MPI_Request reqs[2];
  MPI_Request* send_req = &reqs[0];
  MPI_Request* recv_req = &reqs[1];

  MPI_Send_init(MPI_PAYLOAD_IGNORE, count, MPI_INT, dst, tag,
                MPI_COMM_WORLD, send_req);
  MPI_Recv_init(MPI_PAYLOAD_IGNORE, count, MPI_INT, src, tag,
                MPI_COMM_WORLD, recv_req);
  MPI_Start(send_req);
  MPI_Start(recv_req);
  MPI_Wait(send_req, MPI_STATUS_IGNORE);
  MPI_Wait(recv_req, MPI_STATUS_IGNORE);
  // do the same test as an all
  MPI_Startall(2, reqs);
  MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE);
  MPI_Request_free(send_req);

  MPI_Bsend_init(MPI_PAYLOAD_IGNORE, count, MPI_INT, dst, tag,
                 MPI_COMM_WORLD, send_req);
  MPI_Start(send_req);
  MPI_Start(recv_req);
  MPI_Wait(send_req, MPI_STATUS_IGNORE);
  MPI_Wait(recv_req, MPI_STATUS_IGNORE);
  // do the same test as an all
  MPI_Startall(2, reqs);
  MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE);
  MPI_Request_free(send_req);

  MPI_Rsend_init(MPI_PAYLOAD_IGNORE, count, MPI_INT, dst, tag,
                 MPI_COMM_WORLD, send_req);
  MPI_Start(send_req);
  MPI_Start(recv_req);
  MPI_Wait(send_req, MPI_STATUS_IGNORE);
  MPI_Wait(recv_req, MPI_STATUS_IGNORE);
  // do the same test as an all
  MPI_Startall(2, reqs);
  MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE);
  MPI_Request_free(send_req);

  MPI_Ssend_init(MPI_PAYLOAD_IGNORE, count, MPI_INT, dst, tag,
                 MPI_COMM_WORLD, send_req);
  MPI_Start(send_req);
  MPI_Start(recv_req);
  MPI_Wait(send_req, MPI_STATUS_IGNORE);
  MPI_Wait(recv_req, MPI_STATUS_IGNORE);
  // do the same test as an all
  MPI_Startall(2, reqs);
  MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE);
  MPI_Request_free(send_req);

  //and also the recv
  MPI_Request_free(recv_req);
#endif
}
