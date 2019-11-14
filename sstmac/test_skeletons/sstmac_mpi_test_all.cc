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

#define sstmac_app_name sstmac_mpi_testall

#include <sstmac/compute.h>
#include <sstmac/replacements/mpi.h>
#include <sprockit/errors.h>
#include <math.h>
#include <vector>

#include <unusedvariablemacro.h>

#define heisenbug printf("%s: %d\n", __FILE__, __LINE__); fflush(stdout)

#define failure_printf(str, ...) \
  fprintf(stderr, str "\n%s:%d\n", __VA_ARGS__, __FILE__, __LINE__)
#define failure(str) \
  fprintf(stderr, str "\n%s:%d\n", __FILE__, __LINE__)


static int errors_ = 0;

static void test_sendrecv(MPI_Comm comm);

static void test_barrier(MPI_Comm comm);

static void test_asynch(MPI_Comm comm);

static void test_bcast(MPI_Comm comm);

static void test_scatter(MPI_Comm comm);

static void test_gather(MPI_Comm comm);

// TODOWARNING static void test_gatherv(MPI_Comm comm);

static void test_scan(MPI_Comm comm);

static void test_send(MPI_Comm comm);

static void test_isend(MPI_Comm comm);

static void test_allgather(MPI_Comm comm);

static void test_reduce(MPI_Comm comm);

static void test_allreduce(MPI_Comm comm);

static void test_alltoall(MPI_Comm comm);

static void test_comms(MPI_Comm comm);

static void test_wait(MPI_Comm comm);

static void test_reducescatter(MPI_Comm comm);

//static void test_probe();

//static void test_persistent();

int USER_MAIN(int argc, char** argv)
{
  MPI_Init(&argc, &argv);

  double start = MPI_Wtime();

  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  if (size < 8) {
    spkt_abort_printf(
       "sstmac_mpi_testall needs at least 8 nodes to run - only have %d",
       size);
  }

  std::vector<MPI_Comm> comms;
  comms.push_back(MPI_COMM_WORLD);
  comms.push_back(0);
  MPI_Comm_split(MPI_COMM_WORLD, rank %2, rank, &comms[comms.size() - 1]);

  for(auto comm : comms) {
    test_sendrecv(comm);
    test_send(comm);
    test_isend(comm);
    test_asynch(comm);
    test_bcast(comm);
    test_reduce(comm);
    test_barrier(comm);
    test_allreduce(comm);
    test_scatter(comm);
    test_gather(comm);
    //test_scan(comm);
    test_comms(comm);
    test_wait(comm);
    test_allgather(comm);
    test_alltoall(comm);
    test_reducescatter(comm);
    //test_probe(comm);
  }

  MPI_Barrier(MPI_COMM_WORLD);

  if (rank == 0 && errors_ == 0) {
    std::cout << "- Finished testing! test successful \n";
  } else if (rank == 0 && errors_ != 0) {
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

SSTMAC_MAYBE_UNUSED
void
test_scan(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  int count = 1;
  int send_pay = rank;
  int recv_pay = 0;
  MPI_Scan(&send_pay, &recv_pay, count, MPI_INT, MPI_SUM, comm);
}

void
test_send(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  int src = (rank + size - 1) % size;
  int dst = (rank + size + 1) % size;

  if (rank % 2 == 0) {
    MPI_Send(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, comm);
    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, comm,
             MPI_STATUS_IGNORE);

    MPI_Bsend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, comm);
    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, comm,
             MPI_STATUS_IGNORE);

    MPI_Rsend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, comm);
    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, comm,
             MPI_STATUS_IGNORE);

    MPI_Ssend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, comm);
    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, comm,
             MPI_STATUS_IGNORE);
  } else {
    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, comm,
             MPI_STATUS_IGNORE);
    MPI_Send(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, comm);

    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, comm,
             MPI_STATUS_IGNORE);
    MPI_Bsend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, comm);

    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, comm,
             MPI_STATUS_IGNORE);
    MPI_Rsend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, comm);

    MPI_Recv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, comm,
             MPI_STATUS_IGNORE);
    MPI_Ssend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, comm);
  }
}

void
test_isend(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  int src = (rank + size - 1) % size;
  int dst = (rank + size + 1) % size;

  MPI_Request reqs[8];
  MPI_Isend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, comm,
            &reqs[0]);
  MPI_Ibsend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, comm,
             &reqs[2]);
  MPI_Irsend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, comm,
             &reqs[4]);
  MPI_Issend(MPI_PAYLOAD_IGNORE, 10, MPI_INT, dst, 0, comm,
             &reqs[6]);

  MPI_Irecv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, comm,
            &reqs[1]);
  MPI_Irecv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, comm,
            &reqs[3]);
  MPI_Irecv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, comm,
            &reqs[5]);
  MPI_Irecv(MPI_PAYLOAD_IGNORE, 10, MPI_INT, src, 0, comm,
            &reqs[7]);

  MPI_Waitall(8, reqs, MPI_STATUSES_IGNORE);
}

void
test_sendrecv(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

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
      MPI_Send(&pay, count, MPI_INT, buddy, tag, comm);

    } else {
      MPI_Status stat;
      int recvdata;
      MPI_Recv(&recvdata, count, MPI_INT, buddy, tag, comm,
               &stat);

      if (recvdata != (buddy * 1000)) {
        errors_++;
        failure("did not receive buddy*1000");
      }
    }
  }
}

void
test_barrier(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  if (rank % 2 == 0) {
    sstmac_compute(2e-3);
  }

  MPI_Barrier(comm);
}

void
test_reduce(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  // TODOWARNING int count = 100; //arbitrary

  int var = rank;
  int root = 2 % size;

  int result = 0;
  MPI_Reduce(&var, &result, 1, MPI_INT, MPI_SUM, root, comm);

  if (rank == root){
    int correct = size*(size-1) / 2;
    if (result != correct){
      failure_printf("MPI_Reduce expected %d, got %d", correct, result);
      ++errors_;
    }
  }
}

void
test_asynch(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  int count = 100; //arbitrary

  MPI_Status stat;
  if (rank == 0) {
    int r(1);
    int t(1);
    MPI_Send(NULL, count, MPI_INT, r, t, comm);
  } else if (rank == 1) {
    MPI_Request req;
    int r(0);
    int t(1);
    MPI_Irecv(NULL, count, MPI_INT, r, t, comm, &req);
    MPI_Wait(&req, &stat);
  }
}

void
test_allreduce(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  int count = 1; //arbitrary

  int pay = 1 << rank;
  ;
  int allrecvdata;
  MPI_Allreduce(&pay, &allrecvdata, count, MPI_INT, MPI_SUM, comm);

  int expected = (int) pow(2.0, size) - 1;
  if (allrecvdata != (expected)) {
    errors_++;
    failure_printf("allreduce expected %d, got %d", expected, allrecvdata);
  }
}

void
test_bcast(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  int count = 1; //arbitrary

  int root(0);
  int pay;
  if (rank == root) {
    pay = 1234;
  }

  MPI_Bcast(&pay, count, MPI_INT, root, comm);
  int recvdata = pay;

  if (recvdata != (1234)) {
    errors_++;
    failure_printf("bcast expected 1234, got %d", recvdata);
  }

}

void
test_scatter(MPI_Comm comm)
{
  int size, rank, recv = 0;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  std::vector<int> send(size);
  for(int i = 0; i < size; i++) send[i] = i;

  MPI_Scatter(send.data(), 1, MPI_INT, &recv, 1, MPI_INT, 0, comm);

  // Each rank should receive an integer matching its rank
  if(recv != rank) {
    failure_printf("Error: Unexpected value from MPI_Scatter on rank %i\n", rank);
  }

}

void
test_gather(MPI_Comm comm)
{
  const int root = 0;
  int size, rank;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  std::vector<int> recv;
  if (rank == root) recv.resize(size);

  MPI_Gather(&rank, 1, MPI_INT, recv.data(), 1, MPI_INT, 0, comm);

  // Should have an array with values 0 to (size - 1)
  if(rank == root) {
    for(int i = 0; i < recv.size(); i++)
      if(recv[i] != i)
        failure_printf("Error: MPI_Gather expected result with value %i at index %i but got %i", i, i, recv[i]);
  }
}

void
test_allgather(MPI_Comm comm)
{
  const int send_count = 8;
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  int count = 1; //arbitrary

  // Collect single ints into a null buffer
  MPI_Allgather(NULL, count, MPI_INT, NULL, count, MPI_INT, comm);

  // Gateher multiple integers from each sender
  std::vector<int> send(send_count), recv(size * send_count);
  for(int i = 0; i < send.size(); i++)
    send[i] = rank;

  MPI_Allgather(send.data(), send_count, MPI_INT, recv.data(), send_count, MPI_INT, comm);

  for(int i = 0; i < recv.size(); i++) {
    if (recv[i] != (i / send_count))
      failure_printf("Unexpected MPI_Allgather result on rank %i. Expected %i but got %i\n", rank, i/send_count, recv[i]);
  }

  // Now the same for doubles
  std::vector<double> double_send(send_count), double_recv(size * send_count);
  for(int i = 0; i < send.size(); i++)
    send[i] = rank;

  MPI_Allgather(double_send.data(), send_count, MPI_DOUBLE, double_recv.data(), send_count, MPI_DOUBLE, comm);

  for(int i = 0; i < recv.size(); i++) {
    if (recv[i] != (i / send_count))
      failure_printf("Unexpected MPI_Allgather result on rank %i. Expected %d but got %d\n", rank, i/send_count, recv[i]);
  }
}

void
test_alltoall(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  {
    // Simple example with null buffer
    MPI_Alltoall(NULL, 1, MPI_INT, NULL, 1, MPI_INT, comm);
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
    MPI_Alltoall(send_vect.data(), size, MPI_INT, recv_vect.data(), size, MPI_INT, comm);

    // checking the result, should look like a matrix transpose
    for(int i = 0; i < size; i++)
      if(recv_vect[i] != (i * 100) + rank)
        spkt_abort_printf("Unexpected value in MPI_Alltoall recv buffer! Check your implementation");
  }
}

void
test_comms(MPI_Comm comm)
{
  // A very limited subset of MPI_Comm_* and MPI_Group_* are currently implemented.
  int rank, size, new_size, new_rank;
  MPI_Group world_group;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  MPI_Comm_group(comm, &world_group);

  MPI_Barrier(comm);

  // MPI_Comm_split
  MPI_Comm new_comm;
  MPI_Comm_split(comm, rank %2, rank, &new_comm);

  MPI_Barrier(comm);
  MPI_Group new_group;
  MPI_Comm_group(new_comm, &new_group);

  MPI_Comm_rank(new_comm, &new_rank);
  MPI_Comm_size(new_comm, &new_size);

  if (new_size != size/2 && new_size != (size/2 + 1))
    spkt_abort_printf("MPI comm size mismatch, check MPI_Comm_split");
}

void
test_wait(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  int count = 100;
  int tag(698);

  // Wait on a bunch of MPI_Irecv
  if (rank == 0) {
    sstmac_sleep(1); //lag me, so the others have a chance to send

    std::vector<MPI_Status> stat(size - 1);
    std::vector<MPI_Request> reqs(size - 1);

    for(int i = 0; i < reqs.size(); i++)
      MPI_Irecv(NULL, count, MPI_DOUBLE, i+1, tag, comm, &reqs[i]);

    MPI_Waitall(size - 1, reqs.data(), stat.data());
  } else {
    MPI_Send(NULL, count, MPI_DOUBLE, int(0), tag, comm);
  }

  MPI_Barrier(comm);
}

void
test_reducescatter(MPI_Comm comm)
{
  // sumi does not support reduce_scatter at the moment
  // it does support reduce scatter block
  int rank, size;
  // TODOWARNING int recv = 0;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  std::vector<int> send(size);
  for(int i = 0; i < send.size(); i++){
    if (rank < i){
      send[i] = 1;
    } else {
      send[i] = 0;
    }
  }

  /**
  MPI_Reduce_scatter_block(send.data(), &recv, 1, MPI_INT, MPI_MAX, comm);
  // the result should be equivalent to the rank
  if (rank != recv){
    ++errors_;
    failure_printf("Rank %i expected %i, but got, %i on MPI_Reduce_scatter", rank, rank, recv);
  }
  */

  MPI_Barrier(comm);
}

void
test_probe()
{
}

void
test_persistent()
{
#if 0
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  int src = (rank + size - 1) % size;
  int dst = (rank + size + 1) % size;

  int count = 10;
  int tag = 0;
  MPI_Request reqs[2];
  MPI_Request* send_req = &reqs[0];
  MPI_Request* recv_req = &reqs[1];

  MPI_Send_init(MPI_PAYLOAD_IGNORE, count, MPI_INT, dst, tag,
                comm, send_req);
  MPI_Recv_init(MPI_PAYLOAD_IGNORE, count, MPI_INT, src, tag,
                comm, recv_req);
  MPI_Start(send_req);
  MPI_Start(recv_req);
  MPI_Wait(send_req, MPI_STATUS_IGNORE);
  MPI_Wait(recv_req, MPI_STATUS_IGNORE);
  // do the same test as an all
  MPI_Startall(2, reqs);
  MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE);
  MPI_Request_free(send_req);

  MPI_Bsend_init(MPI_PAYLOAD_IGNORE, count, MPI_INT, dst, tag,
                 comm, send_req);
  MPI_Start(send_req);
  MPI_Start(recv_req);
  MPI_Wait(send_req, MPI_STATUS_IGNORE);
  MPI_Wait(recv_req, MPI_STATUS_IGNORE);
  // do the same test as an all
  MPI_Startall(2, reqs);
  MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE);
  MPI_Request_free(send_req);

  MPI_Rsend_init(MPI_PAYLOAD_IGNORE, count, MPI_INT, dst, tag,
                 comm, send_req);
  MPI_Start(send_req);
  MPI_Start(recv_req);
  MPI_Wait(send_req, MPI_STATUS_IGNORE);
  MPI_Wait(recv_req, MPI_STATUS_IGNORE);
  // do the same test as an all
  MPI_Startall(2, reqs);
  MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE);
  MPI_Request_free(send_req);

  MPI_Ssend_init(MPI_PAYLOAD_IGNORE, count, MPI_INT, dst, tag,
                 comm, send_req);
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
