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

#include <sstmac/backends/mpi/mpi_runtime.h>
#include <cstring>
#include <sprockit/keyword_registration.h>
#include <iostream>

RegisterKeywords(
"mpi_max_num_requests",
);

#define mpi_debug(...) \
  debug_printf(sprockit::dbg::parallel, "LP %d: %s", me_, sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace mpi {

void pair_reduce_function(void *invec, void *inoutvec, int *len, MPI_Datatype *datatype)
{
  int* in = (int*) invec;
  int* out = (int*) inoutvec;
  int length = *len;
  for (int i=0; i < length; ++i){
    int* inpair = &in[2*i];
    int* outpair = &out[2*i];
    outpair[0] += inpair[0];
    outpair[1] += inpair[1];
  }
}

int64_t
mpi_runtime::allreduce_min(int64_t my_time)
{
  if (nproc_ == 1)
    return my_time;

  int64_t min_time = 0;
  int count = 1;
  MPI_Allreduce(&my_time, &min_time, count, MPI_LONG_LONG, MPI_MIN, MPI_COMM_WORLD);
  return min_time;
}

int64_t
mpi_runtime::allreduce_max(int64_t my_time)
{
  if (nproc_ == 1)
    return my_time;

  int64_t max_time = 0;
  int count = 1;
  MPI_Allreduce(&my_time, &max_time, count, MPI_LONG_LONG, MPI_MAX, MPI_COMM_WORLD);
  return max_time;
}

void
mpi_runtime::do_reduce(void* data, int nelems, MPI_Datatype ty, MPI_Op op, int root)
{
  if (root == global_root){
    MPI_Allreduce(MPI_IN_PLACE, data, nelems, ty, op, MPI_COMM_WORLD);
  } else if (me_ == root){
    MPI_Reduce(MPI_IN_PLACE, data, nelems, ty, op, root, MPI_COMM_WORLD);
  } else {
    MPI_Reduce(data, data, nelems, ty, op, root, MPI_COMM_WORLD);
  }
}

void
mpi_runtime::global_max(long *data, int nelems, int root)
{
  if (nproc_ == 1)
    return;

  do_reduce(data, nelems, MPI_LONG, MPI_MAX, root);
}

void
mpi_runtime::global_max(int *data, int nelems, int root)
{
  if (nproc_ == 1)
    return;

  do_reduce(data, nelems, MPI_INT, MPI_MAX, root);
}

void
mpi_runtime::global_sum(long long *data, int nelems, int root)
{
  if (nproc_ == 1)
    return;

  do_reduce(data, nelems, MPI_LONG_LONG, MPI_SUM, root);
}

void
mpi_runtime::global_sum(long *data, int nelems, int root)
{
  if (nproc_ == 1)
    return;

  do_reduce(data, nelems, MPI_LONG, MPI_SUM, root);
}

void
mpi_runtime::init_runtime_params(sprockit::sim_parameters* params)
{
  requests_.resize(2*nproc_);
  bytes_sent_.resize(nproc_);
  bytes_recvd_.resize(nproc_);
  parallel_runtime::init_runtime_params(params);
}

mpi_runtime::mpi_runtime(sprockit::sim_parameters* params) :
  parallel_runtime(params,
  init_rank(params),
  init_size(params))
{
  epoch_ = 0;
}

int
mpi_runtime::init_size(sprockit::sim_parameters* params)
{
  int inited;
  MPI_Initialized(&inited);
  if (!inited){
    int argc = 1;
    char** argv = nullptr;
    int rc = MPI_Init(&argc, &argv);
    if (rc != MPI_SUCCESS){
      spkt_abort_printf("mpi_runtime::init_rank: could not MPI_Init");
    }
  }
  MPI_Comm_size(MPI_COMM_WORLD, &nproc_);
  return nproc_;
}

int
mpi_runtime::init_rank(sprockit::sim_parameters* params)
{
  int inited;
  MPI_Initialized(&inited);
  if (!inited){
    int argc = 1;
    char** argv = nullptr;
    int rc = MPI_Init(&argc, &argv);
    if (rc != MPI_SUCCESS){
      spkt_abort_printf("mpi_runtime::init_rank: could not MPI_Init");
    }
  }
  MPI_Comm_rank(MPI_COMM_WORLD, &me_);
  return me_;
}

void
mpi_runtime::bcast(void *buffer, int bytes, int root)
{
  if (bytes < 0 || bytes > 1e9){
    spkt_throw_printf(
        sprockit::value_error,
        "Illegbal number of bytes %d in broadcast\n",
        bytes);
  }
  MPI_Bcast(buffer, bytes, MPI_BYTE, root, MPI_COMM_WORLD);
}

void
mpi_runtime::send_recv_messages()
{
  int reqIdx = 0;
  for (int i=0; i < nproc_; ++i){
    comm_buffer& comm = send_buffers_[i];
    int size = comm.bytesUsed();
    if (size){
      debug_printf(sprockit::dbg::parallel, "lp %d sending %d bytes to lp %d on epoch %d",
                   me_, size, i, epoch_);
      MPI_Isend(comm.buffer(), size, MPI_BYTE, i, epoch_, MPI_COMM_WORLD, &requests_[reqIdx++]);
    }
    bytes_sent_[i] = size;
  }

  MPI_Alltoall(bytes_sent_.data(), 1, MPI_INT, bytes_recvd_.data(), 1, MPI_INT, MPI_COMM_WORLD);

  for (int i=0; i < nproc_; ++i){
    int size = bytes_recvd_[i];
    if (size){
      comm_buffer& comm = recv_buffers_[i];
      comm.ensureSpace(size);
      debug_printf(sprockit::dbg::parallel, "lp %d receiving %d bytes from lp %d on epoch %d",
                   me_, size, i, epoch_);
      MPI_Irecv(comm.buffer(), size, MPI_BYTE, i, epoch_, MPI_COMM_WORLD, &requests_[reqIdx++]);
      comm.shift(size);
    }
  }

  MPI_Waitall(reqIdx, requests_.data(), MPI_STATUSES_IGNORE);
  ++epoch_;
}

void
mpi_runtime::send(int dst, void *buffer, int buffer_size)
{
  int tag = me_; //the source of the message is the tag
  MPI_Send(buffer, buffer_size, MPI_BYTE, dst, tag, MPI_COMM_WORLD);
}

void
mpi_runtime::recv(int src, void *buffer, int buffer_size)
{
  int tag = src;
  MPI_Recv(buffer, buffer_size, MPI_BYTE, src, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
}

void
mpi_runtime::gather(void *send_buffer, int num_bytes, void *recv_buffer, int root)
{
  MPI_Gather(send_buffer, num_bytes, MPI_BYTE, recv_buffer, num_bytes, MPI_BYTE, root, MPI_COMM_WORLD);
}

void
mpi_runtime::allgather(void *send_buffer, int num_bytes, void *recv_buffer)
{
  MPI_Allgather(send_buffer, num_bytes, MPI_BYTE, recv_buffer, num_bytes, MPI_BYTE, MPI_COMM_WORLD);
}

void
mpi_runtime::finalize()
{
  MPI_Barrier(MPI_COMM_WORLD);
  MPI_Finalize();
}

}
}
