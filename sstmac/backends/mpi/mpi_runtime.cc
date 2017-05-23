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
  debug_printf(sprockit::dbg::parallel, "Rank %d: %s", me_, sprockit::printf(__VA_ARGS__).c_str())

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
  max_num_requests_ = params->get_optional_int_param("mpi_max_num_requests", 1000*1000);
  requests_ = new MPI_Request[max_num_requests_];
  total_num_sent_ = 0;
  parallel_runtime::init_runtime_params(params);

  int rc = MPI_Op_create(pair_reduce_function, 1, &reduce_op_);
  if (rc != MPI_SUCCESS) spkt_abort_printf("failed creating pair reduce function");
}

mpi_runtime::mpi_runtime(sprockit::sim_parameters* params) :
  parallel_runtime(params,
  init_rank(params),
  init_size(params))
{
  epoch_ = 0;

  num_sent_ = new int[2*nproc_];
  ::memset(num_sent_, 0, 2*nproc_ * sizeof(int));
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
mpi_runtime::wait_merge_array(int tag)
{
  merge_request& req = merge_requests_[tag];
  if (req.merged){
    return;  //done
  }

  int* fake_num_sent = new int[nproc_];

  //go to the reduce scatter - but make sure that num sent gets negative
  for (int i=0; i < nproc_; ++i){
    fake_num_sent[i] = -100000;
  }

  int num_sent_to_me;
  MPI_Reduce_scatter_block(fake_num_sent, &num_sent_to_me, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
  do_collective_merges(tag);
  delete[] fake_num_sent;
}

void
mpi_runtime::declare_merge_array(void* buffer, int size, int tag)
{
  merge_request& req = merge_requests_[tag];
  req.buffer = buffer;
  req.size = size;
  req.merged = false;
  req.refcount += 1;
}

bool
mpi_runtime::release_merge_array(int tag)
{
  merge_request& req = merge_requests_[tag];
  --req.refcount;
  if (req.refcount == 0){
    merge_requests_.erase(tag);
    return true;
  }
  return false;
}

void
mpi_runtime::do_merge_array(int tag)
{
  merge_map::iterator it = merge_requests_.find(tag);
  if (it == merge_requests_.end()){
    spkt_throw_printf(sprockit::value_error,
        "mpi_runtime::do_merge_array: invalid merge tag %d",
        tag);
  }http://www.espn.com/
  merge_request& req = it->second;
  MPI_Allreduce(MPI_IN_PLACE, req.buffer, req.size, MPI_BYTE, MPI_BOR, MPI_COMM_WORLD);
  req.merged = true;
}

void
mpi_runtime::do_collective_merges(int my_tag)
{
  int* collectives_to_do = new  int[nproc_];
  int* my_tag_array = new int[1];
  my_tag_array[0] = my_tag;
  MPI_Allgather(my_tag_array, 1, MPI_INT, collectives_to_do, 1, MPI_INT, MPI_COMM_WORLD);
  std::set<int> collective_tags;
  for (int i=0; i < nproc_; ++i){
    int next_tag = collectives_to_do[i];
    if (next_tag >= 0){
      collective_tags.insert(next_tag);
    }
  }

  std::set<int>::iterator it, end = collective_tags.end();
  for (it=collective_tags.begin(); it != end; ++it){
    do_merge_array(*it);
  }

  delete[] collectives_to_do;
}

void
mpi_runtime::do_send_recv_messages(std::vector<void*>& buffers)
{
  int num_sent_to_me = 0;
  int outdata[2];
  //reduce scatter the number of messages sent
  //everybode gets one
  MPI_Reduce_scatter_block(num_sent_, outdata, 1, MPI_2INT, reduce_op_, MPI_COMM_WORLD);
  num_sent_to_me = outdata[0];
  while (num_sent_to_me < 0){
    //ooooh, hey - people want to do collectives
    do_collective_merges(-1); //I don't have anything to do
    MPI_Reduce_scatter_block(num_sent_, outdata, 1, MPI_2INT, reduce_op_, MPI_COMM_WORLD);
    num_sent_to_me = outdata[0];
  }

  buffers.resize(num_sent_to_me);

  int bytes_sent_to_me = outdata[1];
  recv_buffer_.resize(bytes_sent_to_me);
  mpi_debug("receiving %d messages of size %d on epoch %d", num_sent_to_me, bytes_sent_to_me, epoch_);
  for (int i=0; i < num_sent_to_me; ++i){
    MPI_Status stat;
    void* buffer = recv_buffer_.ptr;
    MPI_Recv(buffer, recv_buffer_.remaining, MPI_BYTE, MPI_ANY_SOURCE, epoch_, MPI_COMM_WORLD, &stat);
    int count; MPI_Get_count(&stat, MPI_BYTE, &count);
    recv_buffer_.shift(count);
    buffers[i] = buffer;
  }

  mpi_debug("waiting on %d sends", total_num_sent_);
  MPI_Waitall(total_num_sent_, requests_, MPI_STATUSES_IGNORE);

  ::memset(num_sent_, 0, nproc_ * 2 * sizeof(int));
  total_num_sent_ = 0;
  ++epoch_;
}

void
mpi_runtime::reallocate_requests()
{
  int max_requests = max_num_requests_ * 2;
  MPI_Request* new_requests = new MPI_Request[max_requests];
  ::memcpy(new_requests, requests_, max_num_requests_ * sizeof(MPI_Request));
  max_num_requests_ = max_requests;
  delete[] requests_;
  requests_ = new_requests;

  mpi_debug("reallocated to have %d requests", max_num_requests_);
}

void
mpi_runtime::do_send_message(int lp, void *buffer, int size)
{
#if SST_SANITY_CHECK
  if (size > buf_size_){
    spkt_throw(sprockit::illformed_error,
        "mpi_runtime::do_send_message: sending buffer that is too large");
  }
#endif
  int* pairData = &num_sent_[2*lp];
  pairData[0]++;
  pairData[1] += size;
  if (total_num_sent_ == max_num_requests_){
    reallocate_requests();
  }
  MPI_Request* next_req = &requests_[total_num_sent_];
  total_num_sent_++;
  int tag = epoch_;
  MPI_Isend(buffer, size, MPI_BYTE, lp, tag, MPI_COMM_WORLD, next_req);
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