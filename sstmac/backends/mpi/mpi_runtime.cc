/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/backends/mpi/mpi_runtime.h>
#include <sstmac/common/thread_info.h>
#include <cstring>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
"mpi_max_num_requests",
);

#define mpi_debug(...) \
  debug_printf(sprockit::dbg::parallel, "Rank %d: %s", me_, sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace mpi {

SpktRegister("mpi", parallel_runtime, mpi_runtime);

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
}

mpi_runtime::mpi_runtime(sprockit::sim_parameters* params) :
  finalize_needed_(false),
  parallel_runtime(params,
  init_rank(params),
  init_size(params))
{
  epoch_ = 0;
  array_of_ones_ = new int[nproc_];
  for (int i=0; i < nproc_; ++i){
    array_of_ones_[i] = 1;
  }

  num_sent_ = new int[nproc_];
  ::memset(num_sent_, 0, nproc_ * sizeof(int));
}

int
mpi_runtime::init_size(sprockit::sim_parameters* params)
{
  int inited;
  MPI_Initialized(&inited);
  if (!inited){
    //nothing, fake it
    finalize_needed_ = true;
    int argc = 1;
    char** argv = 0;
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
    //nothing, fake it
    finalize_needed_ = true;
    int argc = 1;
    char** argv = 0;
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
  MPI_Reduce_scatter(fake_num_sent, &num_sent_to_me, array_of_ones_, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
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
  }
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
  //reduce scatter the number of messages sent
  MPI_Reduce_scatter(num_sent_, &num_sent_to_me, array_of_ones_, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
  while (num_sent_to_me < 0){
    //ooooh, hey - people want to do collectives
    do_collective_merges(-1); //I don't have anything to do
    MPI_Reduce_scatter(num_sent_, &num_sent_to_me, array_of_ones_, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
  }

  buffers.resize(num_sent_to_me);

  mpi_debug("receiving %d messages on epoch %d",
    num_sent_to_me, epoch_);


  for (int i=0; i < num_sent_to_me; ++i){
    int reqidx = i + total_num_sent_;
    if (reqidx >= max_num_requests_){
      reallocate_requests();
    }
    MPI_Request* reqptr = &requests_[reqidx];
    void* buffer = recv_buffer_pool_.pop();
    MPI_Irecv(buffer, buf_size_, MPI_BYTE, MPI_ANY_SOURCE, epoch_, MPI_COMM_WORLD, reqptr);
    buffers[i] = buffer;
  }


  int total_pending_ops = num_sent_to_me + total_num_sent_;
  mpi_debug("waiting on %d requests: %d recvs and %d sends",
    total_pending_ops, num_sent_to_me, total_num_sent_);
  MPI_Waitall(total_pending_ops, requests_, MPI_STATUSES_IGNORE);

  ::memset(num_sent_, 0, nproc_ * sizeof(int));
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
  num_sent_[lp]++;
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
  if(finalize_needed_) {
    MPI_Finalize();
  }
}

}
}
