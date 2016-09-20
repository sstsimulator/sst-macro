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

#ifndef MPI_RUNTIME_H
#define MPI_RUNTIME_H

#include <mpi.h>
#include <sstmac/backends/common/parallel_runtime.h>

namespace sstmac {
namespace mpi {

class mpi_runtime :
  public parallel_runtime
{

 public:
  mpi_runtime(sprockit::sim_parameters* params);

  std::string
  to_string() const override {
    return "mpi runtime";
  }

  void
  bcast(void *buffer, int bytes, int root);

  int64_t
  allreduce_min(int64_t mintime);

  int64_t
  allreduce_max(int64_t maxtime);

  void
  global_sum(long long *data, int nelems, int root);

  void
  global_sum(long *data, int nelems, int root);

  void
  global_max(long *data, int nelems, int root);

  void
  global_max(int *data, int nelems, int root);

  void
  send(int dst, void *buffer, int buffer_size);

  void
  recv(int src, void *buffer, int buffer_size);

  void
  gather(void *send_buffer, int num_bytes, void *recv_buffer, int root);

  void
  allgather(void *send_buffer, int num_bytes, void *recv_buffer);

  void
  wait_merge_array(int tag);

  void
  declare_merge_array(void* buffer, int size, int tag);

  bool
  release_merge_array(int tag);

  void
  init_runtime_params(sprockit::sim_parameters* params);

 protected:
  void
  do_send_recv_messages(std::vector<void*>& buffers);

  void
  do_send_message(int lp, void *buffer, int size);

  void
  do_reduce(void* data, int nelems, MPI_Datatype ty, MPI_Op op, int root);

  void
  do_merge_array(int tag);

  void
  do_collective_merges(int my_tag);

  void
  reallocate_requests();

  void finalize();

 private:
  int init_rank(sprockit::sim_parameters* params);
  int init_size(sprockit::sim_parameters* params);

 private:
   MPI_Request* requests_;

   struct merge_request {
     void* buffer;
     int size;
     int refcount;
     bool merged;
   };
   typedef std::map<int, merge_request> merge_map;
   merge_map merge_requests_;

   int max_num_requests_;
   int* num_sent_;
   int* array_of_ones_;
   int total_num_sent_;
   int epoch_;

   bool finalize_needed_;

};

}
}

#endif // MPI_RUNTIME_H
