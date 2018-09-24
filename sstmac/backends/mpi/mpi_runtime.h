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

#ifndef MPI_RUNTIME_H
#define MPI_RUNTIME_H

#include <mpi.h>
#include <sstmac/backends/common/parallel_runtime.h>

namespace sstmac {
namespace mpi {

class mpi_runtime :
  public parallel_runtime
{
  FactoryRegister("mpi", parallel_runtime, mpi_runtime)
 public:
  mpi_runtime(sprockit::sim_parameters* params);

  void bcast(void *buffer, int bytes, int root) override;

  int64_t allreduce_min(int64_t mintime) override;

  int64_t allreduce_max(int64_t maxtime) override;

  void global_sum(long long *data, int nelems, int root) override;

  void global_sum(long *data, int nelems, int root) override;

  void global_max(long *data, int nelems, int root) override;

  void global_max(int *data, int nelems, int root) override;

  void send(int dst, void *buffer, int buffer_size) override;

  void recv(int src, void *buffer, int buffer_size) override;

  void gather(void *send_buffer, int num_bytes, void *recv_buffer, int root) override;

  void allgather(void *send_buffer, int num_bytes, void *recv_buffer) override;

  void init_runtime_params(sprockit::sim_parameters* params) override;

  timestamp send_recv_messages(timestamp vote) override;

 protected:
  void do_reduce(void* data, int nelems, MPI_Datatype ty, MPI_Op op, int root);

  void finalize() override;

 private:
  int init_rank(sprockit::sim_parameters* params);
  int init_size(sprockit::sim_parameters* params);

 private:
  struct send_recv_vote {
    uint64_t time_vote;
    uint64_t num_sent;
    uint64_t max_bytes;
  };

  std::vector<MPI_Request> requests_;
  std::vector<MPI_Status> statuses_;
  std::vector<send_recv_vote> votes_;

  MPI_Datatype vote_type_;
  MPI_Op vote_op_;

  int epoch_;

  static void vote_reduce_function(void *invec, void *inoutvec, int *len, MPI_Datatype *datatype);
};

}
}

#endif // MPI_RUNTIME_H
