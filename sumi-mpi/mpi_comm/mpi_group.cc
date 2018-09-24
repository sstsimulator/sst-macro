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

#include <sumi-mpi/mpi_comm/mpi_group.h>
#include <sumi-mpi/mpi_types.h>
#include <sprockit/errors.h>
#include <sprockit/stl_string.h>

namespace sumi {

mpi_group::mpi_group(const std::vector<task_id>& tl) :
  local_to_world_map_(tl),
  is_comm_world_(false),
  size_(tl.size())
{
}

mpi_group::mpi_group(size_t size) :
 size_(size),
 is_comm_world_(true)
{
} // the comm_world constructor to save space

task_id
mpi_group::at(int rank)
{
  if (is_comm_world_){
    return task_id(rank);
  } else {
    if (rank >= local_to_world_map_.size()){
      spkt_throw_printf(sprockit::value_error,
                        "invalid rank %d requested for MPI group %p of size %d with ranks %s",
                        rank, this, local_to_world_map_.size(),
                        local_to_world_map_.size() < 6 ? stl_string(local_to_world_map_).c_str() : "");
    }
    return local_to_world_map_[rank];
  }
}

void
mpi_group::translate_ranks(int n_ranks, const int* my_ranks, int* other_ranks, mpi_group* other_grp){
  for (int i=0; i < n_ranks; ++i){
    int input_rank = my_ranks[i];
    if (input_rank == MPI_PROC_NULL){
      other_ranks[i] = MPI_PROC_NULL;
    } else {
      int global_rank_i = is_comm_world_ ? input_rank : local_to_world_map_[input_rank];
      other_ranks[i] = other_grp->rank_of_task(global_rank_i);
    }
  }
}

int
mpi_group::rank_of_task(task_id t) const
{
  if (is_comm_world_){
    return int(t);
  } else {
    for (int i=0; i < local_to_world_map_.size(); ++i){
      if (local_to_world_map_[i] == t){
        return int(i);
      }
    }
  }
  return MPI_UNDEFINED;
}

}
