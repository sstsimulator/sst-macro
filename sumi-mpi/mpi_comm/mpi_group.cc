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

#include <sumi-mpi/mpi_comm/mpi_group.h>
#include <sprockit/errors.h>
#include <sprockit/stl_string.h>

namespace sumi {

mpi_group::mpi_group(const std::vector<task_id>& tl) :
  task_list_(tl),
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
    if (rank >= task_list_.size()){
      spkt_throw_printf(sprockit::value_error,
                        "invalid rank %d requested for MPI group %p of size %d with ranks %s",
                        rank, this, task_list_.size(),
                        task_list_.size() < 6 ? stl_string(task_list_).c_str() : "");
    }
    return task_list_[rank];
  }
}

int
mpi_group::rank_of_task(task_id t)
{
  if (is_comm_world_){
    return int(t);
  } else {
    for (int i=0; i < task_list_.size(); ++i){
      if (task_list_[i] == t){
        return int(i);
      }
    }
  }
  return int(-1);
}

}