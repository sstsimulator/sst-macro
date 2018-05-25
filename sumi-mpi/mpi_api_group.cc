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

#include <sumi-mpi/mpi_api.h>

namespace sumi {

int
mpi_api::group_range_incl(MPI_Group oldgrp, int n, int ranges[][3], MPI_Group* newgrp)
{
  std::vector<int> new_ranks;
  for (int i=0; i < n; ++i){
    int start = ranges[i][0];
    int stop = ranges[i][1];
    int stride = ranges[i][2];
    int rank = start;
    if (stride < 0){ //stride can be negativve
      if (start < stop){
        spkt_abort_printf("MPI_group_range_incl: negative stride, but start < stop");
      }
      while (rank >= stop){
        new_ranks.push_back(rank);
        rank += stride;
      }
    } else {
      if (stop < start){
        spkt_abort_printf("MPI_group_range_incl: positive stride, but stop < start");
      }
      while (rank <= stop){
        new_ranks.push_back(rank);
        rank += stride;
      }
    }
  }

  return group_incl(oldgrp, new_ranks.size(), new_ranks.data(), newgrp);
}

int
mpi_api::group_incl(MPI_Group oldgrp, int num_ranks, const int *ranks, MPI_Group *newgrp)
{
  mpi_group* oldgrpPtr = get_group(oldgrp);
  if (num_ranks > oldgrpPtr->size()) {
    spkt_abort_printf("MPI_Group_incl: invalid group size %d", num_ranks);
  }

  std::vector<task_id> vec_ranks(num_ranks, task_id(0));
  for (int i = 0; i < num_ranks; i++) {
    vec_ranks[i] = oldgrpPtr->at(ranks[i]);
  }
  mpi_group* newgrpPtr = new mpi_group(vec_ranks);
  add_group_ptr(newgrpPtr, newgrp);

  mpi_api_debug(sprockit::dbg::mpi, "MPI_Group_incl(%d,%d,*%d)",
                num_ranks, oldgrp, *newgrp);

  return MPI_SUCCESS;
}

bool
mpi_api::group_create_with_id(MPI_Group group, int num_members, const uint32_t *members)
{
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Group_create_with_id(id=%d,n=%d)",
                group, num_members);

  int my_rank = comm_world()->rank();
  bool in_group = false;
  for (int i=0; i < num_members; ++i){
    if (members[i] == my_rank){
      in_group = true;
      break;
    }
  }

  if (!in_group) return false;

  std::vector<task_id> vec_ranks(num_members);
  for (int i=0; i < num_members; ++i){
    vec_ranks[i] = members[i];
  }
  mpi_group* grpPtr = new mpi_group(vec_ranks);
  add_group_ptr(grpPtr, &group);

  return true;
}

int
mpi_api::group_free(MPI_Group *grp)
{
  //do not delete it, leave it around
  //forever and ever and ever
  if (*grp != MPI_GROUP_WORLD){
    auto iter = grp_map_.find(*grp);
    if (iter == grp_map_.end()){
      spkt_abort_printf("Invalid MPI_Group %d passed to group free", *grp);
    }
    delete iter->second;
    grp_map_.erase(iter);
  }
  *grp = MPI_GROUP_NULL;
  return MPI_SUCCESS;
}

int
mpi_api::group_translate_ranks(MPI_Group grp1, int n, const int *ranks1, MPI_Group grp2, int *ranks2)
{
  mpi_group* grp1ptr = get_group(grp1);
  mpi_group* grp2ptr = get_group(grp2);
  grp1ptr->translate_ranks(n, ranks1, ranks2, grp2ptr);
  return MPI_SUCCESS;
}

}

