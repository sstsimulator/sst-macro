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

