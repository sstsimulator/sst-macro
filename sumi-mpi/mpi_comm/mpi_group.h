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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_COMM_MPIGROUP_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_COMM_MPIGROUP_H_INCLUDED

#include <sstmac/software/process/task_id.h>
#include <vector>

namespace sumi {

using sstmac::sw::task_id;

class mpi_group  {

 public:
  mpi_group(const std::vector<task_id>& tl);

  mpi_group(size_t size);

  virtual
  ~mpi_group() {
  }

  virtual std::string
  to_string() const {
    return "mpigroup";
  }

  task_id
  at(int rank);

  size_t
  size() const {
    return size_;
  }

  const std::vector<task_id>&
  ids() const {
    return task_list_;
  }

  int
  rank_of_task(task_id t);

 protected:
  std::vector<task_id> task_list_;

  size_t size_; //used for comm_world
  bool is_comm_world_;  //we don't save all the peers to save space

};

}

#endif

