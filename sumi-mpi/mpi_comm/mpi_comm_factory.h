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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_COMM_MPICOMMFACTORY_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_COMM_MPICOMMFACTORY_H_INCLUDED

#include <sumi-mpi/mpi_comm/mpi_comm.h>
#include <sumi-mpi/mpi_types/mpi_type.h>
#include <sumi-mpi/mpi_api_fwd.h>
#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>

namespace sumi {


/**
 * Construct mpi communicators.
 */
class mpi_comm_factory  {

 public:
  virtual std::string
  to_string() const {
    return "mpicommfactory";
  }

  /// Build comm_world using information retrieved from the environment.
  mpi_comm_factory(app_id aid, mpi_api* parent);

  /// Goodbye.
  virtual ~mpi_comm_factory();

  /// Initialize the object.
  void init(int rank, int nproc);

 public:
  /// Get the world communicator for the given node.
  /// In this communicator, mpiid indices are the same as the taskid
  /// indices given by the environment.  Each node will only have one world.
  mpi_comm*
  world(){
    return worldcomm_;
  }

  /// Get a 'self' communicator.  Each node will have a unique index
  /// for its self.  Each node will only have one self.
  mpi_comm*
  self(){
    return selfcomm_;
  }


  /// Duplicate the given communicator.
  /// Blocks unti lall nodes have entered the call.
  /// Eventually we may opt to deal with the extended attributes (keyval)
  /// stuff in the MPI standard, even though I don't know anybody
  /// who actually uses it.
  mpi_comm* comm_dup(mpi_comm*caller);

  /// Make the given mpiid refer to a newly created communicator.
  /// Blocks the caller until all nodes have entered the call.
  /// This call must be performed by all nodes in the caller communicator
  /// a creating any new communicators.  Returns mpicomm::null_comm
  /// on all nodes that are not participants the new group.
  mpi_comm* comm_create(mpi_comm* caller, mpi_group* group);

  /// MPI_Comm_split -- collective operation.
  mpi_comm* comm_split(mpi_comm* caller, int color, int key);

  mpi_comm* create_cart(mpi_comm* caller, int ndims,
                        const int *dims, const int *periods, int reorder);

 protected:

  mpi_api* parent_;

  app_id aid_;

  /// We can restrict our run to use fewer than the nodes allocated.
  int mpirun_np_;

  /// The next available communicator index.
  MPI_Comm next_id_;

  mpi_comm* worldcomm_;
  mpi_comm* selfcomm_;
};

}

#endif

