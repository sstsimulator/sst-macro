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
  /// Build comm_world using information retrieved from the environment.
  mpi_comm_factory(app_id aid, mpi_api* parent);

  /// Goodbye.
  ~mpi_comm_factory();

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

 private:
  MPI_Comm comm_new_id_agree(MPI_Comm old);

 private:
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