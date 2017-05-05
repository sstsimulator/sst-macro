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

namespace sumi {


/**
 * Construct mpi communicators.
 */
class mpi_comm_factory  {

 public:
  mpi_comm_factory(software_id sid, mpi_api* parent);

  ~mpi_comm_factory();

  void init(int rank, int nproc);

 public:
  mpi_comm* world() const {
    return worldcomm_;
  }

  mpi_comm* self() const {
    return selfcomm_;
  }

  mpi_comm* comm_dup(mpi_comm*caller);

  mpi_comm* comm_create(mpi_comm* caller, mpi_group* group);

  mpi_comm* comm_split(mpi_comm* caller, int color, int key);

  mpi_comm* create_cart(mpi_comm* caller, int ndims,
                        const int *dims, const int *periods, int reorder);

 private:
  MPI_Comm comm_new_id_agree(MPI_Comm old);

 private:
  mpi_api* parent_;

  app_id aid_;

  int mpirun_np_;

  MPI_Comm next_id_;

  mpi_comm* worldcomm_;
  mpi_comm* selfcomm_;
};

}

#endif

