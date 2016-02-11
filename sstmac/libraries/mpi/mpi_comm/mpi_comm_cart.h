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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_COMM_MPICOMM_CART_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_COMM_MPICOMM_CART_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_comm/mpi_comm.h>

namespace sstmac {
namespace sw {

/**
 * An MPI_Cart communicator handle.
 */
class mpi_comm_cart : public mpi_comm
{

 public:
  /// Hello.
  mpi_comm_cart(
    const mpi_comm_id &id, //const appid &aid,
    mpi_id rank, mpi_group* peers,
    app_manager*env, const app_id &aid, int ndims,
    int *dims, int *periods, int reorder);

  /// Goodbye.
  virtual
  ~mpi_comm_cart() {
  }

  int
  get_dim(int i) const {
    if (i > dims_.size()) {
      return -1;
    }
    return dims_[i];
  }

  void set_coords(int rank, int* coords);

  int
  get_period(int i) const {
    if (i > periods_.size()) {
      return -1;
    }
    return periods_[i];
  }

  int get_rank(int* coords);


  int shift(int dir, int dis);

  int
  get_ndims() const {
    return ndims_;
  }

 protected:
  int ndims_;
  std::vector<int> dims_;
  std::vector<int> periods_;
  int reorder_;

};
} //end of namespace sstmac
}
#endif

