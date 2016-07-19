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

#include <sumi-mpi/mpi_comm/mpi_comm.h>

namespace sumi {

using sstmac::sw::app_launch;
using sstmac::sw::app_id;

class mpi_comm_cart : public mpi_comm
{

 public:
  /// Hello.
  mpi_comm_cart(
    MPI_Comm id,
    int rank, mpi_group* peers,
    app_id aid, int ndims,
    const int *dims,
    const int *periods,
    int reorder);

  /// Goodbye.
  virtual
  ~mpi_comm_cart() {
  }

  int
  dim(int i) const {
    if (i > dims_.size()) {
      return -1;
    }
    return dims_[i];
  }

  void set_coords(int rank, int* coords);

  int
  period(int i) const {
    if (i > periods_.size()) {
      return -1;
    }
    return periods_[i];
  }

  int rank(const int* coords);

  int shift(int dir, int dis);

  int
  ndims() const {
    return ndims_;
  }

 protected:
  int ndims_;
  std::vector<int> dims_;
  std::vector<int> periods_;
  int reorder_;

};

}
#endif

