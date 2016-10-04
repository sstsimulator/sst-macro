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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_COMM_KEYVAL_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_COMM_KEYVAL_H_INCLUDED

#include <sumi-mpi/sstmac_mpi_integers.h>
#include <string>

namespace sumi {

class keyval  {

 public:
  keyval(int k, MPI_Copy_function* c, MPI_Delete_function* d, void* e) :
    key_(k), copy_(c), del_(d), extra_(e) {
  }

  virtual
  ~keyval() {
  }

  int
  key() const {
    return key_;
  }

  keyval*
  clone(int k) const;

  void
  set_val(void* val) {
    val_ = val;
  }

  void*
  val() const {
    return val_;
  }

 protected:
  int key_;
  MPI_Copy_function* copy_;
  MPI_Delete_function* del_;
  void* extra_;
  void* val_;

};

}

#endif /* KEYVAL_H_ */

