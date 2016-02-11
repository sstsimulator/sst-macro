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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_TYPES_MPIOP_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_TYPES_MPIOP_H_INCLUDED

#include <sstmac/common/sstmac_config.h>
#include <string>

namespace sstmac {
namespace sw {

/*
 * Operations for simulating data reduction.
 */
struct mpi_op {
  std::string label;

  int id;

  explicit
  mpi_op(const std::string &labelit = "");

  typedef void (*op_fxn)(void *, void *, int *, long *);
  mpi_op(op_fxn fxn, int commute);

  mpi_op(mpi_op* other);

  mpi_op&
  operator=(const mpi_op& other);

  void
  deep_copy(const mpi_op& other);

  op_fxn userfunc_;

  int commute_;

  // Common reduction operations.
  static mpi_op* max;
  static mpi_op* min;
  static mpi_op* sum;
  static mpi_op* prod;
  static mpi_op* land;
  static mpi_op* band;
  static mpi_op* lor;
  static mpi_op* bor;
  static mpi_op* lxor;
  static mpi_op* bxor;
  static mpi_op* minloc;
  static mpi_op* maxloc;
  static mpi_op* replace;
  static mpi_op* op_null;

  static void
  delete_statics();

};

}
} // end of namespace sstmac.

#endif

