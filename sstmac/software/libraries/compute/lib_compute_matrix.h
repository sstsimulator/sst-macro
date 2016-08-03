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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_LIB_COMPUTE_MATRIX_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_LIB_COMPUTE_MATRIX_H_INCLUDED

#include <sstmac/software/libraries/compute/lib_compute_inst.h>
#include <sstmac/software/libraries/compute/lib_compute_memmove.h>

namespace sstmac {
namespace sw {

class lib_compute_matrix :
  public lib_compute_memmove
{
  // ------- constructor / boost stuff -------------//

 public:
  lib_compute_matrix(software_id id) :
    lib_compute_memmove("computelibmatrix", id) {
  }

  virtual
  ~lib_compute_matrix() { }

  void
  double_mxm(long nrow, long nlink, long ncol, long nthread = 1);

  void
  double_add(long nrow, long ncol, long nthread = 1);

  void
  double_copy(long nrow, long ncol, long nthread = 1);

 protected:
  void
  double_compute(long ndata_read, long working_set_size_read,
                 long ndata_write, long working_set_size_write,
                 long nadd, long nmult,
                 long nmultadd, long nthread);

 protected:
  bool doing_memory_;

};

}
} //end of namespace sstmac

#endif

