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

#include <math.h>
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/software/libraries/compute/lib_compute_matrix.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/key.h>

namespace sstmac {
namespace sw {

void
lib_compute_matrix::double_mxm(long nrow, long nlink, long ncol,
                               long nthread)
{
  double_compute(nrow * nlink * ncol,
                 nrow * ncol + nrow * nlink + ncol * nlink, nrow * ncol, nrow * ncol,
                 0, 0, nrow * nlink * ncol, nthread);
}

void
lib_compute_matrix::double_add(long nrow, long ncol, long nthread)
{
  double_compute(2 * nrow * ncol, 2 * nrow * ncol, nrow * ncol, nrow * ncol,
                 nrow * ncol, 0, 0, nthread);
}

void
lib_compute_matrix::double_copy(long nrow, long ncol, long nthread)
{
  double_compute(nrow * ncol, nrow * ncol, nrow * ncol, nrow * ncol, 0, 0,
                 0, nthread);
}

void
lib_compute_matrix::double_compute(long ndata_read,
                                   long working_set_size_read, long ndata_write,
                                   long working_set_size_write, long nadd, long nmult, long nmultadd,
                                   long nthread)
{
  auto inst = new basic_compute_event;
  basic_instructions_st& inst_st = inst->data();
  inst_st.flops = ceil(double(nadd + nmult + 2 * nmultadd) / nthread);
  doing_memory_ = true;
  lib_compute_memmove::read(working_set_size_read / nthread);
  lib_compute_memmove::write(working_set_size_write / nthread);
  doing_memory_ = false;

  lib_compute_inst::compute_inst(inst);
  delete inst;
}

}
} //end of namespace sstmac

