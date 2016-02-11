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

#include <sstmac/libraries/mpi/mpi_types/mpi_op.h>
#include <sstmac/common/thread_info.h>
#include <sstmac/common/thread_safe_int.h>
#include <sprockit/statics.h>

namespace sstmac {
namespace sw {

static thread_safe_int next_id = 0;
static sprockit::need_delete_statics<mpi_op> del_statics;

mpi_op::mpi_op(const std::string &labelit) :
  label(labelit),
  id(0),
  commute_(true)
{
  id = next_id++;
}

mpi_op::mpi_op(op_fxn func, int commute) :
  label("user"), id(0), userfunc_(func), commute_(commute)
{
  id = next_id++;
}

mpi_op::mpi_op(mpi_op* other)
{
  deep_copy(other);
}

mpi_op&
mpi_op::operator=(const mpi_op& other)
{
  deep_copy(other);
  return *this;
}

void
mpi_op::deep_copy(const mpi_op& other)
{
  label = other.label;

  id = other.id;

  commute_ = other.commute_;
  userfunc_ = other.userfunc_;

}

void
mpi_op::delete_statics()
{
  free_static_ptr(max);
  free_static_ptr(min);
  free_static_ptr(sum);
  free_static_ptr(prod);
  free_static_ptr(land);
  free_static_ptr(band);
  free_static_ptr(lor);
  free_static_ptr(bor);
  free_static_ptr(bxor);
  free_static_ptr(lxor);
  free_static_ptr(minloc);
  free_static_ptr(maxloc);
  free_static_ptr(replace);
  free_static_ptr(op_null);
}

mpi_op* mpi_op::max = new mpi_op("MPI_MAX");
mpi_op* mpi_op::min = new mpi_op("MPI_MIN");
mpi_op* mpi_op::sum = new mpi_op("MPI_SUM");
mpi_op* mpi_op::prod = new mpi_op("MPI_PROD");
mpi_op* mpi_op::land = new mpi_op("MPI_LAND");
mpi_op* mpi_op::band = new mpi_op("MPI_BAND");
mpi_op* mpi_op::lor = new mpi_op("MPI_LOR");
mpi_op* mpi_op::bor = new mpi_op("MPI_BOR");
mpi_op* mpi_op::lxor = new mpi_op("MPI_LXOR");
mpi_op* mpi_op::bxor = new mpi_op("MPI_BXOR");
mpi_op* mpi_op::minloc = new mpi_op("MPI_MINLOC");
mpi_op* mpi_op::maxloc = new mpi_op("MPI_MAXLOC");
mpi_op* mpi_op::replace = new mpi_op("MPI_REPLACE");
mpi_op* mpi_op::op_null = new mpi_op("MPI_OP_NULL");

}
} // end of namespace sstmac

