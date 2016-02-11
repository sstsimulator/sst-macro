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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_RMA_MPI_WINDOW_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_RMA_MPI_WINDOW_H_INCLUDED

#include <iosfwd>
#include <sstmac/common/messages/wrapper_payload.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_info.h>
#include <sstmac/libraries/mpi/rma/mpi_rma_message.h>
#include <sstmac/libraries/mpi/mpi_payload.h>
#include <stdio.h>
#include <string.h>

namespace sstmac {
namespace sw {
class mpi_window  {

 public:
  mpi_window(void* base, size_t size, int disp, mpi_info* inf,
             mpi_comm* comm, long id) :
    base_(base), size_(size), disp_(disp), inf_(inf), comm_(comm),
    winid_(id) {
    epoch_ = 0;
  }

  virtual
  ~mpi_window() {
  }

  virtual std::string
  to_string() const {
    return "mpi_window";
  }

  bool
  contains_addr(void* addr) {
    size_t a1 = (size_t) addr;
    size_t a2 = (size_t) base_;
    size_t upper = (size_t) ((char*) base_ + size_);
    return a1 >= a2 && a1 < upper;
  }

  mpi_comm*
  comm() {
    return comm_;
  }

  long
  winid() const {
    return winid_;
  }

  long
  current_epoch() const {
    return epoch_;
  }

  void
  next_epoch() {
    epoch_++;
  }

  payload::const_ptr
  get_data(const mpi_rma_message::op_info &inf) const {
    char* bcast = (char*) base_;
    bcast += (disp_ * inf.disp_);
    return new mpi_payload(bcast, mpi_type::mpi_char, inf.count_, true);
  }

  void
  put_data(const mpi_rma_message::op_info &inf, payload::const_ptr load) {
    char* bcast = (char*) base_;
    bcast += (disp_ * inf.disp_);

    mpi_payload::const_ptr cast = ptr_safe_cast(const mpi_payload, load,
        "mpi_window::put_data");

    // char* d = (char*) cast->data();
    // memcpy(bcast, d, inf.count_);
    cast->extract(bcast);
  }

  void
  acc_data(const mpi_rma_message::op_info &inf,
    payload::const_ptr load,
    mpi_type* t) {
    char* bcast = (char*) base_;
    bcast += (disp_ * inf.disp_);

    mpi_payload::const_ptr them = ptr_safe_cast(const mpi_payload, load,
        "mpi_window::acc_data");

    // char* d = (char*) cast->data();

    mpi_payload::const_ptr me = new mpi_payload(bcast, t,
                                inf.count_ / t->packed_size(), true);
    // mpipayload::const* them = mpipayload::construct(d, t,
    //     inf.count_ / t.get_packed_size(), true);

    payload::const_ptr res;

    if (inf.op_ == mpi_op::min) {
      res = me->min(them);
    }
    else if (inf.op_ == mpi_op::max) {
      res = me->max(them);
    }
    else if (inf.op_ == mpi_op::sum) {
      res = me->add(them);
    }
    else if (inf.op_ == mpi_op::prod) {
      res = me->prod(them);
    }
    else if (inf.op_ == mpi_op::replace) {
      res = me->replace(them);
    }
    else {
      spkt_throw_printf(sprockit::spkt_error,
                       "mpi_window::acc_data() - operation currently not supported");
    }

    mpi_payload::const_ptr rescast = ptr_safe_cast(const mpi_payload, load,
        "mpi_window::put_data");

    rescast->extract(bcast);

    //  memcpy(bcast, d, inf.count_);
  }

 protected:
  void* base_;
  size_t size_;
  int disp_;
  mpi_info* inf_;
  mpi_comm* comm_;
  long winid_;

  long epoch_;

};
}
}
#endif

