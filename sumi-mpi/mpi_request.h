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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIREQUEST_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIREQUEST_H_INCLUDED

#include <sstmac/software/process/key.h>
#include <sumi-mpi/mpi_status.h>
#include <sumi-mpi/mpi_message.h>

namespace sumi {

using sstmac::sw::key;

/**
 * Persistent send operations (send, bsend, rsend, ssend)
 */
class persistent_op
{
 public:
  typedef enum {
    Send,
    Recv
  } op_type_t;
  /// The arguments.
  int count;
  MPI_Datatype datatype;
  MPI_Comm comm;
  op_type_t optype;
  int partner;
  int tag;
  void* content;

};

class mpi_request  {
  // ------- constructor / boost stuff -------------//

 public:
  mpi_request(const key::category& cat);

  std::string
  to_string() const {
    return "mpirequest";
  }

  static mpi_request*
  construct(const key::category& cat);
  // --------------------------------------//

  ~mpi_request();

  void
  complete(const mpi_message::ptr& msg);

  bool
  is_complete() const {
    return complete_;
  }

  void
  cancel() {
    cancelled_ = true;
    complete(mpi_message::ptr());
  }

  void
  persist(persistent_op* op) {
    persistent_op_ = op;
  }

  persistent_op*
  op() const {
    return persistent_op_;
  }

  const MPI_Status&
  status() const {
    return stat_;
  }

  key*
  get_key() const {
    return key_;
  }

  bool
  is_cancelled() const {
    return cancelled_;
  }

  bool
  is_persistent() const {
    return persistent_op_;
  }

 protected:
  MPI_Status stat_;
  key* key_;
  bool complete_;
  bool cancelled_;
  persistent_op* persistent_op_;
};

}

#endif

