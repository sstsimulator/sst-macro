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
#include <dharma-mpi/mpi_status.h>
#include <dharma-mpi/mpi_message.h>


namespace sstmac {
namespace sumi {

class mpi_request  {
  // ------- constructor / boost stuff -------------//

 public:
  mpi_request(const sw::key::category& cat);

  virtual std::string
  to_string() const {
    return "mpirequest";
  }

  virtual
  ~mpi_request();

  static mpi_request*
  construct(const sw::key::category& cat);
  // --------------------------------------//

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

  const MPI_Status&
  status() const {
    return stat_;
  }

  sw::key*
  get_key() const {
    return key_;
  }

  bool
  is_cancelled() const {
    return cancelled_;
  }

  virtual bool
  is_persistent() const {
    return false;
  }

 protected:
  MPI_Status stat_;
  sw::key* key_;
  bool complete_;
  bool cancelled_;
};

}
} //end of namespace sstmac

#endif

