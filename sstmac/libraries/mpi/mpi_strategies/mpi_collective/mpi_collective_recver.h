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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_MPICOLLECTIVE_RECVER_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_MPICOLLECTIVE_RECVER_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_tag.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_id.h>
#include <sstmac/libraries/mpi/mpi_message.h>
#include <sstmac/common/event_handler.h>

namespace sstmac {
namespace sw {

/**
 * A private mpirecv type to handle individual recv operations for
 * collective mpi operations.
 */
class mpi_collective::recver : public event_handler
{

 public:
  virtual std::string
  to_string() const {
    return "recver";
  }

  /// Hi.
  recver(mpi_collective* parent) :
    parent_(parent) {
  }

  /// Goodbye
  virtual
  ~recver() throw () {
  }

  /// Event handler callback.
  virtual void
  handle(sst_message* msg);

 private:
  /// The parent collective operation.
  mpi_collective* parent_;

};

}
} // end of namespace sstmac

#endif

