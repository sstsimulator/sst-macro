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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_MPICOLLECTIVE_SENDER_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_MPICOLLECTIVE_SENDER_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_tag.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_id.h>
#include <sstmac/common/event_handler.h>
#include <sprockit/util.h>

namespace sstmac {
namespace sw {

/**
 * A private mpisend type to handle individual send operations for
 * collective mpi operations.
 */
class mpi_collective::sender : public event_handler
{

 public:
  virtual std::string
  to_string() const {
    return "sender";
  }

  /// Hi.
  sender(mpi_collective* parent) :
    parent_(parent)
  {
  }

  /// Goodbye
  virtual
  ~sender() throw () {
  }

  /// Event handler callback.
  virtual void
  handle(event* ev) {
    parent_->send_complete(safe_cast(mpi_message, ev));
  }

 protected:
  /// The parent collective operation.
  mpi_collective* parent_;

};

}
} // end of namespace sstmac

#endif

