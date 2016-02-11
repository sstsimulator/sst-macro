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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPICORESSEND_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPICORESSEND_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_strategies.h>

namespace sstmac {
namespace sw {

/// Basic strategy for a "fire-and-forget" send operation.
class mpi_core_ssend : public mpi_send_strategy
{

 public:
  /// Goodbye.
  virtual ~mpi_core_ssend() throw();

  /// Basic nonblocking send.  A simulator may opt to delay completion of
  /// this operation depending on message size.
  virtual void
  execute(mpi_request* thekey,
          mpi_queue* queue,
          int count, mpi_type_id type, mpi_id dest,
          mpi_tag tag, mpi_comm* comm,
          const payload::const_ptr& content,
          operating_system* os) const;
};

}
} // end of namespace sstmac

#endif

