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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPICOREBCAST_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPICOREBCAST_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_strategies.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/topology_iterator.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>

namespace sstmac {
namespace sw {

/**
 * Broadcast between nodes.
 */
class mpi_core_bcast : public mpi_bcast_strategy
{

 public:
  /// Goodbye.
  virtual
  ~mpi_core_bcast() throw ();

  /// Build a kernel to perform a broadcast.
  virtual mpi_collective*
  execute(mpi_request* thekey,
          mpi_queue* queue, int count,
          mpi_type_id type, mpi_id root, mpi_tag tag,
          mpi_comm* comm, const payload::const_ptr& content,
          operating_system* os) const;

 protected:
  void
  sendrecv();
};

}
} // end of namespace sstmac

#endif

