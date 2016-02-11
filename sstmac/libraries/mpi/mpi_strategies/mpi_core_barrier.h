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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPICOREBARRIER_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPICOREBARRIER_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_strategies.h>

namespace sstmac {
namespace sw {

/**
 * Synchronize nodes.
 *
 * Performed using approximately the mechanism from mpich2-1.0.8
 * (see util/hfmiterator.h).
 */
class mpi_core_barrier : public mpi_barrier_strategy
{
 public:
  enum BARRIER_IMPL {LINEAR, RING, HFM};


 public:
  /// Hi.
  mpi_core_barrier(BARRIER_IMPL i);

  BARRIER_IMPL impl_;

  /// Goodbye.
  virtual ~mpi_core_barrier() throw();

  /// Build a kernel to synchronize nodes.
  virtual mpi_collective*
  execute(mpi_request* thekey,
          mpi_queue* queue,
          mpi_tag tag, mpi_comm* comm,
          operating_system* os) const;
};

}
} // end of namespace sstmac

#endif

