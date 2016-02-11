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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPICOREALLTOALLV_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPICOREALLTOALLV_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_strategies.h>

namespace sstmac {
namespace sw {

//
// Default strategy for alltoallv operations with variable data lengths.
//
class mpi_core_alltoallv : public mpi_alltoallv_strategy
{

 public:
  /// Must be virtual.
  virtual ~mpi_core_alltoallv() throw();

  /// Build a kernel for a fused reduce-alltoallv operation
  virtual mpi_collective*
  execute(mpi_request* thekey,
          mpi_queue* queue,
          const std::vector<int> &sendcnts, mpi_type_id sendtype,
          const std::vector<int> &recvcnts, mpi_type_id recvtype,
          mpi_tag tag, mpi_comm* comm,
          const std::vector<payload::const_ptr >& content,
          operating_system* os) const;
};

}
} // end of namespace sstmac

#endif

