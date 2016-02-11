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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIRINGALLGATHER_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIRINGALLGATHER_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_payload.h>
#include <sstmac/libraries/mpi/mpi_status.h>

namespace sstmac {
namespace sw {

/**
 * Perform an allgather using a ring.
 * Each node performs size-1 iterations where it receives data from node
 * rank-1 and sends to node rank+1 (where both ranks wrap around).
 */

class mpi_ring_allgather : public mpi_collective
{

 public:
  std::string
  to_string() const {
    return sprockit::printf("mpiringallgather(tag=%d)", int(tag_));
  }

  mpi_ring_allgather(mpi_request* thekey,
                     mpi_queue* queue,
                     int sendcnt, mpi_type_id sendtype,
                     int recvcnt, mpi_type_id recvtype,
                     mpi_tag tag, mpi_comm* comm,
                     const payload::const_ptr& content,
                     event_handler* completion);
  /// Goodbye.
  virtual
  ~mpi_ring_allgather() throw ();

  /// Get busy child.
  virtual void
  start();

 protected:
  /// Callback method to indicate that a send operation has completed.
  virtual void
  send_complete(const mpi_message::ptr& msg);

  /// Callback method to indicate that a receive operation has completed.
  virtual void
  recv_complete(const mpi_message::ptr& msg);

 protected:
  /// The number of elements sent.
  int sendcnt_;
  /// The type we're sending.
  mpi_type_id sendtype_;
  /// The number of elements received.
  int recvcnt_;
  /// The type we are receiving.
  mpi_type_id recvtype_;
  /// The current iteration round (we perform size-1 iterations).
  int iteration_;
  /// The target iterations.
  const int target_iterations_;
  /// The node we will be receiving from.
  const mpi_id source_;
  /// The node we will be sending to.
  const mpi_id dest_;
  /// The number of pending sends (usually zero or one).
  int pending_sends_;
  /// The number of pending receives (usually zero or one).
  int pending_recvs_;

  mpi_collective_payload::ptr content_;

};

}
} // end of namespace sstmac

#endif

