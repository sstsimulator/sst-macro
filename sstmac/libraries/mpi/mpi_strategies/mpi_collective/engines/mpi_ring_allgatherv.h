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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIRINGALLGATHERV_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIRINGALLGATHERV_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_payload.h>
#include <deque>

namespace sstmac {
namespace sw {


/**
 * Perform an allgatherv using a ring.
 * Each node performs size-1 iterations where it receives data from node
 * rank-1 and sends to node rank+1 (where both ranks wrap around).
 */
class mpi_ring_allgatherv : public mpi_collective
{

 public:
  std::string
  to_string() const {
    return sprockit::printf("mpiringallgatherv(tag=%d)", int(tag_));
  }

  mpi_ring_allgatherv(mpi_request* thekey,
                      mpi_queue* queue,
                      int sendcnt, mpi_type_id sendtype,
                      const std::vector<int> &recvcnt, mpi_type_id recvtype,
                      mpi_tag tag, mpi_comm* comm,
                      const payload::const_ptr& content,
                      event_handler* completion);

  /// Goodbye.
  virtual ~mpi_ring_allgatherv() throw();

  /// Get busy child.
  virtual void start();

 protected:
  /// Callback method to indicate that a send operation has completed.
  virtual void
  send_complete(mpi_message* msg);

  /// Callback method to indicate that a receive operation has completed.
  virtual void
  recv_complete(mpi_message* msg);

 protected:
  /// Private nested type to hold send and receive counts.
  struct srpair {
    explicit srpair(int send = -1, int recv = -1) :
      sendcnt(send), recvcnt(recv) {
    }
    int sendcnt;
    int recvcnt;
  };

  /// The number of elements sent to each node.
  int sendcnt_;
  /// The type we're sending.
  mpi_type_id sendtype_;
  /// The number of elements received from each node.
  std::vector<int> recvcnt_;
  /// The type we are receiving.
  mpi_type_id recvtype_;

  /// The remaining iteration rounds (we pop_front on each iteration).
  std::deque<srpair> iteration_;
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

