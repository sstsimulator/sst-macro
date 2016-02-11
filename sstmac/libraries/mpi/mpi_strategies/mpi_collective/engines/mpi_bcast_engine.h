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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIBCASTENGINE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIBCASTENGINE_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_tag.h>
#include <sstmac/libraries/mpi/mpi_request.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/topology_iterator.h>
#include <sstmac/libraries/mpi/mpi_status.h>

namespace sstmac {
namespace sw {

// Important types.


// class payload;

/**
 * A basic mpikernel to provide an MPI_Bcast.
 */
class mpi_bcast_engine : public mpi_collective
{

 public:
  std::string
  to_string() const {
    return sprockit::printf("mpibcast(tag=%d)", int(tag_));
  }

  /// Hi.
  mpi_bcast_engine(topology_iterator* iter,
                   mpi_request* thekey,
                   mpi_queue* queue, int count, mpi_type_id type,
                   mpi_id root, mpi_tag tag, mpi_comm* comm,
                   const payload::const_ptr& content,
                   event_handler* completion);

  /// Goodbye.
  virtual
  ~mpi_bcast_engine() throw ();

  /// Start this kernel.
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
  /// The iterator to whom we entrust decisions on where to send and receive.
  topology_iterator* iter_;
  /// The size of the data array.
  int count_;
  /// The type we are sending and receiving.
  mpi_type_id type_;
  /// The payload we are sending.
  payload::const_ptr content_;
  /// My rank.
  mpi_id rank_;

  /// The number of pending sends.
  int pending_sends_;
  /// The number of pending receives.
  int pending_recvs_;

  bool completing_;

  bool is_root_;

  /// Fire off the next set of send and receive operations.
  void
  sendrecv();

};

}
} // end of namespace sstmac

#endif

