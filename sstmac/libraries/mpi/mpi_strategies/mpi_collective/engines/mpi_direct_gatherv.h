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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIDIRECTGATHERV_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIDIRECTGATHERV_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_tag.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_payload.h>

namespace sstmac {
namespace sw {

// Important types.
class mpi_queue;
class mpi_status;


/**
 * Perform an MPI gatherv using direct sends to the root node.
 *
 * This is the way mpich2 does gatherv.  The problem is that the
 * mpi standard specifies that the array of send lengths only needs
 * to be valid at root.  This means that that you either need to send
 * the length array separately (using a gather, bcast, or whatever)
 * or you just bite the bullet (like here) and send the data all
 * directly from the root node.
 */
class mpi_direct_gatherv : public mpi_collective
{

 public:
  std::string
  to_string() const {
    return sprockit::printf("mpidirectgatherv(tag=%d)", int(tag_));
  }

  /// Hi.
  mpi_direct_gatherv(mpi_request* thekey,
                     mpi_queue* queue,
                     int sendcount, mpi_type_id sendtype,
                     const std::vector<int> &recvcount, mpi_type_id recvtype,
                     mpi_id root, mpi_tag tag,
                     mpi_comm* comm,
                     const payload::const_ptr& content,
                     event_handler* completion);

  /// Goodbye.
  virtual ~mpi_direct_gatherv() throw();

  /// Start this kernel.
  virtual void start();

 protected:
  /// Callback method to indicate that a send operation has completed.
  virtual void
  send_complete(mpi_message* msg);

  /// Callback method to indicate that a receive operation has completed.
  virtual void
  recv_complete(mpi_message* msg);

 protected:
  /// The number of elements of type sendtype sent to root node.
  int sendcount_;
  /// The send type
  mpi_type_id sendtype_;
  /// The number of elements of type recvtype received from each node.
  std::vector<int> recvcount_;
  /// The receive type
  mpi_type_id recvtype_;
  /// The root node.
  mpi_id root_;

  /// Number of pending sends (only on clients).
  int pending_sends_;
  /// Number of pending receives (only on root).
  int pending_recvs_;

  mpi_collective_payload::ptr content_;
  payload::const_ptr send_content_;
};

}
} // end of namespace sstmac.

#endif

