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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_MPICOLLECTIVE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_MPICOLLECTIVE_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_comm/mpi_comm.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_tag.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/libraries/mpi/mpi_request.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_op.h>
#include <sstmac/libraries/mpi/mpi_debug.h>
#include <sstmac/common/event_handler.h>

#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_fwd.h>

namespace sstmac {
namespace sw {


/**
 * Base type for mpikernel objects that do collective operations.
 */
class mpi_collective  {
  /// Nested type to handle the actual send operations.
  class sender;
  /// Nested type to handle the actual receive operations.
  class recver;
  /// Nested type to handle funneled (one-at-a-time) copy operations.
  class funneledcopy;
  /// Nested type to handle concurrent (start-at-once) copy operations.
  class immediate_copy;

 public:
  virtual std::string
  to_string() const = 0;

  /// Goodbye.
  virtual
  ~mpi_collective() throw ();

  /// Complete.  Calls mpikernel::complete.
  void
  complete(const payload::const_ptr& content);

  /*
   * Callbacks for the derived type to request send or receive operations.
   */

  /// Called by the derived type to request a send.
  void
  start_send(int count, mpi_type_id type, mpi_tag tag,
             mpi_id dest, const payload::const_ptr& = payload::const_ptr());

  void
  start_send(int count, mpi_id dest);

  void
  start_send(int count, mpi_id dest, void* buffer);

  /// Called by the derived type to request a receive.
  void
  start_recv(
    int count, mpi_type_id type,
    mpi_tag tag, mpi_id src,
    void* buffer = 0);

  void
  start_recv(int count, mpi_id src);

  void
  start_recv(int count, mpi_id src, void* buffer);

  virtual bool
  done() const {
    return true;
  } 

  /*
   * Callbacks that the derived type needs to provide to complete
   * a send or receive operation.
   */

  /// Callback method to indicate that a send operation has completed.
  virtual void
  send_complete(const mpi_message::ptr& msg) = 0;

  /// Callback method to indicate that a receive operation has completed.
  virtual void
  recv_complete(const mpi_message::ptr& msg) = 0;

  void
  start();

  payload::const_ptr
  combine_content(const payload::const_ptr& p1,
                  const payload::const_ptr& p2,
                  mpi_op* op, int rankhere,
                  int rankincoming);

 protected:
  /// Hi.
  mpi_collective(mpi_request* when_complete,
                 mpi_queue* queue, mpi_tag tag,
                 mpi_comm* comm, event_handler* completion);

 protected:
  /// Our communicator.
  mpi_comm* comm_;

  mpi_tag tag_;

  event_handler* completion_;

  /// Our queue.
  mpi_queue* queue_;

  mpi_request* req_;

  /// Information on total bytes sent and received.
  mpi_status* status_;

  mpi_type_id send_type_;

  mpi_type_id recv_type_;
};

}
} // end of namespace sstmac

#define mpi_coll_debug(name, ...) \
    mpi_debug(comm_->rank(), sprockit::dbg::mpi_collective, \
       "%s : %s", name, sprockit::printf(__VA_ARGS__).c_str())

#endif

