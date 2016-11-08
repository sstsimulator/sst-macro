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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_QUEUE_MPIQUEUE_SENDREQUEST_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_QUEUE_MPIQUEUE_SENDREQUEST_H_INCLUDED

#include <sumi-mpi/mpi_message.h>
#include <sumi-mpi/mpi_queue/mpi_queue_fwd.h>
#include <sumi-mpi/mpi_request_fwd.h>

namespace sumi {


/**
 * A nested type to handle individual send operations.
 */
class mpi_queue_send_request  {
 public:

 protected:
  friend class mpi_queue;

 public:
  mpi_queue_send_request(const mpi_message::ptr& mess,
              mpi_request* key, mpi_queue* queue);

  /// Goodbye.
  ~mpi_queue_send_request() throw();

  bool matches(const mpi_message::ptr& send_ack) const;

  /// Eventhandler completion.
  void complete(const mpi_message::ptr& msg);

  void wait_for_buffer();

  mpi_request* req() const {
    return key_;
  }

 protected:
  /// The queue.
  mpi_queue* queue_;

  mpi_request* key_;

  int tag_;
  int seqnum_;
  int dest_;
  int src_;


};

}

#endif

