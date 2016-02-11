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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_RMA_MPIQUEUE_GETREQUEST_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_RMA_MPIQUEUE_GETREQUEST_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>

namespace sstmac {
namespace sw {

/**
 * A nested type to handle individual mpi receive requests.
 */
class mpi_queue_get_request {

 public:

 public:
  virtual std::string
  to_string() const {
    return "mpi queue get request";
  }

  /// Hello.
  static mpi_queue_get_request*
  construct(mpi_request* key, mpi_queue* queue,
            event_handler* completion) {
    return new mpi_queue_get_request(key, queue, completion);
  }

  /// Goodbye.
  virtual
  ~mpi_queue_get_request() {
  }

  /// We be done.
  void
  handle(const mpi_message::ptr& mess);

  bool
  is_cancelled() const {
    return key_->is_cancelled();
  }

 protected:
  /// Hello.
  mpi_queue_get_request(mpi_request* key, mpi_queue* queue,
              event_handler* completion);

 private:
  friend class mpi_queue;

  /// The queue to whom we belong.
  mpi_queue* queue_;

  mpi_request* key_;

  /// The object that will be notified upon completion.
  event_handler* completion_;

};

}
} // end of namespace sstmac

#endif

