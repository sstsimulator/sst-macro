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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_QUEUE_MPIQUEUE_RECVREQUEST_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_QUEUE_MPIQUEUE_RECVREQUEST_H_INCLUDED

#include <sumi-mpi/mpi_request_fwd.h>
#include <sumi-mpi/mpi_queue/mpi_queue_fwd.h>
#include <sumi-mpi/mpi_message.h>
#include <sstmac/common/event_location.h>

namespace sumi {


/**
 * A nested type to handle individual mpi receive requests.
 */
class mpi_queue_recv_request  {
  friend class mpi_queue;

 public:
  virtual std::string
  to_string() const {
    return "mpi queue recv request";
  }

  /// Hello.
  mpi_queue_recv_request(mpi_request* key, mpi_queue* queue, int count,
               MPI_Datatype type, int source, int tag,
               MPI_Comm comm);

  /// Goodbye.
  virtual ~mpi_queue_recv_request();

  /// Do we match the given message?
  bool matches(const mpi_message::ptr& msg);

  bool open_source() const;

  void
  set_seqnum(int seqnum) {
    seqnum_ = seqnum;
  }

  void
  handle(const mpi_message::ptr& mess);

  bool
  is_cancelled() const;

 private:
  void
  finish_message(void* buffer, const mpi_message::ptr& mess);

 private:
  /// The queue to whom we belong.
  mpi_queue* queue_;

  /// The parameters I will be matching on.
  int source_;
  int tag_;
  MPI_Comm comm_;
  int seqnum_;

  void* buffer_;

  static const int seqnum_unassigned = -1;

  /// The parameters I will not be matching on, but are good error checks.
  int count_;
  MPI_Datatype type_;
  mpi_request* key_;
};

}

#endif

