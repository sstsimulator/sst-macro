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

#include <sstmac/libraries/mpi/mpi_request_fwd.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_fwd.h>
#include <sstmac/libraries/mpi/mpi_message.h>
#include <sstmac/common/event_location.h>

namespace sstmac {
namespace sw {

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
               mpi_type_id type, mpi_id source, mpi_tag tag,
               mpi_comm* comm, mpi_message::category cat,
               event_handler* completion);

  /// Goodbye.
  virtual ~mpi_queue_recv_request();

  /// Do we match the given message?
  bool matches(mpi_message* msg);

  bool open_source() const;

  void
  set_category(mpi_message::category cat) {
    cat_ = cat;
  }

  void
  set_seqnum(int seqnum) {
    seqnum_ = seqnum;
  }

  void
  handle(mpi_message* mess);

  bool
  is_cancelled() const;

 private:
  /// The queue to whom we belong.
  mpi_queue* queue_;

  /// The parameters I will be matching on.
  mpi_id source_;
  mpi_tag tag_;
  mpi_comm_id comm_;
  mpi_message::category cat_;
  int seqnum_;

  void* buffer_;

  static const int seqnum_unassigned = -1;

  /// The parameters I will not be matching on, but are good error checks.
  int count_;
  mpi_type_id type_;
  mpi_request* key_;

  /// The object that will be notified upon completion.
  event_handler* completion_;
};

}
} // end of namespace sstmac

#endif

