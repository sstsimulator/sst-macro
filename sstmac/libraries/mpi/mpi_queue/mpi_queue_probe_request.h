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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_QUEUE_MPIQUEUE_PROBEREQUEST_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_QUEUE_MPIQUEUE_PROBEREQUEST_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_request_fwd.h>
#include <sstmac/libraries/mpi/mpi_message.h>

namespace sstmac {
namespace sw {

/**
 * Nested type to handle individual probe requests.
 */
class mpi_queue_probe_request  {

 public:
  virtual std::string
  to_string() const {
    return "mpi queue probe request";
  }

  /// Hi there.
  mpi_queue_probe_request(mpi_request* key, mpi_comm* comm,
                mpi_id source, mpi_tag tag,
                event_handler* completion);

  /// Test whether we match a given message.
  bool
  matches(const mpi_message::ptr& message) const;

  /// Consider this request complete.
  void
  complete(const mpi_message::ptr& message);

 protected:
  /// The parameters we will be matching on.
  mpi_comm_id myid_;
  mpi_id source_;
  mpi_tag tag_;
  event_handler* completion_;
  mpi_request* key_;

};

}
} // end of namespace sstmac.

#endif

