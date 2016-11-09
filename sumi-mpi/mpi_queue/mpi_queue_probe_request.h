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

#include <sumi-mpi/mpi_request_fwd.h>
#include <sumi-mpi/mpi_message.h>

namespace sumi {


/**
 * Nested type to handle individual probe requests.
 */
class mpi_queue_probe_request  {

 public:
  /// Hi there.
  mpi_queue_probe_request(mpi_request* key, MPI_Comm comm,
                int source, int tag);

  /// Test whether we match a given message.
  bool
  matches(const mpi_message::ptr& message) const;

  /// Consider this request complete.
  void
  complete(const mpi_message::ptr& message);

 protected:
  /// The parameters we will be matching on.
  MPI_Comm myid_;
  int source_;
  int tag_;
  mpi_request* key_;

};

}

#endif

