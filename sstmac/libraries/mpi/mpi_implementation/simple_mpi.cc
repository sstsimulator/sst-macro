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

#include <sstmac/libraries/mpi/mpi_implementation/simple_mpi.h>
#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace sw {

SpktRegister("simple", mpi_implementation, simple_mpi_implementation,
            "The simplest possible MPI that does everything as a buffered, eager send");


mpi_protocol*
simple_mpi_implementation::get_protocol(
  long bytes,
  bool intranode,
  bool ssend, bool onesided) const
{
  if(onesided) {
    return mpi_protocol::eager0_socket_protocol;
  }

  if (intranode) {
    return mpi_protocol::rendezvous_mmap_protocol;
  }
  else {
    if (ssend) {
      return mpi_protocol::eager_ssend_protocol;
    }
    else {
      return mpi_protocol::eager0_socket_protocol;
    }

  }
}


}
}

