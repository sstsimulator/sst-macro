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

#include <sstmac/libraries/mpi/mpi_implementation/basic_mpi.h>
#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/common/sstmac_env.h>
namespace sstmac {
namespace sw {

SpktRegister("basic", mpi_implementation, basic_mpi_implementation,
            "A basic MPI [sockets] that has only two protocols: eager and rendezvous");


mpi_protocol*
basic_mpi_implementation::get_protocol(long bytes, bool intranode,
                                       bool ssend, bool onesided) const
{
  if (onesided) {
    return mpi_protocol::eager0_socket_protocol;
  }

  if (intranode) {
    if (ssend || bytes > handsize_) {
      return mpi_protocol::rendezvous_mmap_protocol;
    }
    else {
      return mpi_protocol::eager0_mmap_protocol;
    }
  }
  else {
    if (bytes <= handsize_) {
      if (ssend) {
        return mpi_protocol::eager_ssend_protocol;
      }
      else {
        return mpi_protocol::eager0_socket_protocol;
      }
    }
    else {
      return mpi_protocol::rendezvous_socket_protocol;
    }
  }
}

void
basic_mpi_implementation::init_factory_params(sprockit::sim_parameters* params)
{
  mpi_implementation::init_factory_params(params);
  handsize_ = params->get_optional_byte_length_param("handshake_size", 65536);
}


}
}

