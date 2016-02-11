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

#include <sstmac/libraries/mpi/mpi_implementation/rdma_mpi.h>
#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace sw {

SpktRegister("rdma", mpi_implementation, rdma_mpi_implementation,
            "An MPI that uses RDMA pathways and has zero-copy semantics for both intra- and inter-node");


mpi_protocol*
rdma_mpi_implementation::get_protocol(long bytes, bool intranode, bool ssend,
                                      bool onesided) const
{

  if (intranode) {
    if (onesided) {
      return mpi_protocol::rendezvous_mmap_protocol;
    }

    if (bytes > smp_single_copy_size_) {
      return mpi_protocol::rendezvous_mmap_protocol;
    }
    else {
      if (ssend) {
        return mpi_protocol::eager_ssend_protocol;
      }
      else {
        return mpi_protocol::eager0_mmap_protocol;
      }
    }
  }
  else {
    if (onesided) {
      return mpi_protocol::rendezvous_rdma_protocol;
    }

    if (bytes <= max_vshort_msg_size_) {
      return mpi_protocol::eager0_rdma_protocol;
    }
    else if (!ssend && bytes <= max_eager_msg_size_) {
      return mpi_protocol::eager1_rdma_singlecpy_protocol;
    }
    else {
      return mpi_protocol::rendezvous_rdma_protocol;
    }
  }
}

void
rdma_mpi_implementation::init_factory_params(sprockit::sim_parameters* params)
{
  mpi_implementation::init_factory_params(params);
  /**
      sstkeyword {
          docstring=The crossover point at which SMP/on-node sends switch from
          an eager, double-copy protocol to a rendezvous, single-copy protocol.;
      }
  */
  smp_single_copy_size_ = params->get_optional_byte_length_param(
                               "smp_single_copy_size", 8192);
  /**
      sstkeyword {
          docstring=The crossover point at which internode sends switch from
          an eager RDMA protocol to a rendezvous, zero-copy RDMA protocol.;
      }
  */
  max_eager_msg_size_ = params->get_optional_byte_length_param(
                             "max_eager_msg_size", 8192);
  /**
      sstkeyword {
          docstring=The crossover point at which internode sends switch from
          using low-latency, short message mailboxes to an eager RDMA protocol;
      }
  */
  max_vshort_msg_size_ = params->get_optional_byte_length_param(
                              "max_vshort_msg_size", 1024);
}

}
}

