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

#include <sstmac/skeletons/test/mpi_protocol_test.h>
#include <sstmac/replacements/mpi.h>
#include <sstmac/software/libraries/compute/lib_sleep.h>
#include <sstmac/util.h>
#include <sprockit/util.h>
#include <sprockit/errors.h>

#include <cstring>
#include <stdio.h>

namespace sstmac {
namespace sw {

SpktRegister("mpiprotocol | MPI_protocol | mpi_protocol", app,
            mpi_protocol_test);

//
// Goodbye.
//
mpi_protocol_test::~mpi_protocol_test() throw ()
{
}

void
mpi_protocol_test::consume_params(sprockit::sim_parameters* params)
{
  print_all_ = params->get_optional_bool_param("mpi_test_print_all", true);
}

//
// Go.
//
void
mpi_protocol_test::skeleton_main()
{
  if (mpi() == 0) {
    spkt_throw(sprockit::null_error, "mpiprotocol::run: mpiapi pointer is null");
  }
  if (print_all_) std::cout << "mpiprotocol[" << sid().to_string()
               << "]: mpi initializing... \n";

  MPI_Init(NULL, NULL);

  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  mpi_implementation* mpi_impl = mpi()->queue()->implementation();
  rdma_mpi_implementation* rdma_impl = safe_cast(rdma_mpi_implementation, mpi_impl,
                     "mpiprotocol test could not find rdma implementation");
  int eager_count = rdma_impl->max_eager_msg_size();
  int rendezvous_count = 2 * rdma_impl->max_eager_msg_size();

  if (print_all_) std::cout << "mpiprotocol[" << rank
               << "]: mpi initialized\n";

  if (size != 2) {
    spkt_throw(sprockit::illformed_error, "mpiprotocol should only be run with 2 processes");
  }

  //make sure the recv is posted first
  if (rank == 0) {
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Send(MPI_PAYLOAD_IGNORE, eager_count, MPI_BYTE, 1, 100, MPI_COMM_WORLD);
  }
  else {
    MPI_Request req;
    MPI_Irecv(MPI_PAYLOAD_IGNORE, eager_count, MPI_BYTE, 0, 100, MPI_COMM_WORLD,
              &req);
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Wait(&req, MPI_STATUS_IGNORE);
  }

  if (print_all_) std::cout << "mpiprotocol[" << rank
               << "]: finished first test\n";

  if (rank == 0) {
    //post the send first
    MPI_Send(MPI_PAYLOAD_IGNORE, eager_count, MPI_BYTE, 1, 100, MPI_COMM_WORLD);
  }
  else {
    sleep(timestamp(0.005));
    MPI_Recv(MPI_PAYLOAD_IGNORE, eager_count, MPI_BYTE, 0, 100, MPI_COMM_WORLD,
             MPI_STATUS_IGNORE);
  }

  if (print_all_) std::cout << "mpiprotocol[" << rank
               << "]: finished second test\n";

  if (rank == 0) {
    //post the send first
    MPI_Send(MPI_PAYLOAD_IGNORE, rendezvous_count, MPI_BYTE, 1, 100,
             MPI_COMM_WORLD);
  }
  else {
    MPI_Recv(MPI_PAYLOAD_IGNORE, rendezvous_count, MPI_BYTE, 0, 100, MPI_COMM_WORLD,
             MPI_STATUS_IGNORE);
  }

  if (print_all_) std::cout << "mpiprotocol[" << rank
               << "]: finished third test\n";

  //now send a really big message
  if (rank == 0) {

  }

  MPI_Finalize();

  if (print_all_) std::cout << "mpiprotocol[" << rank
               << "]: finalized\n";

}

}
} // end of namespace sstmac.


