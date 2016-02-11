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

#include <sstmac/skeletons/test/mpi_congestion.h>
#include <sstmac/libraries/mpi/sstmac_mpi.h>
#include <sstmac/util.h>
#include <sprockit/errors.h>

#include <cstring>
#include <stdio.h>

namespace sstmac {
namespace sw {

SpktRegister("MPI_congestion | mpi_congestion", app, mpi_congestion);

//
// Goodbye.
//
mpi_congestion::~mpi_congestion() throw ()
{
}

void
mpi_congestion::consume_params(sprockit::sim_parameters* params)
{
  iterations_ = params->get_int_param("mpicongestion_iterations");
  window_ = params->get_int_param("mpicongestion_window");
  count_ = params->get_int_param("mpicongestion_count");
}

//
// Go.
//
void
mpi_congestion::skeleton_main()
{
  if (mpi() == 0) {
    throw sprockit::null_error("mpicongestion::run:  mpiapi pointer is null.");
  }

  MPI_Init(NULL, NULL);

  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);


  int tag = 0;
  if (rank == 0) { //recver
    int nreqs = window_ * (size - 1);
    MPI_Request* req = new MPI_Request[nreqs];
    for (int i=0; i < iterations_; ++i) {
      MPI_Request* reqptr = req;
      for (int w=0; w < window_; ++w) {
        for (int src=1; src < size; ++src, ++reqptr) {
          MPI_Irecv(MPI_PAYLOAD_IGNORE, count_, MPI_INT, src, tag, MPI_COMM_WORLD,
                    reqptr);
        }
      }
      MPI_Waitall(nreqs, req, MPI_STATUSES_IGNORE);
    }
  }
  else {
    int nreqs = window_;
    MPI_Request* req = new MPI_Request[nreqs];
    int dst = 0;
    for (int i=0; i < iterations_; ++i) {
      MPI_Request* reqptr = req;
      for (int w=0; w < window_; ++w, ++reqptr) {
        MPI_Isend(MPI_PAYLOAD_IGNORE, count_, MPI_INT, dst, tag, MPI_COMM_WORLD,
                  reqptr);
      }
      MPI_Waitall(nreqs, req, MPI_STATUSES_IGNORE);
    }
  }
  MPI_Finalize();
}

}
} // end of namespace sstmac.

