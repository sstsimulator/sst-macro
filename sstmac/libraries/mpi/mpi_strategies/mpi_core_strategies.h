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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPICORESTRATEGIES_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPICORESTRATEGIES_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_strategies.h>

#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_send.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_rsend.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_ssend.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_recv.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_barrier.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_scan.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_bcast.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_scatter.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_gather.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_scatterv.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_gatherv.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_alltoall.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_alltoallv.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_reduce_scatter.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_allgather.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_allgatherv.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_reduce.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_allreduce.h>

#endif

