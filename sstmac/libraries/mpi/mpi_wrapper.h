
#ifndef sstmac_MPI_H
#define sstmac_MPI_H

#include <sstmac/libraries/mpi/sstmac_mpi.h>
#include <sstmac/util.h>

#ifdef __cplusplus
#include <sstmac/libraries/mpi/mpi_app.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/software/libraries/compute/sstmac_compute.h>

/** Automatically inherit mpi tpes */
using sstmac::sw::mpi_type;
using sstmac::sw::mpi_id;
using sstmac::sw::mpi_op;
using sstmac::sw::mpi_tag;
using sstmac::sw::mpi_comm;
using sstmac::sw::mpi_request;
using sstmac::sw::mpi_status;
using sstmac::sw::mpi_status;
using sstmac::sw::mpi_api;
using sstmac::sw::mpi_app;
#endif

#endif // MPI_H

