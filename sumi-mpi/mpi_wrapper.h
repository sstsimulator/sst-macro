
#ifndef sstmac_mpi_wrapper_H
#define sstmac_mpi_wrapper_H

#include <sumi-mpi/sstmac_mpi.h>

#ifdef __cplusplus
#include <sumi-mpi/mpi_api.h>

#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
inline sumi::mpi_api*
current_mpi() {
  return sstmac::sw::operating_system::current_thread()->get_api<sumi::mpi_api>();
}
#endif

#endif // MPI_H

