
#ifndef sstmac_mpi_wrapper_H
#define sstmac_mpi_wrapper_H

#include <sumi-mpi/sstmac_mpi.h>

#ifdef __cplusplus
#include <sumi-mpi/mpi_api.h>

#include <sstmac/software/process/operating_system.h>
inline sstmac::sumi::mpi_api*
current_mpi() {
  return sstmac::sw::operating_system::current_thread()->get_api<sstmac::sumi::mpi_api>();
}
#endif

#endif // MPI_H

