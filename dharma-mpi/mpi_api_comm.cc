#include <dharma-mpi/mpi_api.h>
#include <sstmac/software/process/backtrace.h>

namespace sstmac {
namespace sumi {

int
mpi_api::comm_dup(MPI_Comm input, MPI_Comm *output)
{
  SSTMACBacktrace("MPI_Comm_dup");
  mpi_comm* inputPtr = get_comm(input);
  mpi_comm* outputPtr = comm_factory_->comm_dup(inputPtr);
  *output = add_comm_ptr(outputPtr);
  return MPI_SUCCESS;
}

int
mpi_api::comm_create(MPI_Comm input, MPI_Group group, MPI_Comm *output)
{
  SSTMACBacktrace("MPI_Comm_create");
  mpi_comm* inputPtr = get_comm(input);
  mpi_group* groupPtr = get_group(group);
  *output = add_comm_ptr(comm_factory_->comm_create(inputPtr, groupPtr));
  return MPI_SUCCESS;
}

int
mpi_api::comm_split(MPI_Comm incomm, int color, int key, MPI_Comm *outcomm)
{
  SSTMACBacktrace("MPI_Comm_split");
  mpi_comm* incommPtr = get_comm(incomm);
  mpi_comm* outcommPtr = comm_factory_->comm_split(incommPtr, color, key);
  *outcomm = add_comm_ptr(outcommPtr);
  return MPI_SUCCESS;
}

int
mpi_api::comm_free(MPI_Comm input)
{
  SSTMACBacktrace("MPI_Comm_free");
  mpi_comm* inputPtr = get_comm(input);
  delete inputPtr;
  return MPI_SUCCESS;
}

}
}
