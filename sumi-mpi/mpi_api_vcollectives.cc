#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/backtrace.h>

using namespace sumi;

namespace sstmac {
namespace sumi {

int
mpi_api::start_allgatherv(const void *sendbuf, void *recvbuf, int sendcount, const int *recvcounts, const int *displs, MPI_Datatype type, MPI_Comm comm)
{
  int typeSize = type_size(type);
  mpi_comm* commPtr = get_comm(comm);
  int tag = commPtr->next_collective_tag();
  spkt_throw(sprockit::unimplemented_error,
    "sumi::allgatherv");
  return tag;

}

int
mpi_api::allgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf, const int *recvcounts, const int *displs, MPI_Datatype recvtype, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Allgatherv");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Allgatherv(%d,%s,<...>,%s,%s)",
    sendcount, type_str(sendtype).c_str(),
    type_str(recvtype).c_str(),
    comm_str(comm).c_str());
  validate_mpi_collective("allgatherv", sendtype, recvtype);
  int tag = start_allgatherv(sendbuf, recvbuf, sendcount, recvcounts, displs, sendtype, comm);
  collective_progress_loop(collective::allgatherv, tag);
  return MPI_SUCCESS;
}

int
mpi_api::allgatherv(int sendcount, MPI_Datatype sendtype, const int *recvcounts, MPI_Datatype recvtype, MPI_Comm comm)
{
  return allgatherv(NULL, sendcount, sendtype, NULL, recvcounts, NULL, recvtype, comm);
}

int
mpi_api::start_alltoallv(const void *sendbuf,  void *recvbuf, const int *sendcounts, const int *sdispls, const int *recvcounts, const int *rdispls, MPI_Datatype type, MPI_Comm comm)
{
  int typeSize = type_size(type);
  mpi_comm* commPtr = get_comm(comm);
  int tag = commPtr->next_collective_tag();
  spkt_throw(sprockit::unimplemented_error,
    "sumi::alltoallv");
  return tag;
}

int
mpi_api::alltoallv(const void *sendbuf, const int *sendcounts, const int *sdispls, MPI_Datatype sendtype, void *recvbuf, const int *recvcounts, const int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Alltoallv");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Alltoallv(<...>,%s,<...>,%s,%s)",
    type_str(sendtype).c_str(), type_str(recvtype).c_str(), comm_str(comm).c_str());
  validate_mpi_collective("alltoallv", sendtype, recvtype);
  int tag = start_alltoallv(sendbuf, recvbuf, sendcounts, sdispls, recvcounts, rdispls, sendtype, comm);
  collective_progress_loop(collective::alltoallv, tag);
  return MPI_SUCCESS;
}

int
mpi_api::alltoallv(const int *sendcounts, MPI_Datatype sendtype, const int *recvcounts, MPI_Datatype recvtype, MPI_Comm comm)
{
  return alltoallv(NULL, sendcounts, NULL, sendtype, NULL, recvcounts, NULL, recvtype, comm);
}

int
mpi_api::start_gatherv(const void *sendbuf, void *recvbuf, int sendcount, const int *recvcounts, const int *displs, MPI_Datatype type, int root, MPI_Comm comm)
{
  int typeSize = type_size(type);
  mpi_comm* commPtr = get_comm(comm);
  int tag = commPtr->next_collective_tag();
  spkt_throw(sprockit::unimplemented_error,
    "sumi::gatherv");
  return tag;
}

int
mpi_api::gatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf, const int *recvcounts, const int *displs, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Scatterv");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Gatherv(%d,%s,<...>,%s,%d,%s)",
    sendcount, type_str(sendtype).c_str(),
    type_str(recvtype).c_str(),
    int(root), comm_str(comm).c_str());
  validate_mpi_collective("gatherv", sendtype, recvtype);
  int tag = start_gatherv(sendbuf, recvbuf, sendcount, recvcounts, displs, sendtype, root, comm);
  collective_progress_loop(collective::gatherv, tag);
  return MPI_SUCCESS;
}

int
mpi_api::gatherv(int sendcount, MPI_Datatype sendtype, const int *recvcounts, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  return gatherv(NULL, sendcount, sendtype, NULL, recvcounts, NULL, recvtype, root, comm);
}

int
mpi_api::start_scatterv(const void *sendbuf, void *recvbuf, const int* sendcounts, const int *displs, int recvcount, MPI_Datatype type, int root, MPI_Comm comm)
{
  int typeSize = type_size(type);
  mpi_comm* commPtr = get_comm(comm);
  int tag = commPtr->next_collective_tag();
  spkt_throw(sprockit::unimplemented_error,
    "sumi::scatterv");
  return tag;
}

int
mpi_api::scatterv(const void *sendbuf, const int *sendcounts, const int *displs, MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  validate_mpi_collective("alltoallv", sendtype, recvtype);
  int tag = start_scatterv(sendbuf, recvbuf, sendcounts, displs, recvcount, sendtype, root, comm);
  collective_progress_loop(collective::scatterv, tag);
  return MPI_SUCCESS;
}

int
mpi_api::scatterv(const int *sendcounts, MPI_Datatype sendtype, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Scatterv");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Scatterv(<...>,%s,%d,%s,%d,%s)",
    type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    int(root), comm_str(comm).c_str());
  return scatterv(NULL, sendcounts, NULL, sendtype, NULL, recvcount, recvtype, root, comm);
}


}
}
