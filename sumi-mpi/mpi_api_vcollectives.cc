#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/backtrace.h>

using namespace sumi;

namespace sumi {

void
mpi_api::finish_vcollective_op(collective_op_base* op_)
{
  collectivev_op* op = static_cast<collectivev_op*>(op_);
  if (op->packed_recv){
    spkt_throw_printf(sprockit::unimplemented_error,
               "cannot handle non-contiguous types in collective %s",
               collective::tostr(op->ty));
  }
  if (op->packed_send){
    spkt_throw_printf(sprockit::unimplemented_error,
               "cannot handle non-contiguous types in collective %s",
               collective::tostr(op->ty));
  }
}

void
mpi_api::start_allgatherv(collectivev_op* op)
{
  transport::allgatherv(op->tmp_recvbuf, op->tmp_sendbuf,
                  op->recvcounts, op->sendtype->packed_size(), op->tag,
                  false, options::initial_context, op->comm);

}

collectivev_op*
mpi_api::start_allgatherv(const char* name,
    const void *sendbuf, int sendcount, MPI_Datatype sendtype,
    void *recvbuf, const int *recvcounts, const int *displs,
    MPI_Datatype recvtype, MPI_Comm comm)
{
  SSTMACBacktrace(name);
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "%s(%d,%s,<...>,%s,%s)", name,
    sendcount, type_str(sendtype).c_str(),
    type_str(recvtype).c_str(),
    comm_str(comm).c_str());

  collectivev_op* op = new collectivev_op(sendcount, const_cast<int*>(recvcounts),
                                          const_cast<int*>(displs), get_comm(comm));
  start_mpi_collective(collective::allgatherv, sendbuf, recvbuf, sendtype, recvtype, op);
  start_allgatherv(op);
  return op;
}

int
mpi_api::allgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                    void *recvbuf, const int *recvcounts, const int *displs,
                    MPI_Datatype recvtype, MPI_Comm comm)
{
  collectivev_op* op = start_allgatherv("MPI_Allgatherv", sendbuf, sendcount, sendtype,
                                       recvbuf, recvcounts, displs, recvtype, comm);
  wait_collective(op);
  delete op;
  return MPI_SUCCESS;
}

int
mpi_api::allgatherv(int sendcount, MPI_Datatype sendtype,
                    const int *recvcounts, MPI_Datatype recvtype, MPI_Comm comm)
{
  return allgatherv(NULL, sendcount, sendtype, NULL, recvcounts, NULL, recvtype, comm);
}

int
mpi_api::iallgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                    void *recvbuf, const int *recvcounts, const int *displs,
                    MPI_Datatype recvtype, MPI_Comm comm, MPI_Request* req)
{
  collectivev_op* op = start_allgatherv("MPI_Iallgatherv", sendbuf, sendcount, sendtype,
                                       recvbuf, recvcounts, displs, recvtype, comm);
  add_immediate_collective(op, req);
  return MPI_SUCCESS;
}

int
mpi_api::iallgatherv(int sendcount, MPI_Datatype sendtype,
                     const int *recvcounts, MPI_Datatype recvtype,
                     MPI_Comm comm, MPI_Request* req)
{
  return iallgatherv(NULL, sendcount, sendtype,
                    NULL, recvcounts, NULL,
                    recvtype, comm, req);
}

void
mpi_api::start_alltoallv(collectivev_op* op)
{
  transport::alltoallv(op->tmp_recvbuf, op->tmp_sendbuf,
                  op->sendcounts, op->recvcounts,
                  op->sendtype->packed_size(), op->tag,
                  false, options::initial_context, op->comm);
}

collectivev_op*
mpi_api::start_alltoallv(const char* name, const void *sendbuf, const int *sendcounts,
                   const int *sdispls, MPI_Datatype sendtype,
                   void *recvbuf, const int *recvcounts,
                   const int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
  SSTMACBacktrace(name);
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "%s(<...>,%s,<...>,%s,%s)", name,
    type_str(sendtype).c_str(), type_str(recvtype).c_str(), comm_str(comm).c_str());

  collectivev_op* op = new collectivev_op(const_cast<int*>(sendcounts), const_cast<int*>(sdispls),
                                          const_cast<int*>(recvcounts), const_cast<int*>(rdispls),
                                          get_comm(comm));
  start_mpi_collective(collective::alltoallv, sendbuf, recvbuf, sendtype, recvtype, op);
  start_alltoallv(op);
  return op;
}

int
mpi_api::alltoallv(const void *sendbuf, const int *sendcounts,
                   const int *sdispls, MPI_Datatype sendtype,
                   void *recvbuf, const int *recvcounts,
                   const int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
  collectivev_op* op = start_alltoallv("MPI_Alltoallv", sendbuf, sendcounts, sdispls, sendtype,
                                recvbuf, recvcounts, rdispls, recvtype, comm);
  wait_collective(op);
  delete op;
  return MPI_SUCCESS;
}

int
mpi_api::alltoallv(const int *sendcounts, MPI_Datatype sendtype,
                   const int *recvcounts, MPI_Datatype recvtype, MPI_Comm comm)
{
  //might not actually need an alltoallv - this is stupid and annoying
  //if I don't need the buffers, don't use them
  mpi_comm* commPtr = get_comm(comm);
  int total_count = 0;
  int nproc = commPtr->size();
  for (int i=0; i < nproc; ++i){
    total_count += sendcounts[i];
  }

  int avg_count = total_count / nproc;
  //probably latency-bound anyway
  return alltoall(NULL, avg_count, sendtype, NULL, avg_count, sendtype, comm);
}

int
mpi_api::ialltoallv(const void *sendbuf, const int *sendcounts,
                   const int *sdispls, MPI_Datatype sendtype,
                   void *recvbuf, const int *recvcounts,
                   const int *rdispls, MPI_Datatype recvtype,
                   MPI_Comm comm, MPI_Request* req)
{
  collectivev_op* op = start_alltoallv("MPI_Ialltoallv", sendbuf, sendcounts, sdispls, sendtype,
                                recvbuf, recvcounts, rdispls, recvtype, comm);

  add_immediate_collective(op, req);
  return MPI_SUCCESS;
}

int
mpi_api::ialltoallv(const int *sendcounts, MPI_Datatype sendtype,
                   const int *recvcounts, MPI_Datatype recvtype,
                   MPI_Comm comm, MPI_Request* req)
{
  //might not actually need an alltoallv - this is stupid and annoying
  //if I don't need the buffers, don't use them
  mpi_comm* commPtr = get_comm(comm);
  int total_count = 0;
  int nproc = commPtr->size();
  for (int i=0; i < nproc; ++i){
    total_count += sendcounts[i];
  }

  int avg_count = total_count / nproc;
  //probably latency-bound anyway
  return ialltoall(NULL, avg_count, sendtype, NULL,
                   avg_count, sendtype, comm, req);
}

void
mpi_api::start_gatherv(collectivev_op* op)
{
  spkt_throw(sprockit::unimplemented_error,
    "sumi::gatherv");
  //transport::gatherv(op->tmp_recvbuf, op->tmp_sendbuf,
  //                     op->sendcnt, typeSize, op->tag,
  //                false, options::initial_context, op->comm);
}

collectivev_op*
mpi_api::start_gatherv(const char* name,
                 const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 void *recvbuf, const int *recvcounts, const int *displs,
                 MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  SSTMACBacktrace(name);
  mpi_api_debug(sprockit::dbg::mpi,
    "%s(%d,%s,<...>,%s,%d,%s)", name,
    sendcount, type_str(sendtype).c_str(),
    type_str(recvtype).c_str(),
    int(root), comm_str(comm).c_str());

  collectivev_op* op = new collectivev_op(sendcount,
                                          const_cast<int*>(recvcounts),
                                          const_cast<int*>(displs), get_comm(comm));
  op->root = root;
  if (root == op->comm->rank()){
    //pass
  } else {
    recvtype = MPI_DATATYPE_NULL;
    recvbuf = 0;
  }

  start_mpi_collective(collective::gatherv, sendbuf, recvbuf, sendtype, recvtype, op);
  start_gatherv(op);

  return op;
}

int
mpi_api::gatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 void *recvbuf, const int *recvcounts, const int *displs,
                 MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  collectivev_op* op = start_gatherv("MPI_Gatherv", sendbuf, sendcount, sendtype,
                                    recvbuf, recvcounts, displs, recvtype,
                                    root, comm);
  wait_collective(op);
  delete op;
  return MPI_SUCCESS;
}

int
mpi_api::gatherv(int sendcount, MPI_Datatype sendtype,
                 const int *recvcounts, MPI_Datatype recvtype,
                 int root, MPI_Comm comm)
{
  //if I don't need the buffers, don't use them
  mpi_comm* commPtr = get_comm(comm);
  if (commPtr->rank() == root){
    int total_count = 0;
    int nproc = commPtr->size();
    for (int i=0; i < nproc; ++i){
      total_count += recvcounts[i];
    }
    int avg_count = total_count / nproc;
    //probably latency-bound anyway
    return gather(sendcount, sendtype, avg_count, recvtype, root, comm);
  } else {
    return gather(sendcount, sendtype, -1, MPI_DATATYPE_NULL, root, comm);
  }
}

int
mpi_api::igatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 void *recvbuf, const int *recvcounts, const int *displs,
                 MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Request* req)
{
  collectivev_op* op = start_gatherv("MPI_Gatherv", sendbuf, sendcount, sendtype,
                                    recvbuf, recvcounts, displs, recvtype,
                                    root, comm);
  add_immediate_collective(op, req);
  return MPI_SUCCESS;
}

int
mpi_api::igatherv(int sendcount, MPI_Datatype sendtype,
                 const int *recvcounts, MPI_Datatype recvtype,
                 int root, MPI_Comm comm, MPI_Request* req)
{
  //if I don't need the buffers, don't use them
  mpi_comm* commPtr = get_comm(comm);
  if (commPtr->rank() == root){
    int total_count = 0;
    int nproc = commPtr->size();
    for (int i=0; i < nproc; ++i){
      total_count += recvcounts[i];
    }
    int avg_count = total_count / nproc;
    //probably latency-bound anyway
    return igather(sendcount, sendtype, avg_count, recvtype, root, comm, req);
  } else {
    return igather(sendcount, sendtype, -1, MPI_DATATYPE_NULL, root, comm, req);
  }
}

void
mpi_api::start_scatterv(collectivev_op* op)
{
  spkt_throw(sprockit::unimplemented_error,
    "sumi::scatterv");
  //transport::allgatherv(op->tmp_recvbuf, op->tmp_sendbuf,
  //                     op->sendcnt, typeSize, op->tag,
  //                false, options::initial_context, op->comm);
}

collectivev_op*
mpi_api::start_scatterv(const char* name,
   const void* sendbuf, const int* sendcounts, const int *displs, MPI_Datatype sendtype,
   void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  SSTMACBacktrace(name);
  mpi_api_debug(sprockit::dbg::mpi,
    "%s(<...>,%s,%d,%s,%d,%s)", name,
    type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    int(root), comm_str(comm).c_str());

  collectivev_op* op = new collectivev_op(const_cast<int*>(sendcounts),
                                          const_cast<int*>(displs),
                                          recvcount, get_comm(comm));

  op->root = root;
  if (root == op->comm->rank()){
    //pass
  } else {
    sendtype = MPI_DATATYPE_NULL;
    sendbuf = 0;
  }

  start_mpi_collective(collective::scatterv, sendbuf, recvbuf, sendtype, recvtype, op);
  start_scatterv(op);

  return op;
}

int
mpi_api::scatterv(const void* sendbuf, const int* sendcounts, const int *displs, MPI_Datatype sendtype,
                  void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  collectivev_op* op = start_scatterv("MPI_Scatterv", sendbuf, sendcounts, displs, sendtype,
                                     recvbuf, recvcount, recvtype, root, comm);
  wait_collective(op);
  delete op;
  return MPI_SUCCESS;
}

int
mpi_api::scatterv(const int *sendcounts, MPI_Datatype sendtype,
                  int recvcount, MPI_Datatype recvtype,
                  int root, MPI_Comm comm)
{
  //if I don't need the buffers, don't use them
  mpi_comm* commPtr = get_comm(comm);
  if (commPtr->rank() == root){
    int total_count = 0;
    int nproc = commPtr->size();
    for (int i=0; i < nproc; ++i){
      total_count += sendcounts[i];
    }
    int avg_count = total_count / nproc;
    //probably latency-bound anyway
    return scatter(avg_count, sendtype, recvcount, recvtype, root, comm);
  } else {
    return scatter(NULL, -1, MPI_DATATYPE_NULL, NULL, recvcount, recvtype, root, comm);
  }
}


int
mpi_api::iscatterv(const void* sendbuf, const int* sendcounts, const int *displs, MPI_Datatype sendtype,
                  void *recvbuf, int recvcount, MPI_Datatype recvtype,
                  int root, MPI_Comm comm, MPI_Request* req)
{
  collectivev_op* op = start_scatterv("MPI_Iscatterv", sendbuf, sendcounts, displs, sendtype,
                                     recvbuf, recvcount, recvtype, root, comm);

  add_immediate_collective(op, req);
  return MPI_SUCCESS;
}

int
mpi_api::iscatterv(const int *sendcounts, MPI_Datatype sendtype,
                  int recvcount, MPI_Datatype recvtype,
                  int root, MPI_Comm comm, MPI_Request* req)
{
  //if I don't need the buffers, don't use them
  mpi_comm* commPtr = get_comm(comm);
  if (commPtr->rank() == root){
    int total_count = 0;
    int nproc = commPtr->size();
    for (int i=0; i < nproc; ++i){
      total_count += sendcounts[i];
    }
    int avg_count = total_count / nproc;
    //probably latency-bound anyway
    return iscatter(avg_count, sendtype, recvcount, recvtype,
                    root, comm, req);
  } else {
    return iscatter(NULL, -1, MPI_DATATYPE_NULL, NULL,
                   recvcount, recvtype, root, comm, req);
  }
}

}
