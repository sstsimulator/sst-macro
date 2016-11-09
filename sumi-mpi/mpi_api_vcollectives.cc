#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/software/process/operating_system.h>

#undef start
#define start(coll, name, ...) \
  start_##coll(name, __VA_ARGS__); \
  start_mpi_call(name)

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

collective_op_base*
mpi_api::start_allgatherv(const char* name,
    const void *sendbuf, int sendcount, MPI_Datatype sendtype,
    void *recvbuf, const int *recvcounts, const int *displs,
    MPI_Datatype recvtype, MPI_Comm comm)
{
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
  collective_op_base* op = start(allgatherv,
                              "MPI_Allgatherv", sendbuf, sendcount, sendtype,
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
  collective_op_base* op = start(allgatherv,
                             "MPI_Iallgatherv", sendbuf, sendcount, sendtype,
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

collective_op_base*
mpi_api::start_alltoallv(const char* name, const void *sendbuf, const int *sendcounts,
                   const int *sdispls, MPI_Datatype sendtype,
                   void *recvbuf, const int *recvcounts,
                   const int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "%s(<...>,%s,<...>,%s,%s)", name,
    type_str(sendtype).c_str(), type_str(recvtype).c_str(), comm_str(comm).c_str());

  if (sendbuf || recvbuf){
    collectivev_op* op = new collectivev_op(const_cast<int*>(sendcounts), const_cast<int*>(sdispls),
                                              const_cast<int*>(recvcounts), const_cast<int*>(rdispls),
                                              get_comm(comm));
    start_mpi_collective(collective::alltoallv, sendbuf, recvbuf, sendtype, recvtype, op);
    start_alltoallv(op);
    return op;
  } else {
    mpi_comm* commPtr = get_comm(comm);
    int send_count = 0;
    int recv_count = 0;
    int nproc = commPtr->size();
    for (int i=0; i < nproc; ++i){
      send_count += sendcounts[i];
      recv_count += recvcounts[i];
    }
    send_count /= nproc;
    recv_count /= nproc;
    collective_op_base* op = start_alltoall(name, sendbuf, send_count, sendtype,
                        recvbuf, recv_count, recvtype, comm);
    return op;
  }
}

int
mpi_api::alltoallv(const void *sendbuf, const int *sendcounts,
                   const int *sdispls, MPI_Datatype sendtype,
                   void *recvbuf, const int *recvcounts,
                   const int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
  collective_op_base* op = start(alltoallv,
                             "MPI_Alltoallv", sendbuf, sendcounts, sdispls, sendtype,
                             recvbuf, recvcounts, rdispls, recvtype, comm);
  wait_collective(op);
  delete op;
  return MPI_SUCCESS;
}

int
mpi_api::alltoallv(const int *sendcounts, MPI_Datatype sendtype,
                   const int *recvcounts, MPI_Datatype recvtype, MPI_Comm comm)
{
  return alltoallv(NULL, sendcounts, NULL, sendtype,
                   NULL, recvcounts, NULL, recvtype, comm);
}

int
mpi_api::ialltoallv(const void *sendbuf, const int *sendcounts,
                   const int *sdispls, MPI_Datatype sendtype,
                   void *recvbuf, const int *recvcounts,
                   const int *rdispls, MPI_Datatype recvtype,
                   MPI_Comm comm, MPI_Request* req)
{
  collective_op_base* op = start(alltoallv,
                             "MPI_Ialltoallv", sendbuf, sendcounts, sdispls, sendtype,
                             recvbuf, recvcounts, rdispls, recvtype, comm);

  add_immediate_collective(op, req);
  return MPI_SUCCESS;
}

int
mpi_api::ialltoallv(const int *sendcounts, MPI_Datatype sendtype,
                   const int *recvcounts, MPI_Datatype recvtype,
                   MPI_Comm comm, MPI_Request* req)
{
  return ialltoallv(NULL, sendcounts, NULL, sendtype,
                    NULL, recvcounts, NULL, recvtype,
                    comm, req);
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

collective_op_base*
mpi_api::start_gatherv(const char* name,
                 const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 void *recvbuf, const int *recvcounts, const int *displs,
                 MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  mpi_api_debug(sprockit::dbg::mpi,
    "%s(%d,%s,<...>,%s,%d,%s)", name,
    sendcount, type_str(sendtype).c_str(),
    type_str(recvtype).c_str(),
    int(root), comm_str(comm).c_str());

  if (sendbuf || recvbuf){
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
  } else {
    mpi_comm* commPtr = get_comm(comm);
    int recvcount = sendcount;
    if (commPtr->rank() == root){
      int total_count = 0;
      int nproc = commPtr->size();
      for (int i=0; i < nproc; ++i){
        total_count += recvcounts[i];
      }
      recvcount = total_count / nproc;
    }
    collective_op_base* op = start_gather(name,
                      sendbuf, sendcount, sendtype,
                      recvbuf, recvcount, recvtype, root, comm);
    return op;
  }
}

int
mpi_api::gatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 void *recvbuf, const int *recvcounts, const int *displs,
                 MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  collective_op_base* op = start(gatherv,
                        "MPI_Gatherv", sendbuf, sendcount, sendtype,
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
  return gatherv(NULL, sendtype, sendcount, NULL, recvcounts, NULL, recvtype, root, comm);
}

int
mpi_api::igatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 void *recvbuf, const int *recvcounts, const int *displs,
                 MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Request* req)
{
  collective_op_base* op = start(gatherv,
                         "MPI_Gatherv", sendbuf, sendcount, sendtype,
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
  return igatherv(NULL, sendcount, sendtype, NULL,
                  recvcounts, NULL, recvtype, root, comm, req);
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

collective_op_base*
mpi_api::start_scatterv(const char* name,
   const void* sendbuf, const int* sendcounts, const int *displs, MPI_Datatype sendtype,
   void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  mpi_api_debug(sprockit::dbg::mpi,
    "%s(<...>,%s,%d,%s,%d,%s)", name,
    type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    int(root), comm_str(comm).c_str());

  if (sendbuf || recvbuf){
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
  } else {
    mpi_comm* commPtr = get_comm(comm);
    int sendcount = recvcount;
    if (commPtr->rank() == root){
      int total_count = 0;
      int nproc = commPtr->size();
      for (int i=0; i < nproc; ++i){
        total_count += sendcounts[i];
      }
      sendcount = total_count / nproc;
    }
    collective_op_base* op = start_scatter(name, sendbuf, sendcount, sendtype,
                       recvbuf, recvcount, recvtype, root, comm);
    return op;
  }
}

int
mpi_api::scatterv(const void* sendbuf, const int* sendcounts, const int *displs, MPI_Datatype sendtype,
                  void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  collective_op_base* op = start(scatterv,
                               "MPI_Scatterv", sendbuf, sendcounts, displs, sendtype,
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
  return scatterv(NULL, sendcounts, NULL, sendtype, NULL,
                  recvcount, recvtype, root, comm);
}


int
mpi_api::iscatterv(const void* sendbuf, const int* sendcounts, const int *displs, MPI_Datatype sendtype,
                  void *recvbuf, int recvcount, MPI_Datatype recvtype,
                  int root, MPI_Comm comm, MPI_Request* req)
{
  collective_op_base* op = start(scatterv,
                          "MPI_Iscatterv", sendbuf, sendcounts, displs, sendtype,
                          recvbuf, recvcount, recvtype, root, comm);

  add_immediate_collective(op, req);
  return MPI_SUCCESS;
}

int
mpi_api::iscatterv(const int *sendcounts, MPI_Datatype sendtype,
                  int recvcount, MPI_Datatype recvtype,
                  int root, MPI_Comm comm, MPI_Request* req)
{
  return iscatterv(NULL, sendcounts, NULL, sendtype,
                   NULL, recvcount, recvtype, root, comm, req);
}

}
