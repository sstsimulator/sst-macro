#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/backtrace.h>

namespace  sumi {


int
mpi_api::send(const void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Send");
  mpi_comm* commPtr = get_comm(comm);
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt,
    "MPI_Send(%d,%s,%d:%d,%s,%s)",
    count, type_str(datatype).c_str(), int(dest), int(commPtr->peer_task(dest)),
    tag_str(tag).c_str(), comm_str(comm).c_str());
  mpi_request* req = mpi_request::construct(default_key_category);
  queue_->send(req, count, datatype, dest, tag, commPtr, const_cast<void*>(buf));
  queue_->progress_loop(req);
  delete req;
  return MPI_SUCCESS;
}

int
mpi_api::sendrecv(const void *sendbuf, int sendcount,
 MPI_Datatype sendtype, int dest, int sendtag,
 void *recvbuf, int recvcount,
 MPI_Datatype recvtype, int source, int recvtag,
 MPI_Comm comm, MPI_Status *status)
{
  MPI_Request recvreq, sendreq;
  isend(sendbuf, sendcount, sendtype, dest, sendtag, comm, &sendreq);
  irecv(recvbuf, recvcount, recvtype, source, recvtag, comm, &recvreq);
  wait(&sendreq, MPI_STATUS_IGNORE);
  wait(&recvreq, status);
  return MPI_SUCCESS;
}

int
mpi_api::start(MPI_Request* req)
{
  mpi_request* reqPtr = get_request(*req);
  persistent_op* op = reqPtr->op();
  if (op == 0){
    spkt_throw_printf(sprockit::value_error,
                      "Starting MPI_Request that is not persistent");
  }

  mpi_comm* commPtr = get_comm(op->comm);
  if (op->optype == persistent_op::Send){
    queue_->send(reqPtr, op->count, op->datatype, op->partner, op->tag, commPtr, op->content);
  } else {
    queue_->recv(reqPtr, op->count, op->datatype, op->partner, op->tag, commPtr, op->content);
  }

  return MPI_SUCCESS;
}

int
mpi_api::send_init(const void *buf, int count,
                   MPI_Datatype datatype, int dest, int tag,
                   MPI_Comm comm, MPI_Request *request)
{
  mpi_request* req = mpi_request::construct(default_key_category);
  *request = add_request_ptr(req);

  persistent_op* op = new persistent_op;
  op->content = const_cast<void*>(buf);
  op->count = count;
  op->datatype = datatype;
  op->partner = dest;
  op->tag = tag;
  op->comm = comm;
  op->optype = persistent_op::Send;

  req->persist(op);

  return MPI_SUCCESS;
}

int
mpi_api::isend(const void *buf, int count, MPI_Datatype datatype, int dest,
               int tag, MPI_Comm comm, MPI_Request *request)
{
  SSTMACBacktrace("MPI_Isend");
  mpi_comm* commPtr = get_comm(comm);
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request | sprockit::dbg::mpi_pt2pt,
    "MPI_Isend(%d,%s,%d:%d,%s,%s)",
    count, type_str(datatype).c_str(), int(dest), int(commPtr->peer_task(dest)),
    tag_str(tag).c_str(), comm_str(comm).c_str());
  mpi_request* req = mpi_request::construct(default_key_category);
  queue_->send(req, count, datatype, dest, tag, commPtr, const_cast<void*>(buf));
  *request = add_request_ptr(req);
  return MPI_SUCCESS;
}

int
mpi_api::recv(void *buf, int count, MPI_Datatype datatype, int source,
              int tag, MPI_Comm comm, MPI_Status *status)
{
  SSTMACBacktrace("MPI_Recv");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt,
    "MPI_Recv(%d,%s,%s,%s,%s)",
    count, type_str(datatype).c_str(), src_str(source).c_str(), tag_str(tag).c_str(), comm_str(comm).c_str());

  mpi_request* req = mpi_request::construct(default_key_category);
  mpi_comm* commPtr = get_comm(comm);
  queue_->recv(req, count, datatype, source, tag, commPtr, buf);

  queue_->progress_loop(req);
  delete req;

  return MPI_SUCCESS;
}

int
mpi_api::recv_init(void *buf, int count, MPI_Datatype datatype, int source,
                   int tag, MPI_Comm comm, MPI_Request *request)
{
  mpi_request* req = mpi_request::construct(default_key_category);
  *request = add_request_ptr(req);

  persistent_op* op = new persistent_op;
  op->content = buf;
  op->count = count;
  op->datatype = datatype;
  op->partner = source;
  op->tag = tag;
  op->comm = comm;
  op->optype = persistent_op::Recv;

  req->persist(op);

  return MPI_SUCCESS;
}

int
mpi_api::irecv(void *buf, int count, MPI_Datatype datatype, int source,
               int tag, MPI_Comm comm, MPI_Request *request)
{
  SSTMACBacktrace("MPI_Irecv");
  mpi_comm* commPtr = get_comm(comm);
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request | sprockit::dbg::mpi_pt2pt,
    "MPI_Irecv(%d,%s,%s,%s,%s)",
    count, type_str(datatype).c_str(),
    src_str(commPtr, source).c_str(), tag_str(tag).c_str(),
    comm_str(comm).c_str());

  mpi_request* req = mpi_request::construct(default_key_category);

  queue_->recv(req, count, datatype, source, tag, commPtr, buf);

  *request = add_request_ptr(req);

  return MPI_SUCCESS;
}


}
