#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/software/process/operating_system.h>

namespace  sumi {

int
mpi_api::send(const void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm)
{
  start_mpi_call("MPI_Send");
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
mpi_api::request_free(MPI_Request *req)
{
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request | sprockit::dbg::mpi_pt2pt,
    "MPI_Request_free(REQ=%d)", *req);

  mpi_request* reqPtr = get_request(*req);
  if (reqPtr){
    if (reqPtr->is_complete()){
      erase_request_ptr(*req);
    } else {
      //you freed an active - request
      //that was silly
      //to punish you, I shall leak this memory
      req_map_.erase(*req);
    }
  }
  *req = MPI_REQUEST_NULL;
  return MPI_SUCCESS;
}

void
mpi_api::do_start(MPI_Request req)
{
  mpi_request* reqPtr = get_request(req);
  persistent_op* op = reqPtr->persistent_data();
  if (op == 0){
    spkt_throw_printf(sprockit::value_error,
                      "Starting MPI_Request that is not persistent");
  }
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request | sprockit::dbg::mpi_pt2pt,
    "MPI_Start(%d,%s,%d:%d,%s,%s;REQ=%d)",
    op->count, type_str(op->datatype).c_str(), op->partner,
    int(get_comm(op->comm)->peer_task(op->partner)),
    tag_str(op->tag).c_str(), comm_str(op->comm).c_str(),
    req);
  reqPtr->set_complete(false);
  mpi_comm* commPtr = get_comm(op->comm);
  if (op->optype == persistent_op::Send){
    queue_->send(reqPtr, op->count, op->datatype, op->partner, op->tag, commPtr, op->content);
  } else {
    queue_->recv(reqPtr, op->count, op->datatype, op->partner, op->tag, commPtr, op->content);
  }
}

int
mpi_api::start(MPI_Request* req)
{
  start_mpi_call("MPI_Start");
  do_start(*req);
  return MPI_SUCCESS;
}

int
mpi_api::startall(int count, MPI_Request* req)
{
  start_mpi_call("MPI_Startall");
  for (int i=0; i < count; ++i){
    do_start(req[i]);
  }
  return MPI_SUCCESS;
}

int
mpi_api::send_init(const void *buf, int count,
                   MPI_Datatype datatype, int dest, int tag,
                   MPI_Comm comm, MPI_Request *request)
{
  start_mpi_call("MPI_Send_init");

  mpi_request* req = mpi_request::construct(default_key_category);
  *request = add_request_ptr(req);

  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request | sprockit::dbg::mpi_pt2pt,
    "MPI_Send_init(%d,%s,%d:%d,%s,%s;REQ=%d)",
    count, type_str(datatype).c_str(), int(dest),
    int(get_comm(comm)->peer_task(dest)),
    tag_str(tag).c_str(), comm_str(comm).c_str(),
    *request);

  persistent_op* op = new persistent_op;
  op->content = const_cast<void*>(buf);
  op->count = count;
  op->datatype = datatype;
  op->partner = dest;
  op->tag = tag;
  op->comm = comm;
  op->optype = persistent_op::Send;

  req->set_persistent(op);

  return MPI_SUCCESS;
}

int
mpi_api::isend(const void *buf, int count, MPI_Datatype datatype, int dest,
               int tag, MPI_Comm comm, MPI_Request *request)
{
  start_mpi_call("MPI_Isend");
  mpi_comm* commPtr = get_comm(comm);
  mpi_request* req = mpi_request::construct(default_key_category);
  *request = add_request_ptr(req);
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request | sprockit::dbg::mpi_pt2pt,
    "MPI_Isend(%d,%s,%d:%d,%s,%s;REQ=%d)",
    count, type_str(datatype).c_str(), int(dest), int(commPtr->peer_task(dest)),
    tag_str(tag).c_str(), comm_str(comm).c_str(), *request);
  queue_->send(req, count, datatype, dest, tag, commPtr, const_cast<void*>(buf));
  return MPI_SUCCESS;
}

int
mpi_api::recv(void *buf, int count, MPI_Datatype datatype, int source,
              int tag, MPI_Comm comm, MPI_Status *status)
{
  start_mpi_call("MPI_Recv");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt,
    "MPI_Recv(%d,%s,%s,%s,%s)",
    count, type_str(datatype).c_str(),
    src_str(source).c_str(), tag_str(tag).c_str(),
    comm_str(comm).c_str());

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
  start_mpi_call("MPI_Recv_init");

  mpi_request* req = mpi_request::construct(default_key_category);
  *request = add_request_ptr(req);

  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt,
    "MPI_Recv_init(%d,%s,%s,%s,%s;REQ=%d)",
    count, type_str(datatype).c_str(),
    src_str(source).c_str(), tag_str(tag).c_str(),
    comm_str(comm).c_str(), *request);

  persistent_op* op = new persistent_op;
  op->content = buf;
  op->count = count;
  op->datatype = datatype;
  op->partner = source;
  op->tag = tag;
  op->comm = comm;
  op->optype = persistent_op::Recv;

  req->set_persistent(op);

  return MPI_SUCCESS;
}

int
mpi_api::irecv(void *buf, int count, MPI_Datatype datatype, int source,
               int tag, MPI_Comm comm, MPI_Request *request)
{
  start_mpi_call("MPI_Irecv");
  mpi_comm* commPtr = get_comm(comm);

  mpi_request* req = mpi_request::construct(default_key_category);
  *request = add_request_ptr(req);

  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request | sprockit::dbg::mpi_pt2pt,
    "MPI_Irecv(%d,%s,%s,%s,%s;REQ=%d)",
    count, type_str(datatype).c_str(),
    src_str(commPtr, source).c_str(), tag_str(tag).c_str(),
    comm_str(comm).c_str(), *request);

  queue_->recv(req, count, datatype, source, tag, commPtr, buf);
  return MPI_SUCCESS;
}


}
