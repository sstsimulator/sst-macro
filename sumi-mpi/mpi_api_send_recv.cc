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
mpi_api::isend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request)
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
mpi_api::recv(void *buf, int count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Status *status)
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
mpi_api::irecv(void *buf, int count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Request *request)
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
