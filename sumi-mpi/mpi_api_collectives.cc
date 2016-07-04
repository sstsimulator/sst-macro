#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/backtrace.h>

namespace sumi {

void
mpi_api::start_mpi_collective(collective::type_t ty,
                              const void *sendbuf, void *recvbuf,
                              MPI_Datatype sendtype, MPI_Datatype recvtype,
                              collective_op_base* op)
{
  op->ty = ty;
  op->sendbuf = const_cast<void*>(sendbuf);
  op->recvbuf = recvbuf;
  const char* name = collective::tostr(ty);

  if (sendbuf == MPI_IN_PLACE){
    if (recvbuf){
      mpi_type* type = type_from_id(recvtype);
      int offset;
      switch(ty){
        case collective::gather:
        case collective::allgather:
          offset = type->extent() * op->recvcnt * op->comm->rank();
          break;
        default:
          offset = 0;
          break;
      }
      op->sendbuf = ((char*)recvbuf) + offset;
    }
    op->sendcnt = op->recvcnt;
    sendtype = recvtype;
  }

  if (sendtype != recvtype){
    if (sendtype == MPI_DATATYPE_NULL || sendtype == MPI_NULL){
      sendtype = recvtype;
      op->sendcnt = op->recvcnt;
      if (sendbuf != MPI_IN_PLACE) op->sendbuf = 0;
    } else if (recvtype == MPI_DATATYPE_NULL || recvtype == MPI_NULL){
      recvtype = sendtype;
      op->recvcnt = op->sendcnt;
      if (recvbuf != MPI_IN_PLACE) op->recvbuf = 0;
    }
  }

  op->tmp_sendbuf = op->sendbuf;
  op->tmp_recvbuf = op->recvbuf;

  op->sendtype = type_from_id(sendtype);
  op->recvtype = type_from_id(recvtype);
  op->packed_recv = false;
  op->packed_send = false;

  if (op->sendbuf && !op->sendtype->contiguous()){
    void* newbuf = allocate_temp_pack_buffer(op->sendcnt, op->sendtype);
    pack_send(op->sendbuf, newbuf, op->sendcnt, op->sendtype);
    op->tmp_sendbuf = newbuf;
    op->packed_send = true;
  } else {
    op->tmp_sendbuf = op->sendbuf;
  }

  if (op->recvbuf && !op->recvtype->contiguous()){
    void* newbuf = allocate_temp_pack_buffer(op->recvcnt, op->recvtype);
    op->tmp_recvbuf = newbuf;
    op->packed_recv = true;
  } else {
    op->tmp_recvbuf = recvbuf;
  }

}



void
mpi_api::pack_send(void* srcbuf, void* dstbuf, int sendcnt, mpi_type* sendtypePtr)
{
  char* src = (char*) srcbuf;
  char* dst = (char*) dstbuf;
  int src_stride = sendtypePtr->extent();
  int dst_stride = sendtypePtr->packed_size();
  for (int i=0; i < sendcnt; ++i, src += src_stride, dst += dst_stride){
    sendtypePtr->pack(src, dst);
  }
}

void*
mpi_api::allocate_temp_pack_buffer(int count, mpi_type* type)
{
  char* newbuf = new char[type->packed_size()*count];
  return newbuf;
}

void
mpi_api::free_temp_pack_buffer(void* srcbuf)
{
  char* buf = (char*) srcbuf;
  delete[] buf;
}

void
mpi_api::unpack_recv(void *srcbuf, void *dstbuf, int recvcnt, mpi_type* recvtypePtr)
{
  char* src = (char*) srcbuf;
  char* dst = (char*) dstbuf;
  int src_stride = recvtypePtr->packed_size();
  int dst_stride = recvtypePtr->extent();
  for (int i=0; i < recvcnt; ++i, src += src_stride, dst += dst_stride){
    recvtypePtr->unpack(src, dst);
  }
}

void
mpi_api::finish_collective_op(collective_op_base* op_)
{
  collective_op* op = static_cast<collective_op*>(op_);
  mpi_api_debug(sprockit::dbg::mpi_collective,
                "finishing op on tag %d for collective %s: packed=(%d,%d)",
                op->tag, collective::tostr(op->ty),
                op->packed_send, op->packed_recv);

  if (op->packed_recv){
    unpack_recv(op->tmp_recvbuf, op->recvbuf, op->recvcnt, op->recvtype);
    free_temp_pack_buffer(op->tmp_recvbuf);
  }
  if (op->packed_send){
    free_temp_pack_buffer(op->tmp_sendbuf);
  }
}

void
mpi_api::finish_collective(collective_op_base* op)
{
  switch(op->ty){
    case collective::reduce:
    case collective::alltoall:
    case collective::gather:
    case collective::scatter:
    case collective::allreduce:
    case collective::scan:
    case collective::allgather:
    case collective::barrier:
    case collective::reduce_scatter:
    case collective::bcast:
      finish_collective_op(op);
      break;
    case collective::alltoallv:
    case collective::gatherv:
    case collective::scatterv:
    case collective::allgatherv:
      finish_vcollective_op(op);
      break;
    case collective::dynamic_tree_vote:
    case collective::heartbeat:
      break; //nothing doing
  }
}

void
mpi_api::wait_collective(collective_op_base* op)
{
  std::list<collective_done_message::ptr> pending;
  while (1){
    sumi::message::ptr msg = blocking_poll();
    if (msg->class_type() == message::collective_done){
      //this is a collective done message
      collective_done_message::ptr cmsg = ptr_safe_cast(collective_done_message, msg);
      mpi_api_debug(sprockit::dbg::mpi_collective,
                    "found collective done message of type=%s tag=%d: need %s,%d",
                    collective::tostr(cmsg->type()), cmsg->tag(),
                    collective::tostr(op->ty), op->tag);
      if (op->tag == cmsg->tag() && op->ty == cmsg->type()){  //done!
        break;
      } else {
        //a different collective completed
        pending.push_back(cmsg);
      }
    } else {
      mpi_message::ptr mpiMsg = ptr_safe_cast(mpi_message, msg);
      queue_->incoming_message(mpiMsg);
    }
  }

  finish_collective(op);
  
  std::list<collective_done_message::ptr>::iterator it, end = pending.end();
  for (it=pending.begin(); it != end; ++it){
    completion_queue_.push_back(*it);
  }
}

void
mpi_api::start_allgather(collective_op *op)
{
  transport::allgather(op->tmp_recvbuf, op->tmp_sendbuf,
                  op->sendcnt, op->sendtype->packed_size(), op->tag,
                  false, options::initial_context, op->comm);
}

int
mpi_api::allgather(int sendcount, MPI_Datatype sendtype,
                   int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
{
  return allgather(NULL, sendcount, sendtype, NULL, recvcount, recvtype, comm);
}

int
mpi_api::allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                   void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Allgather");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Allgather(%d,%s,%d,%s,%s)",
    sendcount, type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    comm_str(comm).c_str());

  collective_op* op = new collective_op(sendcount, recvcount, get_comm(comm));
  start_mpi_collective(collective::allgather, sendbuf, recvbuf, sendtype, recvtype, op);
  start_allgather(op);
  wait_collective(op);
  delete op;
  return MPI_SUCCESS;
}

void
mpi_api::start_alltoall(collective_op* op)
{
  transport::alltoall(op->tmp_recvbuf, op->tmp_sendbuf, op->sendcnt,
                      op->sendtype->packed_size(), op->tag,
                      false, options::initial_context, op->comm);
}

int
mpi_api::alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                  void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Alltoall");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Alltoall(%d,%s,%d,%s,%s)",
    sendcount, type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    comm_str(comm).c_str());
  collective_op* op = new collective_op(sendcount, recvcount, get_comm(comm));
  start_mpi_collective(collective::alltoall, sendbuf, recvbuf, sendtype, recvtype, op);
  start_alltoall(op);
  wait_collective(op);
  delete op;
  return MPI_SUCCESS;
}

int
mpi_api::alltoall(int sendcount, MPI_Datatype sendtype,
                  int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
{
  return alltoall(NULL, sendcount, sendtype, NULL, recvcount, recvtype, comm);
}

void
mpi_api::start_allreduce(collective_op* op)
{
  reduce_fxn fxn = op->sendtype->op(op->op);
  transport::allreduce(op->tmp_recvbuf, op->tmp_sendbuf, op->sendcnt,
                       op->sendtype->packed_size(), op->tag,
                       fxn, false, options::initial_context, op->comm);
}

int
mpi_api::allreduce(const void *src, void *dst, int count,
                   MPI_Datatype type, MPI_Op mop, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Allreduce");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Allreduce(%d,%s,%s,%s)",
    count, type_str(type).c_str(), op_str(mop), comm_str(comm).c_str());

  collective_op* op = new collective_op(count, get_comm(comm));
  if (src == MPI_IN_PLACE){
    src = dst;
  }

  op->op = mop;
  start_mpi_collective(collective::allreduce, src, dst, type, type, op);
  start_allreduce(op);
  wait_collective(op);
  delete op;

  return MPI_SUCCESS;
}

int
mpi_api::allreduce(int count, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  return allreduce(NULL, NULL, count, type, op, comm);
}

void
mpi_api::start_barrier(collective_op* op)
{
  op->ty = collective::barrier;
  transport::barrier(op->tag, false, op->comm);
}

int
mpi_api::barrier(MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Barrier");
  collective_op* op = new collective_op(0, get_comm(comm));
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Barrier(%s) on tag %d",
    comm_str(comm).c_str(), int(op->tag));
  start_barrier(op);
  wait_collective(op);
  delete op;

  return MPI_SUCCESS;
}

void
mpi_api::start_bcast(collective_op* op)
{
  void* buf = op->comm->rank() == op->root ? op->tmp_sendbuf : op->tmp_recvbuf;
  transport::bcast(op->root, buf,
                   op->sendcnt,
                   op->sendtype->packed_size(), op->tag,
                   false, options::initial_context, op->comm);
}

int
mpi_api::bcast(void* buffer, int count, MPI_Datatype type, int root, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Bcast");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Bcast(%d,%s,%d,%s)",
    count, type_str(type).c_str(), int(root), comm_str(comm).c_str());
  collective_op* op = new collective_op(count, get_comm(comm));
  void* sendbuf, *recvbuf;
  op->root = root;
  MPI_Datatype sendtype, recvtype;
  if (op->comm->rank() == root){
    sendbuf = buffer;
    recvbuf = 0;
    sendtype = type;
    recvtype = MPI_DATATYPE_NULL;
  } else {
    sendbuf = 0;
    recvbuf = buffer;
    sendtype = MPI_DATATYPE_NULL;
    recvtype = type;
  }

  start_mpi_collective(collective::bcast, sendbuf, recvbuf, sendtype, recvtype, op);
  start_bcast(op);
  wait_collective(op);
  delete op;

  return MPI_SUCCESS;
}

int
mpi_api::bcast(int count, MPI_Datatype datatype, int root, MPI_Comm comm)
{
  return bcast(NULL, count, datatype, root, comm);
}

void
mpi_api::start_gather(collective_op* op)
{
  transport::gather(op->root, op->tmp_recvbuf, op->tmp_sendbuf, op->sendcnt,
                    op->sendtype->packed_size(), op->tag,
                    false, options::initial_context, op->comm);
}

int
mpi_api::gather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Gather");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Gather(%d,%s,%d,%s,%d,%s)",
    sendcount, type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    int(root), comm_str(comm).c_str());

  if (sendbuf == MPI_IN_PLACE){
    if (recvbuf){
      mpi_type* type = type_from_id(recvtype);
      mpi_comm* cm = get_comm(comm);
      int offset = type->extent() * recvcount * cm->rank();
      sendbuf = ((char*)recvbuf) + offset;
    }
    sendcount = recvcount;
    sendtype = recvtype;
  }

  collective_op* op = new collective_op(sendcount, recvcount, get_comm(comm));
  op->root = root;

  if (root == op->comm->rank()){
    //pass
  } else {
    recvtype = MPI_DATATYPE_NULL;
    recvbuf = 0;
  }

  start_mpi_collective(collective::gather, sendbuf, recvbuf, sendtype, recvtype, op);
  start_gather(op);
  wait_collective(op);
  delete op;
  return MPI_SUCCESS;
}

int
mpi_api::gather(int sendcount, MPI_Datatype sendtype,
                int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  return gather(NULL, sendcount, sendtype, NULL, recvcount, recvtype, root, comm);
}

void
mpi_api::start_reduce(collective_op* op)
{
  reduce_fxn fxn = op->sendtype->op(op->op);
  transport::reduce(op->root, op->tmp_recvbuf, op->tmp_sendbuf, op->sendcnt,
                    op->sendtype->packed_size(), op->tag,
                    fxn, false, options::initial_context, op->comm);
}

collective_op*
mpi_api::start_reduce(const char* name, const void *src, void *dst, int count,
                      MPI_Datatype type, MPI_Op mop, int root, MPI_Comm comm)
{
  SSTMACBacktrace(name);
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "%s(%d,%s,%s,%d,%s)", name, count, type_str(type).c_str(),
    op_str(mop), int(root), comm_str(comm).c_str());

  collective_op* op = new collective_op(count, get_comm(comm));
  op->root = root;
  op->op = mop;
  MPI_Datatype sendtype, recvtype;
  if (root == op->comm->rank()){
    sendtype = type;
    recvtype = type;
  } else {
    sendtype = type;
    recvtype = MPI_DATATYPE_NULL;
    dst = 0;
  }

  start_mpi_collective(collective::reduce, src, dst, sendtype, recvtype, op);
  start_reduce(op);

  return op;
}

int
mpi_api::reduce(const void *src, void *dst, int count,
                MPI_Datatype type, MPI_Op mop, int root, MPI_Comm comm)
{
  collective_op* op = start_reduce("MPI_Reduce", src, dst, count, type, mop, root, comm);
  wait_collective(op);
  delete op;

  return MPI_SUCCESS;
}

int
mpi_api::reduce(int count, MPI_Datatype type, MPI_Op op, int root, MPI_Comm comm)
{
  return reduce(NULL, NULL, count, type, op, root, comm);
}

int
mpi_api::ireduce(const void* sendbuf, void* recvbuf, int count,
                 MPI_Datatype type, MPI_Op mop, int root, MPI_Comm comm,
                 MPI_Request* req)
{
  collective_op* op = start_reduce("Ireduce", sendbuf, recvbuf, count, type, mop, root, comm);
  mpi_request* reqPtr = mpi_request::construct(default_key_category);
  reqPtr->set_collective(op);
  op->comm->add_request(op->tag, reqPtr);
  *req = add_request_ptr(reqPtr);
  return MPI_SUCCESS;
}

int
mpi_api::ireduce(int count, MPI_Datatype type, MPI_Op op, int root, MPI_Comm comm, MPI_Request* req)
{
  return ireduce(NULL, NULL, count, type, op, root, comm, req);
}

void
mpi_api::start_reduce_scatter(collective_op* op)
{
  spkt_throw(sprockit::unimplemented_error,
    "sumi::reduce_scatter");
  //transport::allreduce(op->tmp_recvbuf, op->tmp_sendbuf, op->sendcnt,
  //                     op->sendtype->packed_size(), op->tag,
  //                     fxn, false, options::initial_context, op->comm);

}

int
mpi_api::reduce_scatter(const void *src, void *dst, int *recvcnts,
                        MPI_Datatype type, MPI_Op mop, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Reducescatter");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Reduce_scatter(<...>,%s,%s,%s)",
    type_str(type).c_str(), op_str(mop), comm_str(comm).c_str());

  spkt_throw(sprockit::unimplemented_error,
    "sumi::reduce_scatter");

  collective_op* op = 0;//new collective_op(count, get_comm(comm));
  op->op = mop;

  start_mpi_collective(collective::reduce_scatter, src, dst, type, type, op);
  start_reduce_scatter(op);
  wait_collective(op);
  delete op;

  return MPI_SUCCESS;
}

int
mpi_api::reduce_scatter(int *recvcnts, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  return reduce_scatter(NULL, NULL, recvcnts, type, op, comm);
}

void
mpi_api::start_scan(collective_op* op)
{
  spkt_throw(sprockit::unimplemented_error,
    "sumi::scan");
  //transport::reduce(op->tmp_recvbuf, op->tmp_sendbuf, op->sendcnt,
  //                     op->sendtype->packed_size(), op->tag,
  //                     fxn, false, options::initial_context, op->comm);
}

int
mpi_api::scan(const void *src, void *dst, int count, MPI_Datatype type, MPI_Op mop, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Scan");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Scan(%d,%s,%s,%s)",
    count, type_str(type).c_str(), op_str(mop), comm_str(comm).c_str());

  collective_op* op = new collective_op(count, get_comm(comm));
  op->op = mop;

  start_mpi_collective(collective::scan, src, dst, type, type, op);
  start_scan(op);
  wait_collective(op);
  delete op;

  return MPI_SUCCESS;
}

int
mpi_api::scan(int count, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  return scan(NULL, NULL, count, type, op, comm);
}

void
mpi_api::start_scatter(collective_op* op)
{
  transport::scatter(op->root, op->tmp_recvbuf, op->tmp_sendbuf, op->sendcnt,
                     op->sendtype->packed_size(), op->tag,
                     false, options::initial_context, op->comm);
}

int
mpi_api::scatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Scatter");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Scatter(%d,%s,%d,%s,%d,%s)",
    sendcount, type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    int(root), comm_str(comm).c_str());

  collective_op* op = new collective_op(sendcount, recvcount, get_comm(comm));

  op->root = root;
  if (root == op->comm->rank()){
    //pass
  } else {
    sendtype = MPI_DATATYPE_NULL;
    sendbuf = 0;
  }

  start_mpi_collective(collective::scatter, sendbuf, recvbuf, sendtype, recvtype, op);
  start_scatter(op);
  wait_collective(op);
  delete op;

  return MPI_SUCCESS;
}

int
mpi_api::scatter(int sendcount, MPI_Datatype sendtype,
                 int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  return scatter(NULL, sendcount, sendtype, NULL, recvcount, recvtype, root, comm);
}

}
