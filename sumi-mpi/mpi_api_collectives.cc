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
mpi_api::add_immediate_collective(collective_op_base* op, MPI_Request* req)
{
  mpi_request* reqPtr = mpi_request::construct(default_key_category);
  reqPtr->set_collective(op);
  op->comm->add_request(op->tag, reqPtr);
  *req = add_request_ptr(reqPtr);
}

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
      queue_->incoming_progress_loop_message(mpiMsg);
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

collective_op_base*
mpi_api::start_allgather(const char* name, const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                   void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
{
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "%s(%d,%s,%d,%s,%s)",
    name,
    sendcount, type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    comm_str(comm).c_str());

  collective_op* op = new collective_op(sendcount, recvcount, get_comm(comm));
  start_mpi_collective(collective::allgather, sendbuf, recvbuf, sendtype, recvtype, op);
  start_allgather(op);
  return op;
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
  collective_op_base* op = start(allgather, "MPI_Allgather",
                        sendbuf, sendcount, sendtype,
                        recvbuf, recvcount, recvtype, comm);
  wait_collective(op);
  delete op;
  return MPI_SUCCESS;

}

int
mpi_api::iallgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                    void *recvbuf, int recvcount, MPI_Datatype recvtype,
                    MPI_Comm comm, MPI_Request *req)
{
  collective_op_base* op = start(allgather,
                                 "MPI_Iallgather", sendbuf, sendcount, sendtype,
                                 recvbuf, recvcount, recvtype, comm);
  add_immediate_collective(op, req);
  return MPI_SUCCESS;
}

int
mpi_api::iallgather(int sendcount, MPI_Datatype sendtype,
                    int recvcount, MPI_Datatype recvtype,
                    MPI_Comm comm, MPI_Request *req)
{
  return iallgather(NULL, sendcount, sendtype, NULL, recvcount, recvtype, comm, req);
}

void
mpi_api::start_alltoall(collective_op* op)
{
  transport::alltoall(op->tmp_recvbuf, op->tmp_sendbuf, op->sendcnt,
                      op->sendtype->packed_size(), op->tag,
                      false, options::initial_context, op->comm);
}

collective_op_base*
mpi_api::start_alltoall(const char* name, const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                  void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
{
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "%s(%d,%s,%d,%s,%s)", name,
    sendcount, type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    comm_str(comm).c_str());
  collective_op* op = new collective_op(sendcount, recvcount, get_comm(comm));
  start_mpi_collective(collective::alltoall, sendbuf, recvbuf, sendtype, recvtype, op);
  start_alltoall(op);
  return op;
}

int
mpi_api::alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                  void *recvbuf, int recvcount, MPI_Datatype recvtype,
                  MPI_Comm comm)
{
  collective_op_base* op = start(alltoall,
                                "MPI_Alltoall", sendbuf, sendcount, sendtype,
                                recvbuf, recvcount, recvtype, comm);
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

int
mpi_api::ialltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                  void *recvbuf, int recvcount, MPI_Datatype recvtype,
                  MPI_Comm comm, MPI_Request* req)
{
  collective_op_base* op = start(alltoall,
                                "MPI_Ialltoall", sendbuf, sendcount, sendtype,
                                recvbuf, recvcount, recvtype, comm);
  add_immediate_collective(op, req);
  return MPI_SUCCESS;
}

int
mpi_api::ialltoall(int sendcount, MPI_Datatype sendtype,
                  int recvcount, MPI_Datatype recvtype,
                   MPI_Comm comm, MPI_Request* req)
{
  return ialltoall(NULL, sendcount, sendtype, NULL,
                   recvcount, recvtype, comm, req);
}

void
mpi_api::start_allreduce(collective_op* op)
{
  reduce_fxn fxn = get_collective_function(op);
  transport::allreduce(op->tmp_recvbuf, op->tmp_sendbuf, op->sendcnt,
                       op->sendtype->packed_size(), op->tag,
                       fxn, false, options::initial_context, op->comm);
}

collective_op_base*
mpi_api::start_allreduce(const char* name, const void *src, void *dst, int count,
                   MPI_Datatype type, MPI_Op mop, MPI_Comm comm)
{

  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "%s(%d,%s,%s,%s)", name,
    count, type_str(type).c_str(), op_str(mop), comm_str(comm).c_str());

  collective_op* op = new collective_op(count, get_comm(comm));
  if (src == MPI_IN_PLACE){
    src = dst;
  }

  op->op = mop;
  start_mpi_collective(collective::allreduce, src, dst, type, type, op);
  start_allreduce(op);
  return op;
}

int
mpi_api::allreduce(const void *src, void *dst, int count,
                   MPI_Datatype type, MPI_Op mop, MPI_Comm comm)
{
  collective_op_base* op = start(allreduce, "MPI_Allreduce",
                                 src, dst, count, type, mop, comm);
  wait_collective(op);
  delete op;
  return MPI_SUCCESS;
}

int
mpi_api::allreduce(int count, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  return allreduce(NULL, NULL, count, type, op, comm);
}

int
mpi_api::iallreduce(const void *src, void *dst, int count,
                   MPI_Datatype type, MPI_Op mop,
                    MPI_Comm comm, MPI_Request* req)
{
  collective_op_base* op = start_allreduce("MPI_Iallreduce",
                                      src, dst, count, type, mop, comm);
  add_immediate_collective(op, req);
  return MPI_SUCCESS;
}

int
mpi_api::iallreduce(int count, MPI_Datatype type, MPI_Op op,
                    MPI_Comm comm, MPI_Request* req)
{
  return iallreduce(NULL, NULL, count, type, op, comm, req);
}

void
mpi_api::start_barrier(collective_op* op)
{
  op->ty = collective::barrier;
  transport::barrier(op->tag, false, op->comm);
}

collective_op_base*
mpi_api::start_barrier(const char* name, MPI_Comm comm)
{
  collective_op* op = new collective_op(0, get_comm(comm));
  mpi_api_debug(sprockit::dbg::mpi, "%s(%s) on tag %d",
    name, comm_str(comm).c_str(), int(op->tag));
  start_barrier(op);
  return op;
}

int
mpi_api::barrier(MPI_Comm comm)
{
  collective_op_base* op = start(barrier, "MPI_Barrier", comm);
  wait_collective(op);
  delete op;
  return MPI_SUCCESS;
}

int
mpi_api::ibarrier(MPI_Comm comm, MPI_Request *req)
{
  collective_op_base* op = start(barrier, "MPI_Ibarrier", comm);
  add_immediate_collective(op, req);
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

collective_op_base*
mpi_api::start_bcast(const char* name, void* buffer, int count,
               MPI_Datatype type, int root, MPI_Comm comm)
{
  mpi_api_debug(sprockit::dbg::mpi, "%s(%d,%s,%d,%s)", name,
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

  return op;
}

int
mpi_api::bcast(void* buffer, int count, MPI_Datatype type, int root, MPI_Comm comm)
{
  collective_op_base* op = start(bcast, "MPI_Bcast", buffer, count, type, root, comm);
  wait_collective(op);
  delete op;
  return MPI_SUCCESS;
}

int
mpi_api::bcast(int count, MPI_Datatype datatype, int root, MPI_Comm comm)
{
  return bcast(NULL, count, datatype, root, comm);
}

int
mpi_api::ibcast(void* buffer, int count, MPI_Datatype type, int root,
                MPI_Comm comm, MPI_Request* req)
{
  collective_op_base* op = start(bcast, "MPI_Ibcast", buffer, count, type, root, comm);
  add_immediate_collective(op, req);
  return MPI_SUCCESS;
}

int
mpi_api::ibcast(int count, MPI_Datatype datatype, int root,
                MPI_Comm comm, MPI_Request* req)
{
  return ibcast(NULL, count, datatype, root, comm, req);
}

void
mpi_api::start_gather(collective_op* op)
{
  transport::gather(op->root, op->tmp_recvbuf, op->tmp_sendbuf, op->sendcnt,
                    op->sendtype->packed_size(), op->tag,
                    false, options::initial_context, op->comm);
}

collective_op_base*
mpi_api::start_gather(const char* name, const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  mpi_api_debug(sprockit::dbg::mpi,
    "%s(%d,%s,%d,%s,%d,%s)", name,
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
  return op;
}

int
mpi_api::gather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  collective_op_base* op = start(gather, "MPI_Gather", sendbuf, sendcount, sendtype,
                                 recvbuf, recvcount, recvtype, root, comm);
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

int
mpi_api::igather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                void *recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                MPI_Comm comm, MPI_Request* req)
{
  collective_op_base* op = start(gather, "MPI_Igather", sendbuf, sendcount, sendtype,
                                 recvbuf, recvcount, recvtype, root, comm);
  add_immediate_collective(op, req);
  return MPI_SUCCESS;
}

int
mpi_api::igather(int sendcount, MPI_Datatype sendtype,
                int recvcount, MPI_Datatype recvtype, int root,
                MPI_Comm comm, MPI_Request* req)
{
  return igather(NULL, sendcount, sendtype, NULL,
                 recvcount, recvtype, root, comm, req);
}

reduce_fxn
mpi_api::get_collective_function(collective_op_base* op)
{
  if (op->op >= first_custom_op_id){
    auto iter = custom_ops_.find(op->op);
    if (iter == custom_ops_.end()){
      spkt_throw_printf(sprockit::value_error,
                        "Got invalid MPI_Op %d",
                        op->op);
   }
    MPI_User_function* mpifxn = iter->second;
    MPI_Datatype dtype = op->sendtype->id;
    reduce_fxn fxn = ([=](void* dst, const void* src, int count){
      MPI_Datatype copy_type = dtype;
      (*mpifxn)(const_cast<void*>(src), dst, &count, &copy_type);
    });
    return fxn;
  } else if (op->tmp_sendbuf){
    return op->sendtype->op(op->op);
  } else {
    //the function is irrelevant
    //just give it the integer add - function
    return &ReduceOp<Add,int>::op;
  }
}

void
mpi_api::start_reduce(collective_op* op)
{
  reduce_fxn fxn = get_collective_function(op);
  transport::reduce(op->root, op->tmp_recvbuf, op->tmp_sendbuf, op->sendcnt,
                    op->sendtype->packed_size(), op->tag,
                    fxn, false, options::initial_context, op->comm);
}

collective_op_base*
mpi_api::start_reduce(const char* name, const void *src, void *dst, int count,
                      MPI_Datatype type, MPI_Op mop, int root, MPI_Comm comm)
{
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
  collective_op_base* op = start(reduce, "MPI_Reduce", src, dst, count,
                                 type, mop, root, comm);
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
  collective_op_base* op = start(reduce, "Ireduce", sendbuf, recvbuf,
                                 count, type, mop, root, comm);
  add_immediate_collective(op, req);
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

  reduce_fxn fxn = get_collective_function(op);

  //transport::allreduce(op->tmp_recvbuf, op->tmp_sendbuf, op->sendcnt,
  //                     op->sendtype->packed_size(), op->tag,
  //                     fxn, false, options::initial_context, op->comm);

}

collective_op_base*
mpi_api::start_reduce_scatter(const char *name, const void *src, void *dst,
                              int *recvcnts, MPI_Datatype type,
                              MPI_Op mop, MPI_Comm comm)
{
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "%s(<...>,%s,%s,%s)", name,
    type_str(type).c_str(), op_str(mop), comm_str(comm).c_str());

  spkt_throw(sprockit::unimplemented_error,
    "sumi::reduce_scatter");

  collective_op* op = 0;
  start_mpi_collective(collective::reduce_scatter, src, dst, type, type, op);
  start_reduce_scatter(op);
  op->op = mop;

  return op;
}

int
mpi_api::reduce_scatter(const void *src, void *dst, int *recvcnts,
                        MPI_Datatype type, MPI_Op mop, MPI_Comm comm)
{
  collective_op_base* op = start_reduce_scatter("MPI_Reduce_scatter", src, dst,
                                           recvcnts, type, mop, comm);
  wait_collective(op);
  delete op;
  return MPI_SUCCESS;
}

int
mpi_api::reduce_scatter(int *recvcnts, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  return reduce_scatter(NULL, NULL, recvcnts, type, op, comm);
}

int
mpi_api::ireduce_scatter(const void *src, void *dst, int *recvcnts,
                        MPI_Datatype type, MPI_Op mop,
                        MPI_Comm comm, MPI_Request* req)
{
  collective_op_base* op = start(reduce_scatter,
                "MPI_Ireduce_scatter", src, dst, recvcnts, type, mop, comm);
  add_immediate_collective(op, req);
  return MPI_SUCCESS;
}

int
mpi_api::ireduce_scatter(int *recvcnts, MPI_Datatype type,
                         MPI_Op op, MPI_Comm comm, MPI_Request* req)
{
  return ireduce_scatter(NULL, NULL, recvcnts, type, op, comm, req);
}

collective_op_base*
mpi_api::start_reduce_scatter_block(const char *name, const void *src, void *dst,
                              int recvcnt, MPI_Datatype type,
                              MPI_Op mop, MPI_Comm comm)
{
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "%s(<...>,%s,%s,%s)", name,
    type_str(type).c_str(), op_str(mop), comm_str(comm).c_str());

  spkt_throw(sprockit::unimplemented_error,
    "sumi::reduce_scatter");

  collective_op* op = 0;
  start_mpi_collective(collective::reduce_scatter, src, dst, type, type, op);
  start_reduce_scatter(op);
  op->op = mop;

  return op;
}

int
mpi_api::reduce_scatter_block(const void *src, void *dst, int recvcnt,
                        MPI_Datatype type, MPI_Op mop, MPI_Comm comm)
{
  collective_op_base* op = start_reduce_scatter_block(
       "MPI_Reduce_scatter_block", src, dst,
       recvcnt, type, mop, comm);
  wait_collective(op);
  delete op;
  return MPI_SUCCESS;
}

int
mpi_api::reduce_scatter_block(int recvcnt, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  return reduce_scatter_block(NULL, NULL, recvcnt, type, op, comm);
}

int
mpi_api::ireduce_scatter_block(const void *src, void *dst, int recvcnt,
                        MPI_Datatype type, MPI_Op mop,
                        MPI_Comm comm, MPI_Request* req)
{
  collective_op_base* op = start(reduce_scatter_block,
        "MPI_Ireduce_scatter_block", src, dst, recvcnt, type, mop, comm);
  add_immediate_collective(op, req);
  return MPI_SUCCESS;
}

int
mpi_api::ireduce_scatter_block(int recvcnt, MPI_Datatype type,
                         MPI_Op op, MPI_Comm comm, MPI_Request* req)
{
  return ireduce_scatter_block(NULL, NULL, recvcnt, type, op, comm, req);
}



void
mpi_api::start_scan(collective_op* op)
{
  spkt_throw(sprockit::unimplemented_error, "sumi::scan");
  reduce_fxn fxn = get_collective_function(op);
  //transport::reduce(op->tmp_recvbuf, op->tmp_sendbuf, op->sendcnt,
  //                     op->sendtype->packed_size(), op->tag,
  //                     fxn, false, options::initial_context, op->comm);
}

collective_op_base*
mpi_api::start_scan(const char* name, const void *src, void *dst,
                    int count, MPI_Datatype type, MPI_Op mop, MPI_Comm comm)
{
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "%s(%d,%s,%s,%s)", name,
    count, type_str(type).c_str(), op_str(mop), comm_str(comm).c_str());

  collective_op* op = new collective_op(count, get_comm(comm));
  op->op = mop;

  start_mpi_collective(collective::scan, src, dst, type, type, op);
  start_scan(op);
  return op;
}

int
mpi_api::scan(const void *src, void *dst, int count, MPI_Datatype type, MPI_Op mop, MPI_Comm comm)
{
  collective_op_base* op = start(scan, "MPI_Scan", src, dst, count, type, mop, comm);
  wait_collective(op);
  delete op;
  return MPI_SUCCESS;
}

int
mpi_api::scan(int count, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  return scan(NULL, NULL, count, type, op, comm);
}

int
mpi_api::iscan(const void *src, void *dst, int count, MPI_Datatype type,
               MPI_Op mop, MPI_Comm comm, MPI_Request* req)
{
  collective_op_base* op = start(scan, "MPI_Iscan", src, dst, count, type, mop, comm);
  add_immediate_collective(op, req);
  return MPI_SUCCESS;
}

int
mpi_api::iscan(int count, MPI_Datatype type, MPI_Op op,
               MPI_Comm comm, MPI_Request* req)
{
  return iscan(NULL, NULL, count, type, op, comm, req);
}

void
mpi_api::start_scatter(collective_op* op)
{
  transport::scatter(op->root, op->tmp_recvbuf, op->tmp_sendbuf, op->sendcnt,
                     op->sendtype->packed_size(), op->tag,
                     false, options::initial_context, op->comm);
}

collective_op_base*
mpi_api::start_scatter(const char* name, const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  mpi_api_debug(sprockit::dbg::mpi,
    "%s(%d,%s,%d,%s,%d,%s)", name,
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

  return op;
}

int
mpi_api::scatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 void *recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                 MPI_Comm comm)
{
  collective_op_base* op = start(scatter, "MPI_Scatter", sendbuf, sendcount, sendtype,
                             recvbuf, recvcount, recvtype, root, comm);
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

int
mpi_api::iscatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 void *recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                 MPI_Comm comm, MPI_Request* req)
{
  collective_op_base* op = start(scatter, "MPI_Iscatter", sendbuf, sendcount, sendtype,
                              recvbuf, recvcount, recvtype, root, comm);
  add_immediate_collective(op, req);
  return MPI_SUCCESS;
}

int
mpi_api::iscatter(int sendcount, MPI_Datatype sendtype,
                 int recvcount, MPI_Datatype recvtype,
                 int root, MPI_Comm comm, MPI_Request* req)
{
  return iscatter(NULL, sendcount, sendtype,
                 NULL, recvcount, recvtype,
                 root, comm, req);
}

}
