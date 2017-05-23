/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>

#undef start
#define start(coll, fxn, comm, count, type, ...) \
  start_##coll(comm,count,type,__VA_ARGS__); \
  start_mpi_call(fxn,count,type,comm)

#define start_root(coll, fxn, comm, count, type, root, ...) \
  start(coll, fxn, comm, count, type, root, __VA_ARGS__); \
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective, \
    "%s(%d,%s,%d,%s)", #fxn, count, type_str(type).c_str(), \
    int(root), comm_str(comm).c_str())

#define start_all(coll, fxn, comm, count, type, ...) \
  start(coll, fxn, comm, count, type, __VA_ARGS__); \
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective, \
    "%s(%d,%s,%s)", #fxn, \
    count, type_str(type).c_str(), \
    comm_str(comm).c_str())

namespace sumi {

void
mpi_api::add_immediate_collective(collective_op_base* op, MPI_Request* req)
{
  mpi_request* reqPtr = mpi_request::construct(mpi_request::Collective, default_key_category);
  reqPtr->set_collective(op);
  op->comm->add_request(op->tag, reqPtr);
  add_request_ptr(reqPtr, req);
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
      if (sendbuf != MPI_IN_PLACE) op->sendbuf = nullptr;
    } else if (recvtype == MPI_DATATYPE_NULL || recvtype == MPI_NULL){
      recvtype = sendtype;
      op->recvcnt = op->sendcnt;
      if (recvbuf != MPI_IN_PLACE) op->recvbuf = nullptr;
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
    op->sendtype->pack_send(op->sendbuf, newbuf, op->sendcnt);
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
mpi_api::finish_collective_op(collective_op_base* op_)
{
  collective_op* op = static_cast<collective_op*>(op_);
  mpi_api_debug(sprockit::dbg::mpi_collective,
                "finishing op on tag %d for collective %s: packed=(%d,%d)",
                op->tag, collective::tostr(op->ty),
                op->packed_send, op->packed_recv);

  if (op->packed_recv){
    op->recvtype->unpack_recv(op->tmp_recvbuf, op->recvbuf, op->recvcnt);
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

  if (op->comm->id() == MPI_COMM_WORLD){
    //os_->set_call_graph_active(true);
    crossed_comm_world_barrier_ = true;
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
mpi_api::start_allgather(MPI_Comm comm, int sendcount, MPI_Datatype sendtype,
                         int recvcount, MPI_Datatype recvtype, const void *sendbuf, void *recvbuf)
{
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
mpi_api::allgather(int count, MPI_Datatype type, MPI_Comm comm){
  return allgather(count, type, count, type, comm);
}


int
mpi_api::allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                   void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
{
  collective_op_base* op = start_all(allgather, MPI_Allgather, comm,
                        sendcount, sendtype, recvcount, recvtype,
                        sendbuf, recvbuf);
  wait_collective(op);
  delete op;
  finish_mpi_call(MPI_Allgather);
  return MPI_SUCCESS;

}

int
mpi_api::iallgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                    void *recvbuf, int recvcount, MPI_Datatype recvtype,
                    MPI_Comm comm, MPI_Request *req)
{
  collective_op_base* op = start_all(allgather, MPI_Iallgather, comm,
                                 sendcount, sendtype,
                                 recvcount, recvtype,
                                 sendbuf, recvbuf);
  add_immediate_collective(op, req);
  finish_Impi_call(MPI_Iallgather,req);
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
mpi_api::start_alltoall(MPI_Comm comm, int sendcount, MPI_Datatype sendtype,
                        int recvcount, MPI_Datatype recvtype, const void *sendbuf, void *recvbuf)
{
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
  collective_op_base* op = start_all(alltoall, MPI_Alltoall, comm,
                                 sendcount, sendtype,
                                 recvcount, recvtype,
                                 sendbuf, recvbuf);
  wait_collective(op);
  delete op;
  finish_mpi_call(MPI_Alltoall);
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
  collective_op_base* op = start_all(alltoall, MPI_Ialltoall, comm,
                                 sendcount, sendtype,
                                 recvcount, recvtype,
                                 sendbuf, recvbuf);
  add_immediate_collective(op, req);
  finish_Impi_call(MPI_Ialltoall,req);
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
mpi_api::start_allreduce(MPI_Comm comm, int count, MPI_Datatype type,
                         MPI_Op mop, const void* src, void* dst)
{
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
  collective_op_base* op = start_all(allreduce, MPI_Allreduce, comm,
                                 count, type, mop, src, dst);
  wait_collective(op);
  delete op;
  finish_mpi_call(MPI_Allreduce);
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
  collective_op_base* op = start_all(allreduce,MPI_Iallreduce, comm,
                                      count, type, mop, src, dst);
  add_immediate_collective(op, req);
  finish_Impi_call(MPI_Iallreduce,req);
  return MPI_SUCCESS;
}

int
mpi_api::iallreduce(int count, MPI_Datatype type, MPI_Op op,
                    MPI_Comm comm, MPI_Request* req)
{
  return iallreduce(NULL, NULL, count, type, op, comm, req);
}

void
mpi_api::start_scan(collective_op* op)
{
  reduce_fxn fxn = get_collective_function(op);
  transport::scan(op->tmp_recvbuf, op->tmp_sendbuf, op->sendcnt,
                  op->sendtype->packed_size(), op->tag,
                  fxn, false, options::initial_context, op->comm);
}

collective_op_base*
mpi_api::start_scan(MPI_Comm comm, int count, MPI_Datatype type,
                    MPI_Op mop, const void* src, void* dst)
{
  collective_op* op = new collective_op(count, get_comm(comm));
  if (src == MPI_IN_PLACE){
    src = dst;
  }

  op->op = mop;
  start_mpi_collective(collective::scan, src, dst, type, type, op);
  start_scan(op);
  return op;
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
  start_mpi_call(MPI_Barrier,0,0,comm);
  collective_op_base* op = start_barrier("MPI_Barrier", comm);
  wait_collective(op);
  delete op;
  finish_mpi_call(MPI_Barrier);
  return MPI_SUCCESS;
}

int
mpi_api::ibarrier(MPI_Comm comm, MPI_Request *req)
{
  start_mpi_call(MPI_Ibarrier,0,0,comm);
  collective_op_base* op = start_barrier("MPI_Ibarrier", comm);
  add_immediate_collective(op, req);
  finish_Impi_call(MPI_Ibarrier,req);
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
mpi_api::start_bcast(MPI_Comm comm, int count, MPI_Datatype datatype, int root, void *buffer)
{
  collective_op* op = new collective_op(count, get_comm(comm));
  void* sendbuf, *recvbuf;
  op->root = root;
  MPI_Datatype sendtype, recvtype;
  if (op->comm->rank() == root){
    sendbuf = buffer;
    recvbuf = nullptr;
    sendtype = datatype;
    recvtype = MPI_DATATYPE_NULL;
  } else {
    sendbuf = nullptr;
    recvbuf = buffer;
    sendtype = MPI_DATATYPE_NULL;
    recvtype = datatype;
  }

  start_mpi_collective(collective::bcast, sendbuf, recvbuf, sendtype, recvtype, op);
  start_bcast(op);

  return op;
}

int
mpi_api::bcast(void* buffer, int count, MPI_Datatype type, int root, MPI_Comm comm)
{
  collective_op_base* op = start_root(bcast, MPI_Bcast, comm, count, type, root, buffer);
  wait_collective(op);
  delete op;
  finish_mpi_call(MPI_Bcast);
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
  collective_op_base* op = start_root(bcast, MPI_Ibcast, comm, count, type, root, buffer);
  add_immediate_collective(op, req);
  finish_Impi_call(MPI_Ibcast,req);
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
mpi_api::start_gather(MPI_Comm comm, int sendcount, MPI_Datatype sendtype, int root,
                      int recvcount, MPI_Datatype recvtype, const void *sendbuf, void *recvbuf)
{
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
    recvbuf = nullptr;
  }

  start_mpi_collective(collective::gather, sendbuf, recvbuf, sendtype, recvtype, op);
  start_gather(op);
  return op;
}

int
mpi_api::gather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  collective_op_base* op = start_root(gather, MPI_Gather, comm, sendcount, sendtype, root,
                                 recvcount, recvtype, sendbuf, recvbuf);
  wait_collective(op);
  delete op;
  finish_mpi_call(MPI_Gather);
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
  collective_op_base* op = start_root(gather, MPI_Igather, comm, sendcount, sendtype, root,
                                      recvcount, recvtype, sendbuf, recvbuf);
  add_immediate_collective(op, req);
  finish_Impi_call(MPI_Igather,req);
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
mpi_api::start_reduce(MPI_Comm comm, int count, MPI_Datatype type, int root,
                      MPI_Op mop, const void* src, void* dst)
{
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
    dst = nullptr;
  }

  start_mpi_collective(collective::reduce, src, dst, sendtype, recvtype, op);
  start_reduce(op);

  return op;
}

int
mpi_api::reduce(const void *src, void *dst, int count,
                MPI_Datatype type, MPI_Op mop, int root, MPI_Comm comm)
{
  collective_op_base* op = start_root(reduce, MPI_Reduce, comm, count,
                                 type, root, mop, src, dst);
  wait_collective(op);
  delete op;
  finish_mpi_call(MPI_Reduce);
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
  collective_op_base* op = start_root(reduce, MPI_Ireduce, comm, count,
                                 type, root, mop, sendbuf, recvbuf);
  add_immediate_collective(op, req);
  finish_Impi_call(MPI_Ireduce,req);
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
mpi_api::start_reduce_scatter(MPI_Comm comm, const int* recvcounts, MPI_Datatype type,
                              MPI_Op mop, const void* src, void* dst)
{
  spkt_throw(sprockit::unimplemented_error,
    "sumi::reduce_scatter");

  collective_op* op = nullptr;
  start_mpi_collective(collective::reduce_scatter, src, dst, type, type, op);
  start_reduce_scatter(op);
  op->op = mop;

  return op;
}

int
mpi_api::reduce_scatter(const void *src, void *dst, const int *recvcnts,
                        MPI_Datatype type, MPI_Op mop, MPI_Comm comm)
{
  collective_op_base* op = start_all(reduce_scatter,MPI_Reduce_scatter,
                                     comm,recvcnts,type,mop,src,dst);
  wait_collective(op);
  delete op;
  finish_mpi_call(MPI_Reduce_scatter);
  return MPI_SUCCESS;
}

int
mpi_api::reduce_scatter(int *recvcnts, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  return reduce_scatter(NULL, NULL, recvcnts, type, op, comm);
}

int
mpi_api::ireduce_scatter(const void *src, void *dst, const int *recvcnts,
                        MPI_Datatype type, MPI_Op mop,
                        MPI_Comm comm, MPI_Request* req)
{
  collective_op_base* op = start_all(reduce_scatter,
                MPI_Ireduce_scatter, comm, recvcnts, type, mop, src, dst);
  add_immediate_collective(op, req);
  finish_Impi_call(MPI_Ireduce_scatter,req);
  return MPI_SUCCESS;
}

int
mpi_api::ireduce_scatter(int *recvcnts, MPI_Datatype type,
                         MPI_Op op, MPI_Comm comm, MPI_Request* req)
{
  return ireduce_scatter(NULL, NULL, recvcnts, type, op, comm, req);
}

collective_op_base*
mpi_api::start_reduce_scatter_block(MPI_Comm comm, int count, MPI_Datatype type,
                                    MPI_Op mop, const void* src, void* dst)
{
  spkt_throw(sprockit::unimplemented_error,
    "sumi::reduce_scatter");

  collective_op* op = nullptr;
  start_mpi_collective(collective::reduce_scatter, src, dst, type, type, op);
  start_reduce_scatter(op);
  op->op = mop;

  return op;
}

int
mpi_api::reduce_scatter_block(const void *src, void *dst, int recvcnt,
                        MPI_Datatype type, MPI_Op mop, MPI_Comm comm)
{
  collective_op_base* op = start_all(reduce_scatter_block,
       MPI_Reduce_scatter_block, comm, recvcnt, type, mop, src, dst);
  wait_collective(op);
  delete op;
  finish_mpi_call(MPI_Reduce_scatter_block);
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
  collective_op_base* op = start_all(reduce_scatter_block,
        MPI_Ireduce_scatter_block, comm, recvcnt, type, mop, src, dst);
  add_immediate_collective(op, req);
  finish_Impi_call(MPI_Ireduce_scatter_block,req);
  return MPI_SUCCESS;
}

int
mpi_api::ireduce_scatter_block(int recvcnt, MPI_Datatype type,
                         MPI_Op op, MPI_Comm comm, MPI_Request* req)
{
  return ireduce_scatter_block(NULL, NULL, recvcnt, type, op, comm, req);
}

int
mpi_api::scan(const void *src, void *dst, int count, MPI_Datatype type, MPI_Op mop, MPI_Comm comm)
{
  collective_op_base* op = start_all(scan, MPI_Scan, comm, count, type, mop, src, dst);
  wait_collective(op);
  delete op;
  finish_mpi_call(MPI_Scan);
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
  collective_op_base* op = start_all(scan, MPI_Iscan, comm, count, type, mop, src, dst);
  add_immediate_collective(op, req);
  finish_Impi_call(MPI_Iscan,req);
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
mpi_api::start_scatter(MPI_Comm comm, int sendcount, MPI_Datatype sendtype, int root,
                       int recvcount, MPI_Datatype recvtype, const void *sendbuf, void *recvbuf)
{
  collective_op* op = new collective_op(sendcount, recvcount, get_comm(comm));

  op->root = root;
  if (root == op->comm->rank()){
    //pass
  } else {
    sendtype = MPI_DATATYPE_NULL;
    sendbuf = nullptr;
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
  collective_op_base* op = start_root(scatter, MPI_Scatter, comm, sendcount, sendtype, root,
                             recvcount, recvtype, sendbuf, recvbuf);
  wait_collective(op);
  delete op;
  finish_mpi_call(MPI_Scatter);
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
  collective_op_base* op = start_root(scatter, MPI_Iscatter, comm, sendcount, sendtype, root,
                             recvcount, recvtype, sendbuf, recvbuf);
  add_immediate_collective(op, req);
  finish_Impi_call(MPI_Iscatter,req);
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