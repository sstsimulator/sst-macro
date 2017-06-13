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

#define start_pt2pt_call(fxn, count, type, partner, tag, comm) \
  start_mpi_call(fxn,count,type,comm); \
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt, \
   "%s(%d,%s,%s,%s,%s)", #fxn, \
   count, type_str(type).c_str(), src_str(partner).c_str(), \
   tag_str(tag).c_str(), comm_str(comm).c_str());

#define start_Ipt2pt_call(fxn,count,type,partner,tag,comm,reqPtr) \
  start_mpi_call(fxn,count,type,comm)


namespace sumi {

int
mpi_api::send(const void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm)
{
  start_pt2pt_call(MPI_Send,count,datatype,dest,tag,comm);
  mpi_comm* commPtr = get_comm(comm);
  mpi_request* req = mpi_request::construct(mpi_request::Send, default_key_category);
  queue_->send(req, count, datatype, dest, tag, commPtr, const_cast<void*>(buf));
  queue_->progress_loop(req);
  delete req;
  finish_mpi_call(MPI_Send);
  return MPI_SUCCESS;
}

int
mpi_api::sendrecv(const void *sendbuf, int sendcount,
 MPI_Datatype sendtype, int dest, int sendtag,
 void *recvbuf, int recvcount,
 MPI_Datatype recvtype, int source, int recvtag,
 MPI_Comm comm, MPI_Status *status)
{
  start_pt2pt_call(MPI_Sendrecv,recvcount,recvtype,source,recvtag,comm);
  mpi_request* req = do_isend(sendbuf, sendcount, sendtype, dest, sendtag, comm);
  do_recv(recvbuf, recvcount, recvtype, source, recvtag, comm, status);
  queue_->progress_loop(req);
  delete req;
  finish_mpi_call(MPI_Sendrecv);
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
  if (op == nullptr){
    spkt_throw_printf(sprockit::value_error,
                      "Starting MPI_Request that is not persistent");
  }

  reqPtr->set_complete(false);
  mpi_comm* commPtr = get_comm(op->comm);
  if (reqPtr->optype() == mpi_request::Send){
    queue_->send(reqPtr, op->count, op->datatype, op->partner, op->tag, commPtr, op->content);
  } else {
    queue_->recv(reqPtr, op->count, op->datatype, op->partner, op->tag, commPtr, op->content);
  }
}

int
mpi_api::start(MPI_Request* req)
{
  _start_mpi_call_(MPI_Start);
  do_start(*req);
  return MPI_SUCCESS;
}

int
mpi_api::startall(int count, MPI_Request* req)
{
  _start_mpi_call_(MPI_Startall);
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
  _start_mpi_call_(MPI_Send_init);

  mpi_request* req = mpi_request::construct(mpi_request::Send,default_key_category);
  add_request_ptr(req, request);

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

  req->set_persistent(op);

  return MPI_SUCCESS;
}

mpi_request*
mpi_api::do_isend(const void *buf, int count, MPI_Datatype datatype, int dest,
               int tag, MPI_Comm comm)
{
  mpi_comm* commPtr = get_comm(comm);
  mpi_request* req = mpi_request::construct(mpi_request::Send,default_key_category);
  queue_->send(req, count, datatype, dest, tag, commPtr, const_cast<void*>(buf));
  return req;
}

int
mpi_api::isend(const void *buf, int count, MPI_Datatype datatype, int dest,
               int tag, MPI_Comm comm, MPI_Request *request)
{
  using namespace sprockit;
  start_Ipt2pt_call(MPI_Isend,count,datatype,dest,tag,comm,request);
  mpi_api_debug(dbg::mpi | dbg::mpi_request | dbg::mpi_pt2pt,
    "MPI_Isend(%d,%s,%d,%s,%s;REQ=%d)",
    count, type_str(datatype).c_str(), int(dest),
    tag_str(tag).c_str(), comm_str(comm).c_str(), *request);
  mpi_request* req = do_isend(buf, count, datatype, dest, tag, comm);
  add_request_ptr(req, request);
  queue_->nonblocking_progress();
  finish_Impi_call(MPI_Isend,request);
  return MPI_SUCCESS;
}

int
mpi_api::recv(void *buf, int count, MPI_Datatype datatype, int source,
              int tag, MPI_Comm comm, MPI_Status *status)
{
  start_pt2pt_call(MPI_Recv,count,datatype,source,tag,comm);
  int rc = do_recv(buf,count,datatype,source,tag,comm,status);
  finish_mpi_call(MPI_Recv);
  return rc;
}

int
mpi_api::do_recv(void *buf, int count, MPI_Datatype datatype, int source,
              int tag, MPI_Comm comm, MPI_Status *status)
{
  mpi_request* req = mpi_request::construct(mpi_request::Recv,default_key_category);
  mpi_comm* commPtr = get_comm(comm);
  queue_->recv(req, count, datatype, source, tag, commPtr, buf);
  queue_->progress_loop(req);
  if (status != MPI_STATUS_IGNORE){
    *status = req->status();
  }
  delete req;
  return MPI_SUCCESS;
}

int
mpi_api::recv_init(void *buf, int count, MPI_Datatype datatype, int source,
                   int tag, MPI_Comm comm, MPI_Request *request)
{
  _start_mpi_call_(MPI_Recv_init);

  mpi_request* req = mpi_request::construct(mpi_request::Recv,default_key_category);
  add_request_ptr(req, request);

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

  req->set_persistent(op);

  return MPI_SUCCESS;
}

int
mpi_api::irecv(void *buf, int count, MPI_Datatype datatype, int source,
               int tag, MPI_Comm comm, MPI_Request *request)
{
  start_Ipt2pt_call(MPI_Irecv,count,datatype,dest,tag,comm,request);

  using namespace sprockit;
  mpi_comm* commPtr = get_comm(comm);

  mpi_request* req = mpi_request::construct(mpi_request::Recv,default_key_category);
  add_request_ptr(req, request);

  mpi_api_debug(dbg::mpi | dbg::mpi_request | dbg::mpi_pt2pt,
      "MPI_Irecv(%d,%s,%s,%s,%s;REQ=%d)",
      count, type_str(datatype).c_str(),
      src_str(commPtr, source).c_str(), tag_str(tag).c_str(),
      comm_str(comm).c_str(), *request);

  queue_->recv(req, count, datatype, source, tag, commPtr, buf);
  queue_->nonblocking_progress();
  finish_Impi_call(MPI_Irecv,request);
  return MPI_SUCCESS;
}


}