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

#define start_vcoll(coll, fxn, comm, count, type,...) \
  start_##coll(#fxn,comm,count,type,__VA_ARGS__); \
  start_mpi_call(fxn,count,type,comm)

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
mpi_api::start_allgatherv(const char* name, MPI_Comm comm, int sendcount, MPI_Datatype sendtype,
                          const int *recvcounts, const int *displs, MPI_Datatype recvtype,
                          const void *sendbuf, void *recvbuf)
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
  collective_op_base* op = start_vcoll(allgatherv, MPI_Allgatherv, comm, sendcount, sendtype,
                                       recvcounts, displs, recvtype, sendbuf, recvbuf);
  wait_collective(op);
  delete op;
  finish_mpi_call(MPI_Allgatherv);
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
  collective_op_base* op = start_vcoll(allgatherv, MPI_Iallgatherv, comm, sendcount, sendtype,
                                       recvcounts, displs, recvtype, sendbuf, recvbuf);
  add_immediate_collective(op, req);
  finish_Impi_call(MPI_Iallgatherv,req);
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
mpi_api::start_alltoallv(const char* name, MPI_Comm comm,
                         const int *sendcounts, MPI_Datatype sendtype, const int *sdispls,
                         const int *recvcounts, MPI_Datatype recvtype, const int *rdispls,
                         const void *sendbuf,  void *recvbuf)
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
    collective_op_base* op = start_alltoall(comm, send_count, sendtype,
                                            recv_count, recvtype, sendbuf, recvbuf);
    return op;
  }
}

int
mpi_api::alltoallv(const void *sendbuf, const int *sendcounts,
                   const int *sdispls, MPI_Datatype sendtype,
                   void *recvbuf, const int *recvcounts,
                   const int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
  collective_op_base* op = start_vcoll(alltoallv, MPI_Alltoallv, comm,
                             sendcounts, sendtype, sdispls,
                             recvcounts, recvtype, rdispls, sendbuf, recvbuf);
  wait_collective(op);
  delete op;
  finish_mpi_call(MPI_Alltoallv);
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
  collective_op_base* op = start_vcoll(alltoallv, MPI_Ialltoallv, comm, sendcounts, sendtype, sdispls,
                                       recvcounts, recvtype, rdispls, sendbuf, recvbuf);

  add_immediate_collective(op, req);
  finish_Impi_call(MPI_Ialltoallv,req);
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
mpi_api::start_gatherv(const char* name, MPI_Comm comm, int sendcount, MPI_Datatype sendtype, int root,
                       const int *recvcounts, const int *displs, MPI_Datatype recvtype,
                       const void *sendbuf, void *recvbuf)
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
      recvbuf = nullptr;
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
    collective_op_base* op = start_gather(comm, sendcount, sendtype, root, recvcount, recvtype,
                                          sendbuf, recvbuf);
    return op;
  }
}

int
mpi_api::gatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 void *recvbuf, const int *recvcounts, const int *displs,
                 MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  collective_op_base* op = start_vcoll(gatherv, MPI_Gatherv, comm, sendcount, sendtype, root,
                                    recvcounts, displs, recvtype, sendbuf, recvbuf);
  wait_collective(op);
  delete op;
  finish_mpi_call(MPI_Gatherv);
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
  collective_op_base* op = start_vcoll(gatherv, MPI_Igatherv, comm, sendcount, sendtype, root,
                                       recvcounts, displs, recvtype, sendbuf, recvbuf);
  add_immediate_collective(op, req);
  finish_Impi_call(MPI_Igatherv,req);
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
mpi_api::start_scatterv(const char* name, MPI_Comm comm, const int *sendcounts, MPI_Datatype sendtype, int root,
                        const int *displs, int recvcount, MPI_Datatype recvtype, const void *sendbuf, void *recvbuf)
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
      sendbuf = nullptr;
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
    collective_op_base* op = start_scatter(comm, sendcount, sendtype, root,
                                           recvcount, recvtype, sendbuf, recvbuf);
    return op;
  }
}

int
mpi_api::scatterv(const void* sendbuf, const int* sendcounts, const int *displs, MPI_Datatype sendtype,
                  void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  collective_op_base* op = start_vcoll(scatterv, MPI_Scatterv, comm, sendcounts, sendtype, root, displs,
                                       recvcount, recvtype, sendbuf, recvbuf);
  wait_collective(op);
  delete op;
  finish_mpi_call(MPI_Scatterv);
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
  collective_op_base* op = start_vcoll(scatterv, MPI_Iscatterv, comm, sendcounts, sendtype, root, displs,
                                       recvcount, recvtype, sendbuf, recvbuf);

  add_immediate_collective(op, req);
  finish_Impi_call(MPI_Iscatterv,req);
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