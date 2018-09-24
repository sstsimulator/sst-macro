/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIREQUEST_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIREQUEST_H_INCLUDED

#include <sstmac/software/process/key.h>
#include <sumi/collective.h>
#include <sumi-mpi/mpi_status.h>
#include <sumi-mpi/mpi_message.h>
#include <sumi-mpi/mpi_comm/mpi_comm_fwd.h>
#include <sstmac/common/sstmac_config.h>


namespace sumi {

/**
 * Persistent send operations (send, bsend, rsend, ssend)
 */
class persistent_op
{
 public:
  /// The arguments.
  int count;
  MPI_Datatype datatype;
  MPI_Comm comm;
  int partner;
  int tag;
  void* content;
};

struct collective_op_base
{

  bool packed_send;
  bool packed_recv;
  void* sendbuf;
  void* recvbuf;
  void* tmp_sendbuf;
  void* tmp_recvbuf;
  int tag;
  MPI_Op op;
  mpi_type* sendtype;
  mpi_type* recvtype;
  collective::type_t ty;
  mpi_comm* comm;
  int sendcnt;
  int recvcnt;
  int root;

  virtual ~collective_op_base(){}

 protected:
  collective_op_base(mpi_comm* cm);

};

struct collective_op :
  public collective_op_base,
  public sprockit::thread_safe_new<collective_op>
{
  collective_op(int count, mpi_comm* comm);
  collective_op(int sendcnt, int recvcnt, mpi_comm* comm);


};

struct collectivev_op :
  public collective_op_base,
  public sprockit::thread_safe_new<collectivev_op>
{
  collectivev_op(int scnt, int* recvcnts, int* disps, mpi_comm* comm);
  collectivev_op(int* sendcnts, int* disps, int rcnt, mpi_comm* comm);
  collectivev_op(int* sendcnts, int* sdisps,
                 int* recvcnts, int* rdisps, mpi_comm* comm);

  int* recvcounts;
  int* sendcounts;
  int* sdisps;
  int* rdisps;
  int size;
};

class mpi_request :
  public sprockit::thread_safe_new<mpi_request>
{
 public:
  typedef enum {
    Send,
    Recv,
    Collective,
    Probe
  } op_type_t;

  mpi_request(op_type_t ty) :
   complete_(false),
   cancelled_(false),
   optype_(ty),
   persistent_op_(nullptr),
   collective_op_(nullptr)
  {
  }

  std::string to_string() const {
    return "mpirequest";
  }

  std::string type_str() const;

  static mpi_request* construct(op_type_t ty){
    return new mpi_request(ty);
  }

  ~mpi_request();

  void complete(mpi_message* msg);

  bool is_complete() const {
    return complete_;
  }

  void cancel() {
    cancelled_ = true;
    complete();
  }

  void complete() {
    complete_ = true;
  }

  void set_complete(bool flag){
    complete_ = flag;
  }

  void set_persistent(persistent_op* op) {
    persistent_op_ = op;
  }

  persistent_op* persistent_data() const {
    return persistent_op_;
  }

  void set_collective(collective_op_base* op) {
    collective_op_ = op;
  }

  collective_op_base* collective_data() const {
    return collective_op_;
  }

  const MPI_Status& status() const {
    return stat_;
  }

  bool is_cancelled() const {
    return cancelled_;
  }

  bool is_persistent() const {
    return persistent_op_;
  }

  bool is_collective() const {
    return collective_op_;
  }

  op_type_t optype() const {
    return optype_;
  }

 private:
  MPI_Status stat_;
  bool complete_;
  bool cancelled_;
  op_type_t optype_;

  persistent_op* persistent_op_;
  collective_op_base* collective_op_;

};

}

#endif
