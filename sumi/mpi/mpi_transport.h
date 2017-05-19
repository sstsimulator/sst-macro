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

#ifndef mpi_mpi_transport_h
#define mpi_mpi_transport_h

#include <sumi/monitor.h>
#include <sumi/timeout.h>
#include <sumi/message.h>
#include <sumi/collective.h>
#include <sumi/transport.h>
#include <sumi/comm_functions.h>
#include <sumi/active_msg_transport.h>
#include <mpi.h>

namespace sumi {


class PendingMPI
{
 public:
  typedef enum {
    SmsgSend,
    SmsgRecv,
    RecvGetReq,
    SendGetReq,
    RecvPutReq,
    SendPutReq,
    SendPutAck,
    RDMAGetRecv,
    RDMAGetSend,
    RDMAPutRecv,
    RDMAPutSend,
    SendPingRequest,
    SendPingResponse,
    Null
  } type_t;


  int size;
  int sender;
  int recver;
  int rdma_tag;
  void* send_buf;
  void* recv_buf;
  type_t type;
  PendingMPI* rdma_req;
  int smsg_tag;
  MPI_Request* req;
  message::ptr msg;
  int id;

  PendingMPI();

  void clear();

  static const char*
  tostr(type_t ty);

};


class mpi_transport :
  public active_msg_transport
{
  FactoryRegister("mpi", transport, mpi_transport,
              "Create a SUMI transport suitable for MPI")
 public:
  mpi_transport();

  virtual ~mpi_transport();

  void init();

  void finalize();

  void wait_on_pending();

  public_buffer allocate_public_buffer(int size){
    return public_buffer(::malloc(size));
  }

  public_buffer make_public_buffer(void* buf, int size){
    return public_buffer(buf); //nothing to do
  }

 protected:
  void block_inner_loop();

  void do_send_terminate(int dst);

  void do_send_ping_request(int dst);

  void do_smsg_send(int dst, const message::ptr &msg);

  void do_rdma_get(int src, const message::ptr &msg);

  void do_rdma_put(int dst, const message::ptr &msg);

  void do_nvram_get(int src, const message::ptr &msg);

 private:
  int ping_status_;

  int max_num_requests_;

  int poll_burst_size_;

  MPI_Request* requests_;

  PendingMPI* pending_;

  std::list<PendingMPI*> pending_pool_;

  std::list<PendingMPI*> pending_mpi_;

  std::list<MPI_Request*> request_pool_;

  void new_incoming_msg(int src, int tag, int size);

  void recv_ping_request(int src);

  void recv_ping_response(int src);

  void recv_smsg(int src, int tag, int size);

  void recv_rdma_req(int src, int size, int tag, PendingMPI::type_t ty);

  void process_rdma_get_req(PendingMPI *pending);

  void process_rdma_put_req(PendingMPI *pending);

  void process_smsg(PendingMPI* pending);

  void send_transaction_ack(int dst, const message::ptr& msg);

  void recv_transaction_ack(int src);

  void rdma_get_ack(const message::ptr& msg);

  message::ptr
  deserialize_smsg(PendingMPI* pending, void* extra_md = 0, int md_size = 0);

  typedef enum {
   mpi_rdma_get_tag = 0,
   mpi_rdma_put_tag = 1,
   smsg_send_tag = 2,
   ping_request_tag = 3,
   ping_response_tag = 4,
   transaction_ack = 5,
   terminate_tag = 6,
   rdma_get_payload_tag = 10,
   rdma_put_payload_tag = 16000
  } tag_t;

  static const char*
  tostr(ping_status_t stat);

  static const char*
  tostr(tag_t tag);

  void add_pending(PendingMPI* pending);

  void free_pending(PendingMPI* pending);

  PendingMPI* allocate_pending();

  void clear_pending(PendingMPI* pending);

  void poll();

  void poll_burst();

  void recv_terminate(int src);

  void go_die();

  void go_revive();

  char* allocate_smsg_buffer();

  void transport_smsg_send(int dst, int tag, PendingMPI::type_t ty,
    const message::ptr& msg,
    void* extra_md = 0, int md_size = 0);

};

}

#endif // MPIMGR_H