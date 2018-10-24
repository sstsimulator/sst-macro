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

#ifndef sstmac_software_libraries_mpi_MPIPROTOCOL_H
#define sstmac_software_libraries_mpi_MPIPROTOCOL_H

#include <sumi-mpi/mpi_queue/mpi_queue_fwd.h>
#include <sumi-mpi/mpi_api_fwd.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request_fwd.h>
#include <sumi-mpi/mpi_request_fwd.h>
#include <sumi-mpi/mpi_message.h>
#include <sprockit/sim_parameters_fwd.h>
#include <sstmac/common/timestamp.h>

namespace sumi {

/**
 * @brief The mpi_protocol class
 */
class mpi_protocol : public sprockit::printable {

 public:
  enum PROTOCOL_ID {
    EAGER0=0,
    EAGER1=1,
    RENDEZVOUS_GET=2,
    NUM_PROTOCOLS
  };

 public:
  virtual void start(void* buffer, int src_rank, int dst_rank, sstmac::sw::task_id tid, int count, mpi_type* typeobj,
                     int tag, MPI_Comm comm, int seq_id, mpi_request* req) = 0;

  virtual void incoming(mpi_message* msg) = 0;

  virtual void incoming(mpi_message *msg, mpi_queue_recv_request* req) = 0;

 protected:
  mpi_protocol(mpi_queue* queue);

  mpi_queue* queue_;

  mpi_api* mpi_;

  void* fill_send_buffer(int count, void *buffer, mpi_type *typeobj);
};

/**
 * @brief The eager0 class
 * Basic eager protocol. Sender copies into a temp buf before sending.
 * MPI_Send completes immediately. Assumes dest has allocated "mailbox"
 * temp buffers to receive unexpected messages.
 */
class eager0 final : public mpi_protocol
{
 public:
  eager0(sprockit::sim_parameters* params, mpi_queue* queue) :
    mpi_protocol(queue){}

  ~eager0(){}

  std::string to_string() const override {
    return "eager0";
  }

  void start(void* buffer, int src_rank, int dst_rank, sstmac::sw::task_id tid, int count, mpi_type* typeobj,
             int tag, MPI_Comm comm, int seq_id, mpi_request* req) override;

  void incoming(mpi_message *msg) override;

  void incoming(mpi_message *msg, mpi_queue_recv_request* req) override;

 private:
  std::map<uint64_t,void*> send_flows_;

};

/**
 * @brief The eager1 class
 * The eager1 protocol uses RDMA to complete the operation, but
 * copies into buffers to allow send/recv to complete faster.
 * On MPI_Send, the source copies into a temp buffer.
 * It then sends an RDMA header to the dest.
 * Whenever the header arrives - even if before MPI_Recv is posted,
 * the dest allocates and temp buffer and then posts an RDMA get
 * from temp buf into temp buf.  On MPI_Recv (or completion of transfer),
 * the final result is copied from temp into recv buf and temp bufs are freed.
 */
class eager1 final : public mpi_protocol
{
 public:
  eager1(sprockit::sim_parameters* params, mpi_queue* queue)
    : mpi_protocol(queue) {}

  virtual ~eager1(){}

  std::string to_string() const override {
    return "eager1";
  }

  void start(void* buffer, int src_rank, int dst_rank, sstmac::sw::task_id tid, int count, mpi_type* typeobj,
             int tag, MPI_Comm comm, int seq_id, mpi_request* req) override;

  void incoming(mpi_message *msg) override;

  void incoming(mpi_message *msg, mpi_queue_recv_request* req) override;

 private:
  void incoming_ack(mpi_message* msg);
  void incoming_header(mpi_message* msg);
  void incoming_payload(mpi_message* msg);

};

class rendezvous_protocol : public mpi_protocol
{
 public:
  rendezvous_protocol(sprockit::sim_parameters* params, mpi_queue* queue);

  std::string to_string() const override {
    return "rendezvous";
  }

  virtual ~rendezvous_protocol(){}

 protected:
  bool software_ack_;
  bool pin_delay_;

};


/**
 * @brief The rendezvous_get class
 * Encapsulates a rendezvous protocol. On MPI_Send, the source sends an RDMA header
 * to the destination. On MPI_Recv, the destination then posts an RDMA get.
 * Hardware acks arrive at both dest/source signaling done.
 */
class rendezvous_get final : public rendezvous_protocol
{
 public:
  rendezvous_get(sprockit::sim_parameters* params, mpi_queue* queue) :
    rendezvous_protocol(params, queue)
  {
  }

  ~rendezvous_get();

  void start(void* buffer, int src_rank, int dst_rank, sstmac::sw::task_id tid, int count, mpi_type* type,
             int tag, MPI_Comm comm, int seq_id, mpi_request* req) override;

  void incoming(mpi_message *msg) override;

  void incoming(mpi_message *msg, mpi_queue_recv_request* req) override;

  std::string to_string() const override {
    return "rendezvous protocol rdma";
  }

 private:
  struct send {
    mpi_request* req;
    void* original;
    void* temporary;
    send(mpi_request* r, void* o, void* t) :
      req(r), original(0), temporary(t){}
  };

  void incoming_ack(mpi_message* msg);
  void incoming_header(mpi_message* msg);
  void incoming_payload(mpi_message* msg);
  void* configure_send_buffer(int count, void* buffer, mpi_type* obj);

  std::map<uint64_t,mpi_queue_recv_request*> recv_flows_;

  std::map<uint64_t,send> send_flows_;
};

}

#endif // MPIPROTOCOL_H
