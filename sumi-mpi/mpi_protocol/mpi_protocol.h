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

#ifndef sstmac_software_libraries_mpi_MPIPROTOCOL_H
#define sstmac_software_libraries_mpi_MPIPROTOCOL_H

#include <sumi-mpi/mpi_queue/mpi_queue_fwd.h>
#include <sumi-mpi/mpi_queue/mpi_queue_send_request_fwd.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request_fwd.h>
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
    PROTOCOL_INVALID=0,
    EAGER0=1,
    EAGER1_SINGLECPY=2,
    EAGER1_DOUBLECPY=3,
    RENDEZVOUS_GET=4
  };

 public:
  /**
   * @brief send_header  Begin the send operation by sending a header
   * from source to destination. May send payload or RDMA header
   * depending on the protocol.
   * @param queue
   * @param msg
   */
  virtual void
  send_header(mpi_queue* queue, const mpi_message::ptr& msg) = 0;

  /**
   * @brief incoming_header  When the header from #send_header
   * arrives at destination, perform operations needed to process the header
   * @param queue The queue the header arrived at
   * @param msg The header
   */
  virtual void
  incoming_header(mpi_queue* queue, const mpi_message::ptr& msg);

  /**
   * @brief incoming_header  When the header from #send_header
   * arrives at destination, perform operations needed to process the header.
   * May perform extra operations if recv has already been posted (i.e. req
   * is non-null)
   * @param queue The queue the header arrived at
   * @param msg The header
   * @param req A descriptor for the recv. If non-null,
   *            the recv has already been posted.
   */
  virtual void
  incoming_header(mpi_queue* queue, const mpi_message::ptr& msg,
                  mpi_queue_recv_request* req);

  /**
   * @brief incoming_payload  When the payload form #send_header or RDMA
   * arrives at destination, perform operations needed to process the payload.
   * May perform extra operations if recv has already been posted (i.e. req
   * is non-null). For all protocols except eager1_doublecpy,
   * this function is only called after the recv has been posted
   * @param queue The queue the header arrived at
   * @param msg The header
   */
  virtual void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);


  /**
   * @brief incoming_payload  When the payload form #send_header or RDMA
   * arrives at destination, perform operations needed to process the payload.
   * May perform extra operations if recv has already been posted (i.e. req
   * is non-null). For all protocols except eager1_doublecpy,
   * this function is only called after the recv has been posted
   * @param queue The queue the header arrived at
   * @param msg The header
   * @param req A descriptor for the recv. If non-null,
   *            the recv has already been posted.
   */
  virtual void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg,
                   mpi_queue_recv_request* req);

  /**
   * @brief configure_send_buffer Depending on protocol,
   * special processing (copies, rdma pinning) might need to happen.
   * @param msg   The message to be sent
   * @param buffer The buffer corresponding to the send
   */
  virtual void configure_send_buffer(mpi_queue* queue, const mpi_message::ptr& msg,
                                     void* buffer, mpi_type* typeobj) = 0;

  virtual bool send_needs_completion_ack() const = 0;

  virtual bool send_needs_nic_ack() const = 0;

  virtual bool send_needs_eager_ack() const = 0;

  virtual PROTOCOL_ID get_prot_id() const = 0;

  virtual ~mpi_protocol(){}

  static mpi_protocol* eager0_protocol;
  static mpi_protocol* eager1_singlecpy_protocol;
  static mpi_protocol* eager1_doublecpy_protocol;
  static mpi_protocol* rendezvous_protocol;

  static mpi_protocol* get_protocol_object(PROTOCOL_ID id);

  static void delete_statics();

 protected:
  void* fill_send_buffer(const mpi_message::ptr &msg,
                         void *buffer, mpi_type *typeobj);
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
  eager0(sprockit::sim_parameters* params){}

  ~eager0(){}

  std::string to_string() const override {
    return "eager0";
  }

  bool send_needs_completion_ack() const override {
    return false;
  }

  bool send_needs_nic_ack() const override {
    return true;
  }

  bool send_needs_eager_ack() const override {
    return true;
  }

  void configure_send_buffer(mpi_queue* queue, const mpi_message::ptr& msg,
                             void* buffer, mpi_type* typeobj) override;

  void send_header(mpi_queue* queue, const mpi_message::ptr& msg) override;

  void incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg) override;

  void incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg,
                   mpi_queue_recv_request* req) override;

  PROTOCOL_ID get_prot_id() const override {
    return EAGER0;
  }

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
class eager1 : public mpi_protocol
{
 public:
  eager1(sprockit::sim_parameters* params){}

  virtual ~eager1(){}

  std::string to_string() const override {
    return "eager1";
  }

  bool send_needs_completion_ack() const override {
    return false;
  }

  void configure_send_buffer(mpi_queue* queue, const mpi_message::ptr& msg,
                             void* buffer, mpi_type* typeobj) override;

  bool send_needs_nic_ack() const override {
    return false;
  }

  bool send_needs_eager_ack() const override {
    return true;
  }

  void send_header(mpi_queue* queue, const mpi_message::ptr& msg) override;

  void incoming_header(mpi_queue* queue, const mpi_message::ptr& msg) override;

  void incoming_header(mpi_queue* queue, const mpi_message::ptr& msg,
                  mpi_queue_recv_request* req) override;

};

/**
 * @brief The eager1 class
 * Eager1 optimization that eliminates recv temp buf if receive is
 * posted before RDMA header arrives from sender.
 */
class eager1_singlecpy final : public eager1
{
 public:
  eager1_singlecpy(sprockit::sim_parameters* params) :
    eager1(params)
  {
  }

  ~eager1_singlecpy(){}

  std::string to_string() const override {
    return "eager1_rdma_singlecpy";
  }

  PROTOCOL_ID get_prot_id() const override {
    return EAGER1_SINGLECPY;
  }

  void incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg) override;

  void incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg,
                   mpi_queue_recv_request* req) override;

};

/**
 * @brief The eager1 class
 * Standard eager1 protocol uses temp bufs for both send/recv
 */
class eager1_doublecpy final : public eager1
{
 public:
  eager1_doublecpy(sprockit::sim_parameters* params) :
    eager1(params)
  {
  }

  virtual ~eager1_doublecpy(){}

  std::string to_string() const override {
    return "eager1_doublecpy";
  }

  PROTOCOL_ID get_prot_id() const override {
    return EAGER1_DOUBLECPY;
  }

  void incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg) override;

  void incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg,
                   mpi_queue_recv_request* req) override;

};

class rendezvous_protocol : public mpi_protocol
{
 public:
  rendezvous_protocol(sprockit::sim_parameters* params);

  std::string to_string() const override {
    return "rendezvous";
  }

  virtual ~rendezvous_protocol(){}

 protected:
  sstmac::timestamp rdma_pin_delay_;
  bool software_ack_;

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
  rendezvous_get(sprockit::sim_parameters* params) :
    rendezvous_protocol(params)
  {
  }

  ~rendezvous_get();

  bool send_needs_nic_ack() const override {
    return !software_ack_;
  }

  bool send_needs_eager_ack() const override {
    return false;
  }

  bool send_needs_completion_ack() const override {
    return software_ack_;
  }

  void send_header(mpi_queue* queue, const mpi_message::ptr& msg) override;

  void incoming_header(mpi_queue* queue, const mpi_message::ptr& msg) override;

  void incoming_header(mpi_queue* queue, const mpi_message::ptr& msg,
                   mpi_queue_recv_request* req) override;

  void incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg) override;

  std::string to_string() const override {
    return "rendezvous protocol rdma";
  }

  PROTOCOL_ID get_prot_id() const override {
    return RENDEZVOUS_GET;
  }

  void configure_send_buffer(mpi_queue* queue, const mpi_message::ptr& msg,
                             void* buffer, mpi_type* typeobj) override;

};

}

#endif // MPIPROTOCOL_H