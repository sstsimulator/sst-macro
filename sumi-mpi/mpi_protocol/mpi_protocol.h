#ifndef sstmac_software_libraries_mpi_MPIPROTOCOL_H
#define sstmac_software_libraries_mpi_MPIPROTOCOL_H

#include <sumi-mpi/mpi_queue/mpi_queue_fwd.h>
#include <sumi-mpi/mpi_queue/mpi_queue_send_request_fwd.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request_fwd.h>
#include <sumi-mpi/mpi_message.h>
#include <sstmac/common/sst_event.h>

namespace sumi {

using sstmac::event;

class mpi_protocol  {

 public:
  enum PROTOCOL_ID {
    PROTOCOL_INVALID,
    EAGER0,
    EAGER1_SINGLECPY,
    EAGER1_DOUBLECPY,
    RENDEZVOUS_GET,
  };

 public:
  virtual std::string
  to_string() const = 0;

  virtual void
  send_header(mpi_queue* queue, const mpi_message::ptr& msg) = 0;

  virtual void
  incoming_header(mpi_queue* queue, const mpi_message::ptr& msg) = 0;

  virtual void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg) = 0;

  virtual void
  finish_recv_header(mpi_queue* queue, const mpi_message::ptr& msg,
              mpi_queue_recv_request* req) = 0;

  virtual void
  configure_send_buffer(const mpi_message::ptr& msg, void* buffer) = 0;

  virtual void
  finish_recv_payload(mpi_queue* queue, const mpi_message::ptr& msg,
               mpi_queue_recv_request* req) = 0;

  virtual bool
  handshake_only() const = 0;

  virtual bool
  send_needs_completion_ack() const = 0;

  virtual bool
  send_needs_nic_ack() const = 0;

  virtual bool
  send_needs_eager_ack() const = 0;

  virtual PROTOCOL_ID
  get_prot_id() const = 0;

  virtual void
  handle_nic_ack(mpi_queue* queue, const mpi_message::ptr& msg);

  virtual ~mpi_protocol(){}

  static mpi_protocol* eager0_protocol;
  static mpi_protocol* eager1_singlecpy_protocol;
  static mpi_protocol* eager1_doublecpy_protocol;
  static mpi_protocol* rendezvous_protocol;

  static mpi_protocol*
  get_protocol_object(PROTOCOL_ID id);

  static void
  delete_statics();

};


class eager_protocol : public mpi_protocol
{
};

class eager0 : public eager_protocol
{
 public:

  virtual ~eager0(){}

  bool
  handshake_only() const {
    return false;
  }

  std::string
  to_string() const {
    return "eager0";
  }

  bool
  send_needs_completion_ack() const {
    return false;
  }

  bool
  send_needs_nic_ack() const {
    return true;
  }

  bool
  send_needs_eager_ack() const {
    return true;
  }

  void
  configure_send_buffer(const mpi_message::ptr& msg, void* buffer);

  void
  finish_recv_header(mpi_queue* queue, const mpi_message::ptr& msg,
              mpi_queue_recv_request* req);
  void
  finish_recv_payload(mpi_queue* queue, const mpi_message::ptr& msg,
               mpi_queue_recv_request* req);
  void
  incoming_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  send_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  virtual PROTOCOL_ID
  get_prot_id() const {
    return EAGER0;
  }

};

class eager1 : public eager_protocol
{
 public:
  virtual ~eager1(){}

  std::string
  to_string() const {
    return "eager1";
  }

  bool
  handshake_only() const {
    return true;
  }

  bool
  send_needs_completion_ack() const {
    return false;
  }

  void
  configure_send_buffer(const mpi_message::ptr& msg, void* buffer);

  bool
  send_needs_nic_ack() const {
    return false;
  }

  bool
  send_needs_eager_ack() const {
    return true;
  }

  void
  send_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  finish_recv_header(mpi_queue* queue, const mpi_message::ptr& msg,
              mpi_queue_recv_request* req);

  void
  finish_recv_payload(mpi_queue* queue, const mpi_message::ptr& msg,
               mpi_queue_recv_request* req);

};

class eager1_singlecpy : public eager1
{
 public:

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  virtual ~eager1_singlecpy();

  std::string
  to_string() const {
    return "eager1_rdma_singlecpy";
  }

  virtual PROTOCOL_ID
  get_prot_id() const {
    return EAGER1_SINGLECPY;
  }
};

class eager1_doublecpy : public eager1
{
 public:

  virtual ~eager1_doublecpy();

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  std::string
  to_string() const {
    return "eager1_doublecpy";
  }

  virtual PROTOCOL_ID
  get_prot_id() const {
    return EAGER1_DOUBLECPY;
  }
};

class rendezvous_protocol : public mpi_protocol
{
 public:
  std::string
  to_string() const {
    return "rendezvous";
  }

  bool
  handshake_only() const {
    return true;
  }

  virtual ~rendezvous_protocol(){}


};

class rendezvous_get : public rendezvous_protocol
{
 public:

  virtual ~rendezvous_get();

  bool
  send_needs_nic_ack() const {
    return true;
  }

  bool
  send_needs_eager_ack() const {
    return false;
  }

  bool
  send_needs_completion_ack() const {
    return false;
  }

  void
  send_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  std::string
  to_string() const {
    return "rendezvous protocol rdma";
  }

  virtual PROTOCOL_ID
  get_prot_id() const {
    return RENDEZVOUS_GET;
  }

  void
  configure_send_buffer(const mpi_message::ptr& msg, void* buffer);

  void
  finish_recv_header(mpi_queue* queue, const mpi_message::ptr& msg,
              mpi_queue_recv_request* req);

  void
  finish_recv_payload(mpi_queue* queue, const mpi_message::ptr& msg,
               mpi_queue_recv_request* req);

  void
  init_system_recv(long nbytes, mpi_queue* queue,
                   event* ev);

};

}

#endif // MPIPROTOCOL_H

