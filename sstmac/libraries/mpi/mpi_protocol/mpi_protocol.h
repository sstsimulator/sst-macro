#ifndef sstmac_software_libraries_mpi_MPIPROTOCOL_H
#define sstmac_software_libraries_mpi_MPIPROTOCOL_H

#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_fwd.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_send_request_fwd.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_recv_request_fwd.h>
#include <sstmac/libraries/mpi/mpi_message.h>
#include <sstmac/common/sst_event.h>

namespace sstmac {
namespace sw {

class mpi_protocol  {

 public:
  enum PROTOCOL_ID {
    PROTOCOL_INVALID,
    EAGER_SSEND,
    EAGER0_MMAP,
    EAGER0_SOCKET,
    EAGER0_RDMA,
    EAGER1_RDMA_SINGLECPY,
    EAGER1_RDMA_DOUBLECPY,
    RVOUS_MMAP,
    RVOUS_SOCKET,
    RVOUS_RDMA,
    RVOUS_RMA
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
  send_needs_rendezvous_ack() const = 0;

  virtual bool
  send_needs_nic_ack() const = 0;

  virtual bool
  send_needs_eager_ack() const = 0;

  virtual PROTOCOL_ID
  get_prot_id() const = 0;

  virtual void
  handle_nic_ack(mpi_queue* queue, const mpi_message::ptr& msg);

  virtual ~mpi_protocol(){}

  static mpi_protocol* eager_ssend_protocol;
  static mpi_protocol* eager0_mmap_protocol;
  static mpi_protocol* eager0_socket_protocol;
  static mpi_protocol* eager0_rdma_protocol;
  static mpi_protocol* eager1_rdma_singlecpy_protocol;
  static mpi_protocol* eager1_rdma_doublecpy_protocol;
  static mpi_protocol* rendezvous_mmap_protocol;
  static mpi_protocol* rendezvous_socket_protocol;
  static mpi_protocol* rendezvous_rdma_protocol;
  static mpi_protocol* rendezvous_rma_protocol;

  static mpi_protocol*
  get_protocol_object(PROTOCOL_ID id);

  static void
  delete_statics();

};


class eager_ssend : public mpi_protocol
{
 public:
  bool
  handshake_only() const;

  bool
  send_needs_completion_ack() const;

  bool
  send_needs_rendezvous_ack() const;

  bool
  send_needs_nic_ack() const;

  bool
  send_needs_eager_ack() const;

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

  void
  configure_send_buffer(const mpi_message::ptr &msg, void *buffer){
    msg->set_buffer(buffer, false); //not eager
  }

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  std::string
  to_string() const {
    return "eager ssend";
  }

  virtual PROTOCOL_ID
  get_prot_id() const {
    return EAGER_SSEND;
  }
};

class eager_protocol : public mpi_protocol
{
 public:
  void
  configure_send_buffer(const mpi_message::ptr& msg, void* buffer);
};

class eager0_protocol : public eager_protocol
{
 public:

  virtual ~eager0_protocol(){}

  bool
  handshake_only() const;

  bool
  send_needs_completion_ack() const;

  bool
  send_needs_rendezvous_ack() const;

  bool
  send_needs_nic_ack() const;

  bool
  send_needs_eager_ack() const;

  void
  finish_recv_header(mpi_queue* queue, const mpi_message::ptr& msg,
              mpi_queue_recv_request* req);
  void
  finish_recv_payload(mpi_queue* queue, const mpi_message::ptr& msg,
               mpi_queue_recv_request* req);
  void
  incoming_header(mpi_queue* queue, const mpi_message::ptr& msg);

};

class eager0_mmap : public eager0_protocol
{
 public:

  void
  send_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  std::string
  to_string() const {
    return "eager0_mmap";
  }

  virtual PROTOCOL_ID
  get_prot_id() const {
    return EAGER0_MMAP;
  }

  virtual ~eager0_mmap();

};

class eager0_socket : public eager0_protocol
{
 public:

  virtual ~eager0_socket();

  void
  send_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  std::string
  to_string() const {
    return "eager0_socket";
  }

  virtual PROTOCOL_ID
  get_prot_id() const {
    return EAGER0_SOCKET;
  }
};

class eager0_rdma : public eager0_protocol
{
 public:

  virtual ~eager0_rdma();

  void
  send_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  std::string
  to_string() const {
    return "eager0_rdma";
  }

  virtual PROTOCOL_ID
  get_prot_id() const {
    return EAGER0_RDMA;
  }
};

class eager1_rdma : public eager_protocol
{
 public:

  virtual ~eager1_rdma(){}

  bool
  handshake_only() const;

  bool
  send_needs_completion_ack() const;
  bool
  send_needs_rendezvous_ack() const;
  bool
  send_needs_nic_ack() const;
  bool
  send_needs_eager_ack() const;

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

  std::string
  to_string() const {
    return "eager1_rdma";
  }

};

class eager1_rdma_singlecpy : public eager1_rdma
{
 public:

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  virtual ~eager1_rdma_singlecpy();

  std::string
  to_string() const {
    return "eager1_rdma_singlecpy";
  }

  virtual PROTOCOL_ID
  get_prot_id() const {
    return EAGER1_RDMA_SINGLECPY;
  }
};

class eager1_rdma_doublecpy : public eager1_rdma
{
 public:

  virtual ~eager1_rdma_doublecpy();

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  std::string
  to_string() const {
    return "eager1_rdma_doublecpy";
  }

  virtual PROTOCOL_ID
  get_prot_id() const {
    return EAGER1_RDMA_DOUBLECPY;
  }
};

class rendezvous_protocol : public mpi_protocol
{
 public:
  bool
  handshake_only() const;

  virtual ~rendezvous_protocol(){}

  bool
  send_needs_nic_ack() const;

  bool
  send_needs_eager_ack() const;

  virtual void
  finish_recv_header(mpi_queue* queue, const mpi_message::ptr& msg,
              mpi_queue_recv_request* req);

  void
  configure_send_buffer(const mpi_message::ptr& msg, void* buffer){
    msg->set_buffer(buffer, false); //not eager
  }

};

class rendezvous_socket : public rendezvous_protocol
{
 public:

  virtual ~rendezvous_socket();

  bool
  send_needs_completion_ack() const;

  bool
  send_needs_rendezvous_ack() const;

  void
  send_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  finish_recv_payload(mpi_queue* queue, const mpi_message::ptr& msg,
               mpi_queue_recv_request* req);

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  std::string
  to_string() const {
    return "rendezvous protocol socket";
  }

  virtual PROTOCOL_ID
  get_prot_id() const {
    return RVOUS_SOCKET;
  }
};

class rendezvous_rdma : public rendezvous_protocol
{
 public:

  virtual ~rendezvous_rdma();

  bool
  send_needs_completion_ack() const;

  bool
  send_needs_rendezvous_ack() const;

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
    return RVOUS_RDMA;
  }

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

class rendezvous_rma : public rendezvous_protocol
{
 public:

  bool
  send_needs_completion_ack() const;

  bool
  send_needs_rendezvous_ack() const;

  void
  send_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  std::string
  to_string() const {
    return "rendezvous protocol rdma rma";
  }

  virtual PROTOCOL_ID
  get_prot_id() const {
    return RVOUS_RMA;
  }

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

class rendezvous_mmap : public rendezvous_protocol
{
 public:

  virtual ~rendezvous_mmap();

  bool
  send_needs_completion_ack() const;

  bool
  send_needs_rendezvous_ack() const;

  void
  send_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  std::string
  to_string() const {
    return "rendezvous protocol mmap";
  }

  virtual PROTOCOL_ID
  get_prot_id() const {
    return RVOUS_MMAP;
  }

  void
  finish_recv_payload(mpi_queue* queue,
               const mpi_message::ptr& msg,
               mpi_queue_recv_request* req);

};

}
}

#endif // MPIPROTOCOL_H

