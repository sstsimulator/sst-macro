#ifndef sstmac_software_libraries_mpi_MPIPROTOCOL_H
#define sstmac_software_libraries_mpi_MPIPROTOCOL_H

#include <sumi-mpi/mpi_queue/mpi_queue_fwd.h>
#include <sumi-mpi/mpi_queue/mpi_queue_send_request_fwd.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request_fwd.h>
#include <sumi-mpi/mpi_message.h>
#include <sstmac/common/sst_event.h>

namespace sumi {

using sstmac::event;

/**
 * @brief The mpi_protocol class
 */
class mpi_protocol  {

 public:
  enum PROTOCOL_ID {
    PROTOCOL_INVALID=0,
    EAGER0=1,
    EAGER1_SINGLECPY=2,
    EAGER1_DOUBLECPY=3,
    RENDEZVOUS_GET=4
  };

 public:
  virtual std::string
  to_string() const = 0;

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
  virtual void
  configure_send_buffer(const mpi_message::ptr& msg, void* buffer) = 0;

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

/**
 * @brief The eager0 class
 * Basic eager protocol. Sender copies into a temp buf before sending.
 * MPI_Send completes immediately. Assumes dest has allocated "mailbox"
 * temp buffers to receive unexpected messages.
 */
class eager0 : public mpi_protocol
{
 public:

  virtual ~eager0(){}

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
  send_header(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg,
                   mpi_queue_recv_request* req);

  virtual PROTOCOL_ID
  get_prot_id() const {
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
  virtual ~eager1(){}

  std::string
  to_string() const {
    return "eager1";
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
  incoming_header(mpi_queue* queue, const mpi_message::ptr& msg,
                  mpi_queue_recv_request* req);

};

/**
 * @brief The eager1 class
 * Eager1 optimization that eliminates recv temp buf if receive is
 * posted before RDMA header arrives from sender.
 */
class eager1_singlecpy : public eager1
{
 public:
  virtual ~eager1_singlecpy(){}

  std::string
  to_string() const {
    return "eager1_rdma_singlecpy";
  }

  virtual PROTOCOL_ID
  get_prot_id() const {
    return EAGER1_SINGLECPY;
  }

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg,
                   mpi_queue_recv_request* req);

};

/**
 * @brief The eager1 class
 * Standard eager1 protocol uses temp bufs for both send/recv
 */
class eager1_doublecpy : public eager1
{
 public:
  virtual ~eager1_doublecpy(){}

  std::string
  to_string() const {
    return "eager1_doublecpy";
  }

  virtual PROTOCOL_ID
  get_prot_id() const {
    return EAGER1_DOUBLECPY;
  }

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg);

  void
  incoming_payload(mpi_queue* queue, const mpi_message::ptr& msg,
                   mpi_queue_recv_request* req);

};

class rendezvous_protocol : public mpi_protocol
{
 public:
  std::string
  to_string() const {
    return "rendezvous";
  }

  virtual ~rendezvous_protocol(){}
};


/**
 * @brief The rendezvous_get class
 * Encapsulates a rendezvous protocol. On MPI_Send, the source sends an RDMA header
 * to the destination. On MPI_Recv, the destination then posts an RDMA get.
 * Hardware acks arrive at both dest/source signaling done.
 */
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
  incoming_header(mpi_queue* queue, const mpi_message::ptr& msg,
                   mpi_queue_recv_request* req);

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

};

}

#endif // MPIPROTOCOL_H

