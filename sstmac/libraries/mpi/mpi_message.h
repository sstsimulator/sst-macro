/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIMESSAGE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIMESSAGE_H_INCLUDED

#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/common/messages/library_message.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm_id.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_id.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_tag.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/common/messages/payload.h>

#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol_fwd.h>

namespace sstmac {
namespace sw {

struct mpi_buffer {

  mpi_buffer() : data(0), eager(false) {}

  ~mpi_buffer(){
    if (eager && data){
      delete[] data;
    }
  }

  char* data;
  bool eager;
};

/**
 * A specialization of networkdata that contains envelope information
 * relevant to MPI messaging.
 */
class mpi_message :
  public library_interface,
  public hw::network_message,
  public sprockit::serializable_type<mpi_message>
{
  ImplementSerializableDefaultConstructor(mpi_message);

 public:
  typedef_opaque_int(id, uint64_t);

  /// The category label used to differentiate user data (which can be
  /// matched using wildcards or [later] probed using probe or iprobe)
  /// from internal communications (e.g. collective operations).
  /// The send_ready and recv_ready categories are intended for setting
  /// up rendezvous operations.  The recv_ack category is indented for
  /// completing synchronous send operations.
  typedef enum {
    user = 0, collective, onesided
  } category;

  typedef enum {
    null_content, data, header, eager_payload, rendezvous_ack, completion_ack, fake
  } content_type_t;

  typedef enum {
    HEADER_TO_RDMA_GET_REQ,
    HEADER_TO_RDMA_PUT_REQ
  } mpi_conversion_t;

  void
  recompute_bytes();

 public:
  /// Hello.
  mpi_message(const std::string& libn,
              int64_t envelope, int64_t mintrans, int count,
              mpi_type_id type, int type_packed_size,
              mpi_id source, mpi_id dest, mpi_tag tag,
              mpi_comm_id commid, int seqnum, mpi_message::id msgid,
              bool ssend, category cat, task_id source_task,
              task_id dest_task, app_id aid,
              mpi_protocol* protocol,
              const payload::const_ptr& content = payload::const_ptr());

  mpi_message(content_type_t content_type);

  mpi_message(const payload::const_ptr& content, content_type_t content_type);

  mpi_message();

  virtual std::string
  to_string() const;

  static const char*
  str(content_type_t content_type);

  static const char*
  str(category cat);

  /// Goodbye.
  virtual ~mpi_message() throw ();

  network_message*
  clone_injection_ack() const;

  virtual mpi_message*
  clone() const;

  /**
   * Serialize this message during parallel simulation.
   * @param ser The serializer to use
   */
  virtual void
  serialize_order(sprockit::serializer& ser);

  long
  payload_bytes() const;

  mpi_protocol*
  protocol() const;

  void
  put_on_wire();

  void
  set_protocol(mpi_protocol* protocol);

  void
  header_to_rendezvous_ack();

  void
  header_to_completion_ack();

  void
  payload_to_completion_ack();

  void
  rendezvous_ack_to_payload();

  void
  reverse();

  /// The number of elements sent.
  int
  count() const {
    return count_;
  }

  bool
  is_header() const {
    switch (content_type_){
  case rendezvous_ack:
  case completion_ack:
  case fake:
  case header:
    return true;
  default:
    return false;
    }
  }

  bool
  is_payload() const {
    switch (content_type_){
  case eager_payload:
  case data:
    return true;
  default:
    return false;
    }
  }

  /// Access the type label associted with this mesage.
  mpi_type_id
  type() const {
    return type_;
  }

  int
  type_packed_size() const {
    return type_packed_size_;
  }

  /// Access the index of the sender.
  mpi_id
  source() const {
    return source_;
  }

  /// Access the index of the target.
  mpi_id
  dest() const {
    return dest_;
  }

  app_id
  app() const {
    return aid_;
  }

  /// Access the tag associated with this message.
  mpi_tag
  tag() const {
    return tag_;
  }

  /// Access the id of the communicator that owns this message.
  mpi_comm_id
  commid() const {
    return commid_;
  }

  /// Access the sequence number for this message.
  int
  seqnum() const {
    return seqnum_;
  }

  /// A (hopfully unique) message id.
  mpi_message::id
  unique_mpi_id() const {
    return msgid_;
  }

  bool
  intranode() const {
    return fromaddr_ == toaddr_;
  }

  /// Return true if the receiver needs to acknowledge receipt.
  bool
  ssend() const {
    return ssend_;
  }

  /// Access the category label for this message.
  category
  cat() const {
    return cat_;
  }

  void
  cat(category c) {
    cat_ = c;
  }

  content_type_t
  content_type() {
    return content_type_;
  }

  void
  content_type(content_type_t ty);

  /// Access the payload associated with this message.
  const payload::const_ptr&
  content() const {
    return content_;
  }

  void
  set_content(const payload::const_ptr& p);

  void*
  buffer() const {
    return buffer_.data;
  }

  void
  set_buffer(void* buffer, bool eager) {
    buffer_.data = (char*) buffer;
    buffer_.eager = eager;
  }

  virtual void
  build_status(mpi_status* stat) const;

  void
  convert(mpi_conversion_t);

  void
  set_ignore_seqnum(bool flag);

  bool
  ignore_seqnum() const;

 protected:
  void
  clone_into(mpi_message* cln) const;

 protected:
  int64_t envelope_;
  int64_t mintrans_;
  int count_;
  mpi_type_id type_;
  int type_packed_size_;
  mpi_id source_;
  mpi_id dest_;
  mpi_tag tag_;
  mpi_comm_id commid_;
  int seqnum_;
  mpi_message::id msgid_;
  category cat_;
  content_type_t content_type_;
  bool ssend_;
  app_id aid_;
  payload::const_ptr content_;
  int protocol_;
  bool ignore_seqnum_;
  mpi_buffer buffer_;

#if SSTMAC_ENABLE_MPI_TIMELINE
 private:
  sstmac::timestamp started_;

 public:
  void
  set_start_time(sstmac::timestamp t){
    started_ = t;
  }

  sstmac::timestamp
  start_time() const {
    return started_;
  }

#endif

};

implement_opaque_int(mpi_message::id)

}
} // end of namespace sstmac.

#endif

