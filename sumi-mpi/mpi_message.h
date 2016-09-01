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
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/common/messages/library_message.h>
#include <sumi-mpi/mpi_status.h>
#include <sumi-mpi/mpi_types/mpi_type.h>
#include <sumi-mpi/sstmac_mpi.h>
#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>

#include <sstmac/software/process/operating_system_fwd.h>
#include <sumi-mpi/mpi_protocol/mpi_protocol_fwd.h>
#include <sumi/message.h>

namespace sumi {

/**
 * A specialization of networkdata that contains envelope information
 * relevant to MPI messaging.
 */
class mpi_message :
  public sumi::message,
  public serializable_type<mpi_message>
{
  ImplementSerializableDefaultConstructor(mpi_message)

 public:
  typedef sprockit::refcount_ptr<mpi_message> ptr;
  typedef sprockit::refcount_ptr<const mpi_message> const_ptr;
  typedef uint64_t id;

  typedef enum {
    null_content, data, header, eager_payload, completion_ack, fake
  } content_type_t;

  void
  recompute_bytes();

 public:
  /// Hello.
  mpi_message(int src, int dst, int count,
              MPI_Datatype type, int type_packed_size,
              int tag,
              MPI_Comm commid, int seqnum,
              mpi_message::id msgid,
              mpi_protocol* protocol);

  mpi_message(){}

  virtual std::string
  to_string() const;

  static const char*
  str(content_type_t content_type);

  /// Goodbye.
  virtual ~mpi_message() throw ();

  virtual sumi::message*
  clone() const;

  /**
   * Serialize this message during parallel simulation.
   * @param ser The serializer to use
   */
  virtual void
  serialize_order(serializer& ser);

  long
  payload_bytes() const {
    return count_ * type_packed_size_;
  }

  mpi_protocol*
  protocol() const;

  void
  put_on_wire();

  void
  set_protocol(mpi_protocol* protocol);

  void
  payload_to_completion_ack();

  /// The number of elements sent.
  int
  count() const {
    return count_;
  }

  bool
  is_header() const {
    switch (content_type_){
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
  MPI_Datatype
  type() const {
    return type_;
  }

  int
  type_packed_size() const {
    return type_packed_size_;
  }

  /// Access the tag associated with this message.
  int
  tag() const {
    return tag_;
  }

  /// Access the id of the communicator that owns this message.
  MPI_Comm
  comm() const {
    return commid_;
  }

  /// Access the sequence number for this message.
  int
  seqnum() const {
    return seqnum_;
  }

  int
  src_rank() const {
    return src_rank_;
  }

  void
  set_src_rank(int rank) {
    src_rank_ = rank;
  }

  int
  dst_rank() const {
    return dst_rank_;
  }

  void
  set_dst_rank(int rank) {
    dst_rank_ = rank;
  }

  /// A (hopfully unique) message id.
  mpi_message::id
  unique_int() const {
    return msgid_;
  }

  content_type_t
  content_type() const {
    return content_type_;
  }

  void
  set_content_type(content_type_t ty) {
    content_type_ = ty;
    recompute_bytes();
  }

  void
  build_status(MPI_Status* stat) const;

  void
  set_in_flight(bool flag){
    in_flight_ = flag;
  }

  bool
  in_flight() const {
    return in_flight_;
  }

  virtual void
  move_remote_to_local();

  virtual void
  move_local_to_remote();

 protected:
  void
  clone_into(mpi_message* cln) const;

  virtual void
  buffer_send();

 protected:
  int src_rank_;
  int dst_rank_;
  int count_;
  MPI_Datatype type_;
  int type_packed_size_;
  int tag_;
  MPI_Comm commid_;
  int seqnum_;
  mpi_message::id msgid_;
  content_type_t content_type_;
  int protocol_;
  bool in_flight_;

};

}

#endif

