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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPISTATUS_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPISTATUS_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_comm/mpi_comm.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_id.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_tag.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/common/messages/payload.h>


namespace sstmac {
namespace sw {

/**
 * The status of an MPI message (sent or received).
 */
class mpi_status  {

 public:
  virtual std::string
  to_string() const {
    return "mpistatus";
  }

  static mpi_status* ignore;

  /// Hello.
  mpi_status();

  /// Copy constructor.
  mpi_status(const mpi_status &m);

  /// Goodbye.
  virtual
  ~mpi_status() throw ();

  long
  count() const {
    return count_;
  }

  void
  set_count(long c) {
    count_ = c;
  }

  /// Get a copy of this status object.
  mpi_status*
  clone() const;

  /// The communicator that was used.
  mpi_comm_id
  commid() const {
    return commid_;
  }

  /// Register the communicator used.
  void
  set_commid(mpi_comm_id c) {
    commid_ = c;
  }

  /// Transmissions originated from here:
  mpi_id
  source() const {
    return source_;
  }

  /// Define the source.
  void
  set_source(mpi_id s) {
    source_ = s;
  }

  /// Transmissions ended up here:
  mpi_id
  dest() const {
    return dest_;
  }

  /// Define the destination.
  void
  set_dest(mpi_id d){
    dest_ = d;
  }

  /// This was the tag used:
  mpi_tag
  tag() const {
    return tag_;
  }

  /// Define the tag.
  void
  set_tag(mpi_tag t) {
    tag_ = t;
  }

  /// This is the type that was sent:
  mpi_type_id
  send_type() const {
    return send_type_;
  }

  /// Define the type.
  void
  set_send_type(mpi_type_id t) {
    send_type_ = t;
  }

  /// This is the type that was received.
  mpi_type_id
  recv_type() const {
    return recv_type_;
  }

  /// Define the received type.
  void
  set_recv_type(mpi_type_id t) {
    recv_type = t;
  }

  /// This is the content.
  payload::const_ptr
  content() const {
    return content_;
  }

  /// Set the content.
  void
  set_content(const payload::const_ptr& cc) {
    content_ = cc;
  }

  /// Access bytes sent.
  int64_t
  bytes_sent() const {
    return sent_;
  }

  /// Increment the number of bytes sent.
  void
  add_bytes_sent(int64_t bytes) {
    sent_ += bytes;
  }

  /// Access bytes received.
  int64_t
  bytes_received() const {
    return recvd_;
  }

  /// Increment the number of bytes received.
  void
  add_bytes_received(int64_t bytes) {
    recvd_ += bytes;
  }

 protected:
  /// The communicator involved in this communication.
  mpi_comm_id commid_;
  /// The source and destination for this communication.
  mpi_id source_, dest_;
  /// The tag that was used.
  mpi_tag tag_;
  /// The datatype that was communicated.
  mpi_type_id send_type_;
  mpi_type_id recv_type_;
  /// The content sent (may be null).
  payload::const_ptr content_;

  /// The amount of bytes I communicated.
  int64_t sent_, recvd_;

  long count_;

};

}
} // end of namespace sstmac.

#endif

