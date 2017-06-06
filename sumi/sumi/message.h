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

#ifndef sumi_api_MESSAGE_H
#define sumi_api_MESSAGE_H

#include <sprockit/util.h>
#include <sprockit/ptr_type.h>
#include <sprockit/printable.h>
#include <sumi/serialization.h>
#include <sumi/config.h>
#include <sumi/sumi_config.h>

START_SERIALIZATION_NAMESPACE
template <>
class serialize<sumi::public_buffer>
{
 public:
  void
  operator()(sumi::public_buffer& buf, serializer& ser){
    ser.primitive(buf);
  }
};
END_SERIALIZATION_NAMESPACE

namespace sumi {



class message :
  public sprockit::ptr_type,
  public sprockit::printable,
  public sumi::serializable
{
 ImplementSerializable(message)

 public:
  virtual std::string to_string() const override;

  typedef enum {
    header,
    eager_payload,
    eager_payload_ack,
    software_ack,
    nvram_get,
    rdma_put,
    rdma_put_ack,
    rdma_get,
    rdma_get_ack,
    rdma_get_nack,
    failure,
    none
  } payload_type_t;

 typedef enum {
    terminate,
    pt2pt,
    bcast,
    unexpected,
    collective,
    collective_done,
    ping,
    no_class,
    fake
 } class_t;

 public:
  static const int ack_size;
  static const int header_size;

  typedef sprockit::refcount_ptr<message> ptr;

  message() :
    message(sizeof(message))
  {
  }

  message(long num_bytes) :
    message(-1,-1,num_bytes)
  {
  }

  message(class_t cls) :
    message(-1,-1,sizeof(message),cls,none)
  {
  }

  message(long num_bytes, class_t cls) :
    message(-1,-1,num_bytes,cls,none)
  {
  }

  message(int sender,
          int recver,
          long num_bytes) :
    message(sender, recver, num_bytes, pt2pt, none)
  {
  }

  message(int sender,
          int recver,
          long num_bytes,
          class_t cls,
          payload_type_t pty) :
    num_bytes_(num_bytes),
    payload_type_(pty),
    class_(cls),
    sender_(sender),
    recver_(recver),
    transaction_id_(-1),
    needs_send_ack_(false),
    needs_recv_ack_(false)
#if SUMI_COMM_SYNC_STATS
    ,sent_(-1),
    header_arrived_(-1),
    payload_arrived_(-1),
    synced_(-1)
#endif
  {
  }

  static const char* tostr(payload_type_t ty);

  static const char* tostr(class_t ty);

  payload_type_t payload_type() const {
    return payload_type_;
  }

  bool is_nic_ack() const;

  virtual void serialize_order(sumi::serializer &ser) override;

  void set_payload_type(payload_type_t ty) {
    payload_type_ = ty;
  }

  virtual message* clone() const;

  virtual void buffer_send();

  message* clone_ack() const;

  message* clone_msg() const {
    return clone();
  }

  class_t class_type() const {
    return class_;
  }

  void set_class_type(class_t cls) {
    class_ = cls;
  }

  int recver() const {
    return recver_;
  }

  void set_recver(int dst) {
    recver_ = dst;
  }

  int sender() const {
    return sender_;
  }

  void set_sender(int src) {
    sender_ = src;
  }

  long byte_length() const {
    return num_bytes_;
  }

  void set_byte_length(long bytes) {
    num_bytes_ = bytes;
  }

  int transaction_id() const {
    return transaction_id_;
  }

  void set_transaction_id(int tid) {
    transaction_id_ = tid;
  }

  bool has_transaction_id() const {
    return transaction_id_ >= 0;
  }

  virtual void reverse();

  bool needs_send_ack() const {
    return needs_send_ack_;
  }

  void set_needs_send_ack(bool need) {
    needs_send_ack_ = need;
  }

  bool needs_recv_ack() const {
    return needs_recv_ack_;
  }

  void set_needs_recv_ack(bool need) {
    needs_recv_ack_ = need;
  }

  bool has_payload() const {
    return local_buffer_.ptr || remote_buffer_.ptr;
  }

  virtual void move_remote_to_local();

  virtual void move_local_to_remote();

  sumi::public_buffer& local_buffer() { return local_buffer_; }
  sumi::public_buffer& remote_buffer() { return remote_buffer_; }

  void*& eager_buffer() {
   return local_buffer_.ptr;
  }

 protected:
  void clone_into(message* cln) const;

  static void buffer_send(public_buffer& buf, long num_bytes);

 protected:
  long num_bytes_;
  sumi::public_buffer local_buffer_;
  sumi::public_buffer remote_buffer_;

 private:
  payload_type_t payload_type_;

  class_t class_;

  int sender_;

  int recver_;

  int transaction_id_;

  bool needs_send_ack_;

  bool needs_recv_ack_;

#if SUMI_COMM_SYNC_STATS
 public:
  double time_sent() const {
    return sent_;
  }

  double time_header_arrived() const {
    return header_arrived_;
  }

  double time_payload_arrived() const {
    return payload_arrived_;
  }

  double time_synced() const {
    return synced_;
  }

  void
  set_time_sent(double now){
    if (sent_ < 0){
      //if already set, don't overwrite
      sent_ = now;
    }
  }

  void
  set_time_arrived(double now){
    if (header_arrived_ < 0){
      header_arrived_ = now;
    } else {
      payload_arrived_ = now;
    }
  }

  void
  set_time_synced(double now){
    synced_ = now;
  }

 private:
  double sent_;

  double header_arrived_;

  double payload_arrived_;

  double synced_;
#endif
};

class system_bcast_message : public message
{
  ImplementSerializable(system_bcast_message)
 public:
  typedef sprockit::refcount_ptr<system_bcast_message> ptr;

  typedef enum {
    shutdown
  } action_t;

  system_bcast_message(action_t action, int root) :
    message(bcast),
    root_(root),
    action_(action)
  {
  }

  system_bcast_message(){} //serialization

  int root() const {
    return root_;
  }

  void serialize_order(serializer& ser) override;

  action_t action() const {
    return action_;
  }

 private:
  int root_;
  action_t action_;
};

}

#endif // SIMPLE_MESSAGE_H