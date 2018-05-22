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

#ifndef sumi_api_MESSAGE_H
#define sumi_api_MESSAGE_H

#include <memory>
#include <sprockit/util.h>
#include <sprockit/printable.h>
#include <sumi/rdma.h>
#include <sstmac/common/serializable.h>
#include <sstmac/common/sstmac_config.h>

START_SERIALIZATION_NAMESPACE
template <>
class serialize<sumi::public_buffer>
{
 public:
  void operator()(sumi::public_buffer& buf, serializer& ser){
    ser.primitive(buf);
  }
};
END_SERIALIZATION_NAMESPACE

namespace sumi {

class message :
  public sprockit::printable,
  public sstmac::serializable
{
 ImplementSerializable(message)

 public:
  virtual std::string to_string() const override;

  static const int no_ack = -1;
  static const int default_cq = 0;

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
    ping,
    terminate,
    pt2pt,
    bcast,
    collective,
    collective_done,
    no_class,
    fake
 } class_t;

 public:
  static const int ack_size;
  static const int header_size;

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
    send_cq_(-1),
    recv_cq_(-1),
    owns_local_buffer_(false),
    owns_remote_buffer_(false)
  #if SSTMAC_COMM_SYNC_STATS
    ,sent_(-1),
    header_arrived_(-1),
    payload_arrived_(-1),
    synced_(-1)
#endif
  {
  }

  virtual ~message();

  static const char* tostr(payload_type_t ty);

  static const char* tostr(class_t ty);

  payload_type_t payload_type() const {
    return payload_type_;
  }

  bool is_nic_ack() const;

  virtual void serialize_order(sstmac::serializer &ser) override;

  void serialize_buffers(sstmac::serializer& ser);

  void set_payload_type(payload_type_t ty) {
    payload_type_ = ty;
  }

  virtual message* clone(payload_type_t ty) const;

  message* clone_ack() const;

  virtual void write_sync_value(){}

  void set_owns_local_buffer(bool flag){
    owns_local_buffer_ = flag;
  }

  void set_owns_remote_buffer(bool flag){
    owns_remote_buffer_ = flag;
  }

  void buffer_remote();

  void buffer_local();

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

  int send_cq() const {
    return send_cq_;
  }

  int recv_cq() const {
    return recv_cq_;
  }

  uint64_t byte_length() const {
    return num_bytes_;
  }

  int cq_id() const {
    switch (payload_type_){
     case header:
     case eager_payload:
     case software_ack:
     case rdma_get:
     case rdma_put:
     case nvram_get:
     case failure:
     case rdma_get_nack:
     case none: //annoying for now - this is collectives
      return recv_cq_;
     case eager_payload_ack:
     case rdma_put_ack:
     case rdma_get_ack:
      return send_cq_;
    }
  }

  void set_byte_length(uint64_t bytes) {
    num_bytes_ = bytes;
  }

  virtual void reverse();

  bool needs_send_ack() const {
    return send_cq_ >= 0;
  }

  void set_send_cq(int cq){
    send_cq_ = cq;
  }

  bool needs_recv_ack() const {
    return recv_cq_ >= 0;
  }

  void set_recv_cq(int cq) {
    recv_cq_ = cq;
  }

  bool has_payload() const {
    return local_buffer_.ptr || remote_buffer_.ptr;
  }

  /**
   * @brief inject_remote_to_local
   * Coming off the NIC, copy data into the waiting buffer to
   * complete a get operation: remote->local
   */
  void inject_remote_to_local();

  /**
   * @brief inject_local_to_remote
   * Comming off the NIC, copy data into the waiting buffer to
   * complete a put operation: local->remote
   */
  void inject_local_to_remote();

  void memmove_remote_to_local();

  void memmove_local_to_remote();

  sumi::public_buffer& local_buffer() { return local_buffer_; }
  sumi::public_buffer& remote_buffer() { return remote_buffer_; }

  const sumi::public_buffer& local_buffer() const { return local_buffer_; }
  const sumi::public_buffer& remote_buffer() const { return remote_buffer_; }

  void*& eager_buffer() {
   return local_buffer_.ptr;
  }

 protected:
  void clone_into(message* cln) const;

  static void buffer_send(public_buffer& buf, long num_bytes);

 protected:
  uint64_t num_bytes_;
  sumi::public_buffer local_buffer_;
  sumi::public_buffer remote_buffer_;

 private:
  payload_type_t payload_type_;

  class_t class_;

  int sender_;

  int recver_;

  int send_cq_;

  int recv_cq_;

  bool owns_local_buffer_;
  bool owns_remote_buffer_;

#if SSTMAC_COMM_SYNC_STATS
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

  void set_time_sent(double now){
    if (sent_ < 0){
      //if already set, don't overwrite
      sent_ = now;
    }
  }

  void set_time_arrived(double now){
    if (header_arrived_ < 0){
      header_arrived_ = now;
    } else {
      payload_arrived_ = now;
    }
  }

  void set_time_synced(double now){
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

  void serialize_order(sstmac::serializer& ser) override;

  action_t action() const {
    return action_;
  }

 private:
  int root_;
  action_t action_;
};

/**
* @brief The transport_message class
* Base class for anything that carries a sumi message as a payload
*/
class transport_message {
 public:
  sumi::message* take_payload() {
    auto ret = payload_;
    payload_ = nullptr;
    return ret;
  }

  virtual ~transport_message(){
    if (payload_) delete payload_;
  }

 protected:
  transport_message(sumi::message* pload) :
    payload_(pload) {}

  transport_message(){} //for serialization

  sumi::message* payload_;
};

}

#endif // SIMPLE_MESSAGE_H
