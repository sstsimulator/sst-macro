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
#include <sstmac/common/serializable.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/common/messages/library_message.h>
#include <sstmac/common/sstmac_config.h>
#include <sumi/message.h>
#include <sprockit/thread_safe_new.h>


namespace sumi {

class message :
  public sprockit::printable,
  public sstmac::serializable
{
 ImplementSerializable(message)

 public:
  static void* offset_ptr(void* in, int offset){
    if (in) return ((char*)in) + offset;
    else return nullptr;
  }

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

  message(uint64_t num_bytes) :
    message(-1,-1,num_bytes)
  {
  }

  message(class_t cls) :
    message(-1,-1,sizeof(message),cls,none)
  {
  }

  message(uint64_t num_bytes, class_t cls) :
    message(-1,-1,num_bytes,cls,none)
  {
  }

  message(int sender,
          int recver,
          uint64_t num_bytes) :
    message(sender, recver, num_bytes, pt2pt, none)
  {
  }

  message(int sender,
          int recver,
          uint64_t num_bytes,
          class_t cls,
          payload_type_t pty) :
    local_buffer_(nullptr),
    remote_buffer_(nullptr),
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

  void set_byte_length(uint64_t bytes) {
    num_bytes_ = bytes;
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

  /**
   * Update the remote buffer to simulate injecting packets
   * such that the original remote buffer is free to use again
   * in the application
   */
  virtual void put_remote_on_wire();

  /**
   * Update the local buffer to simulate injecting packets
   * such that the original local buffer is free to use again
   * in the application
   */
  virtual void put_local_on_wire();

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
     default:
      spkt_abort_printf("Bad payload type %d to CQ id", payload_type_);
      return -1;
    }
  }

  void set_local_buffer(void* buf) {
    local_buffer_ = buf;
  }

  void set_remote_buffer(void* buf) {
    remote_buffer_ = buf;
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
    return local_buffer_ || remote_buffer_;
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

  void* local_buffer() const { return local_buffer_; }
  void* remote_buffer() const { return remote_buffer_; }

 protected:
  void clone_into(message* cln) const;

  /**
   * @brief buffer
   * @param src
   * @param num_bytes
   * @return A new buffer containing the contents of the input buffer
   */
  static void* buffer(void* src, uint64_t num_bytes);

 protected:
  void* local_buffer_;
  void* remote_buffer_;
  uint64_t num_bytes_;

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

class transport_message :
  public ::sstmac::hw::network_message,
  public ::sstmac::library_interface,
  public sprockit::thread_safe_new<transport_message>
{
   ImplementSerializable(transport_message)

 public:
  transport_message(
     const std::string& libname,
     sstmac::sw::app_id aid,
     sumi::message* msg,
     uint64_t byte_length)
   : network_message(aid, byte_length),
   library_interface(libname),
   payload_(msg)
  {
  }

  void serialize_order(sstmac::serializer& ser) override;

  sumi::message* take_payload() {
    auto ret = payload_;
    payload_ = nullptr;
    return ret;
  }

  sumi::message* get_payload() const {
    return payload_;
  }

  std::string to_string() const override;

  int dest_rank() const {
    return dest_;
  }

  void set_dest_rank(int dest) {
    dest_ = dest;
  }

  int src_rank() const {
    return src_;
  }

  void set_src_rank(int src) {
    src_ = src;
  }

  void set_apps(int src, int dst){
    src_app_ = src;
    dest_app_ = dst;
  }

  int src_app() const {
    return src_app_;
  }

  int dest_app() const {
    return dest_app_;
  }

  virtual void put_on_wire() override;
  virtual void take_off_wire() override;
  virtual void intranode_memmove() override;

  ::sstmac::hw::network_message* clone_injection_ack() const override;

  virtual ~transport_message(){
    if (payload_) delete payload_;
  }

  void clone_into(transport_message* cln) const;

  void reverse() override;

 private:
  transport_message(){} //for serialization

  sumi::message* payload_;

  int src_;
  int dest_;
  int src_app_;
  int dest_app_;

};

}

#endif // SIMPLE_MESSAGE_H
