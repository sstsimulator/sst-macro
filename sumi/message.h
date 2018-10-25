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
#include <sstmac/common/sstmac_config.h>
#include <sumi/message.h>
#include <sprockit/thread_safe_new.h>


namespace sumi {

class message : public sstmac::hw::network_message
{
 ImplementSerializable(message)

 public:
  static void* offset_ptr(void* in, int offset){
    if (in) return ((char*)in) + offset;
    else return nullptr;
  }

 public:
  static const int no_ack = -1;
  static const int default_cq = 0;

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

  template <class... Args>
  message(int sender, int recver, int send_cq, int recv_cq, class_t cls,
          Args&&... args) :
   sstmac::hw::network_message(std::forward<Args>(args)...),
#if SSTMAC_COMM_SYNC_STATS
    sent_(-1),
    header_arrived_(-1),
    payload_arrived_(-1),
    synced_(-1),
#endif
    class_(cls),
    sender_(sender),
    recver_(recver),
    send_cq_(send_cq),
    recv_cq_(recv_cq)
  {
  }

  virtual ~message();

  static const char* tostr(class_t ty);

  virtual std::string to_string() const override;

  virtual void serialize_order(sstmac::serializer &ser) override;

  void serialize_buffers(sstmac::serializer& ser);

  static bool needs_ack(sstmac::hw::network_message::type_t ty,
                        int send_cq, int recv_cq);

  virtual sstmac::hw::network_message* clone_injection_ack() const override {
    auto* cln = new message(*this);
    cln->convert_to_ack();
    return cln;
  }

  void convert_to_ack(){
    sstmac::hw::network_message::convert_to_ack();
  }

  virtual void write_sync_value(){}

  class_t class_type() const {
    return class_;
  }

  void set_class_type(class_t cls) {
    class_ = cls;
  }

  int recver() const {
    return recver_;
  }

  int sender() const {
    return sender_;
  }

  int send_cq() const {
    return send_cq_;
  }

  int recv_cq() const {
    return recv_cq_;
  }

  int target_rank() const {
    switch (network_message::type()){
     case network_message::payload:
     case network_message::rdma_get_payload:
     case network_message::rdma_put_payload:
     case network_message::rdma_get_nack:
      return recver_;
     case network_message::payload_sent_ack:
     case network_message::rdma_get_sent_ack:
     case network_message::rdma_put_sent_ack:
      return sender_;
     default:
      spkt_abort_printf("Bad payload type %d to CQ id", network_message::type());
      return -1;
    }
  }

  int cq_id() const {
    switch (network_message::type()){
     case network_message::payload:
     case network_message::rdma_get_payload:
     case network_message::rdma_put_payload:
     case network_message::rdma_get_nack:
      return recv_cq_;
     case network_message::payload_sent_ack:
     case network_message::rdma_get_sent_ack:
     case network_message::rdma_put_sent_ack:
      return send_cq_;
     default:
      spkt_abort_printf("Bad payload type %d to CQ id", network_message::type());
      return -1;
    }
  }

  void reverse() {
    std::swap(sender_, recver_);
  }

  bool needs_send_ack() const {
    return send_cq_ >= 0;
  }

  void set_send_cq(int cq){
    send_cq_ = cq;
    set_needs_ack(cq != message::no_ack);
  }

  bool needs_recv_ack() const {
    return recv_cq_ >= 0;
  }

  void set_recv_cq(int cq) {
    recv_cq_ = cq;
  }

 protected:
  //void clone_into(message* cln) const;
  message(){} //for serialization only

 private:
  class_t class_;
  int sender_;
  int recver_;

  int send_cq_;
  int recv_cq_;

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

class protocol_message : public message {
 public:
  uint64_t count() const {
    return count_;
  }

  int type_size() const {
    return type_size_;
  }

  void* partner_buffer() const {
    return partner_buffer_;
  }

  uint64_t payload_size() const {
    return count_ * type_size_;
  }

  int protocol() const {
    return protocol_;
  }

  int stage() const {
    return stage_;
  }

  void advance_stage() {
    stage_++;
  }

 protected:
  template <class... Args>
  protocol_message(uint64_t count, int type_size, void* partner_buffer, int protocol,
                   Args&&... args) :
    message(std::forward<Args>(args)...),
    count_(count), type_size_(type_size),
    partner_buffer_(partner_buffer),
    protocol_(protocol),
    stage_(0)
  {
  }

  virtual void serialize_order(sstmac::serializer& ser){
    ser & stage_;
    ser & protocol_;
    ser & count_;
    ser & type_size_;
    ser.primitive(partner_buffer_);
    message::serialize_order(ser);
  }

 protected:
  protocol_message(){}

 private:
  uint64_t count_;
  int type_size_;
  void* partner_buffer_;
  int stage_;
  int protocol_;

};

}

#endif // SIMPLE_MESSAGE_H
