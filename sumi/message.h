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
#include <sstmac/common/timestamp.h>
#include <sumi/message.h>
#include <sprockit/thread_safe_new.h>


namespace sumi {

class Message : public sstmac::hw::NetworkMessage
{
 ImplementSerializable(Message)

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
    no_class,
    fake
 } class_t;

 public:
  static const int ack_size;
  static const int header_size;

  template <class... Args>
  Message(int sender, int recver, int send_cq, int recv_cq, class_t cls,
          Args&&... args) :
   sstmac::hw::NetworkMessage(std::forward<Args>(args)...),
    class_(cls),
    sender_(sender),
    recver_(recver),
    send_cq_(send_cq),
    recv_cq_(recv_cq)
  {
  }

  virtual ~Message();

  static const char* tostr(class_t ty);

  virtual std::string toString() const override;

  virtual void serialize_order(sstmac::serializer &ser) override;

#if !SSTMAC_INTEGRATED_SST_CORE
  void validate_serialization(serializable *ser) override;
#endif

  void serializeBuffers(sstmac::serializer& ser);

  static bool needsAck(sstmac::hw::NetworkMessage::type_t ty,
                        int sendCQ, int recvCQ);

  virtual sstmac::hw::NetworkMessage* cloneInjectionAck() const override {
    auto* cln = new Message(*this);
    cln->convertToAck();
    return cln;
  }

  void convertToAck(){
    sstmac::hw::NetworkMessage::convertToAck();
  }

  virtual void writeSyncValue(){}

  class_t classType() const {
    return class_;
  }

  void setClassType(class_t cls) {
    class_ = cls;
  }

  int recver() const {
    return recver_;
  }

  int sender() const {
    return sender_;
  }

  int sendCQ() const {
    return send_cq_;
  }

  int recvCQ() const {
    return recv_cq_;
  }

  int targetRank() const {
    switch (NetworkMessage::type()){
     case NetworkMessage::payload:
     case NetworkMessage::rdma_get_payload:
     case NetworkMessage::rdma_put_payload:
     case NetworkMessage::rdma_get_nack:
      return recver_;
     case NetworkMessage::payload_sent_ack:
     case NetworkMessage::rdma_get_sent_ack:
     case NetworkMessage::rdma_put_sent_ack:
      return sender_;
     default:
      spkt_abort_printf("Bad payload type %d to CQ id", NetworkMessage::type());
      return -1;
    }
  }

  int cqId() const {
    switch (NetworkMessage::type()){
     case NetworkMessage::payload:
     case NetworkMessage::rdma_get_payload:
     case NetworkMessage::rdma_put_payload:
     case NetworkMessage::rdma_get_nack:
      return recv_cq_;
     case NetworkMessage::payload_sent_ack:
     case NetworkMessage::rdma_get_sent_ack:
     case NetworkMessage::rdma_put_sent_ack:
      return send_cq_;
     default:
      spkt_abort_printf("Bad payload type %d to CQ id", NetworkMessage::type());
      return -1;
    }
  }

  void reverse() {
    std::swap(sender_, recver_);
  }

  bool needsSendAck() const {
    return send_cq_ >= 0;
  }

  void setSendCq(int cq){
    send_cq_ = cq;
    setNeedsAck(cq != Message::no_ack);
  }

  bool needsRecvAck() const {
    return recv_cq_ >= 0;
  }

  void setRecvCQ(int cq) {
    recv_cq_ = cq;
  }

  size_t hash() const;

 protected:
  //void clone_into(message* cln) const;
  Message(){} //for serialization only

 private:
  class_t class_;
  int sender_;
  int recver_;

  int send_cq_;
  int recv_cq_;

#if SSTMAC_COMM_DELAY_STATS
 public:
  sstmac::Timestamp timeSent() const {
    return sent_;
  }

  sstmac::Timestamp timeArrived() const {
    return arrived_;
  }

  void setTimeSent(sstmac::Timestamp now){
    sent_ = now;
  }

  void setTimeArrived(sstmac::Timestamp now){
    arrived_ = now;
  }

 private:
  sstmac::Timestamp sent_;

  sstmac::Timestamp arrived_;
#endif

#if SSTMAC_COMM_SYNC_STATS
 public:
  sstmac::Timestamp timeStarted() const {
    return started_;
  }

  sstmac::Timestamp timeSynced() const {
    return synced_;
  }

  sstmac::Timestamp timeSyncArrived() const {
    return sync_arrived_;
  }

  void setTimeStarted(sstmac::Timestamp now){
    started_ = now;
  }

  void setTimeSynced(sstmac::Timestamp now){
    synced_ = now;
    sync_arrived_ = arrived_;
  }

 private:
  sstmac::Timestamp started_;

  sstmac::Timestamp synced_;

  sstmac::Timestamp sync_arrived_;
#endif

};

class ProtocolMessage : public Message {
 public:
  uint64_t count() const {
    return count_;
  }

  int typeSize() const {
    return type_size_;
  }

  void* partnerBuffer() const {
    return partner_buffer_;
  }

  uint64_t payloadSize() const {
    return count_ * type_size_;
  }

  int protocol() const {
    return protocol_;
  }

  int stage() const {
    return stage_;
  }

  void advanceStage() {
    stage_++;
  }

 protected:
  template <class... Args>
  ProtocolMessage(uint64_t count, int type_size, void* partner_buffer, int protocol,
                   Args&&... args) :
    Message(std::forward<Args>(args)...),
    count_(count), type_size_(type_size),
    partner_buffer_(partner_buffer),
    stage_(0),
    protocol_(protocol)
  {
  }

  virtual void serialize_order(sstmac::serializer& ser) override {
    ser & stage_;
    ser & protocol_;
    ser & count_;
    ser & type_size_;
    ser.primitive(partner_buffer_);
    Message::serialize_order(ser);
  }

#if !SSTMAC_INTEGRATED_SST_CORE
  void validate_serialization(serializable *ser) override;
#endif

 protected:
  ProtocolMessage(){}

 private:
  uint64_t count_;
  int type_size_;
  void* partner_buffer_;
  int stage_;
  int protocol_;

};

}

#endif // SIMPLE_MESSAGE_H
