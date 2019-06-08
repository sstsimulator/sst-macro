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

#ifndef sstmac_hardware_network_NETWORK_MESSAGE_H
#define sstmac_hardware_network_NETWORK_MESSAGE_H

#include <sstmac/hardware/common/flow.h>
#include <sstmac/hardware/network/network_id.h>
#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>

namespace sstmac {
namespace hw {

class NetworkMessage : public Flow
{
 public:
  typedef enum {
    RDMA_GET_FAILED,
    RDMA_GET_REQ_TO_RSP,
    NVRAM_GET_REQ_TO_RSP
  } nic_event_t;

  struct rdma_get {};
  struct rdma_put {};
  struct header {};

  typedef enum {
    null_netmsg_type=0,
    rdma_get_request=1,
    rdma_get_sent_ack=2,
    rdma_get_nack=3,
    rdma_put_sent_ack=4,
    rdma_put_nack=5,
    payload_sent_ack=6,
    payload=7,
    rdma_get_payload=8,
    rdma_put_payload=9,
    nvram_get_request=10,
    nvram_get_payload=11,
    failure_notification=12
  } type_t;

 public:
  NetworkMessage(
   uint64_t flow_id,
   const std::string& libname,
   sw::AppId aid,
   NodeId to,
   NodeId from,
   uint64_t size,
   bool needs_ack,
   void* buf,
   header ctor_tag) :
    NetworkMessage(flow_id, libname, aid, to, from,
                    size, size, needs_ack, nullptr, nullptr, buf,
                    payload)
  {
  }

  NetworkMessage(
   uint64_t flow_id,
   const std::string& libname,
   sw::AppId aid,
   NodeId to,
   NodeId from,
   uint64_t payload_size,
   bool needs_ack,
   void* local_buf,
   void* remote_buf,
   rdma_get ctor_tag) :
    NetworkMessage(flow_id, libname, aid, to, from,
                    64/*default to 64 bytes for now*/,
                    payload_size, needs_ack, local_buf, remote_buf, nullptr,
                    rdma_get_request)
  {
  }

  NetworkMessage(
   uint64_t flow_id,
   const std::string& libname,
   sw::AppId aid,
   NodeId to,
   NodeId from,
   uint64_t payload_size,
   bool needs_ack,
   void* local_buf,
   void* remote_buf,
   rdma_put ctor_tag) :
    NetworkMessage(flow_id, libname, aid, to, from,
                    payload_size, payload_size, needs_ack, local_buf, remote_buf, nullptr,
                    rdma_put_payload)
  {
  }



  virtual std::string toString() const override {
    return "network message";
  }

  virtual ~NetworkMessage();

  static const char* tostr(nic_event_t mut);

  static const char* tostr(type_t ty);

  const char* typeStr() const {
    return tostr(type_);
  }

  bool isMetadata() const;

  virtual NetworkMessage* cloneInjectionAck() const = 0;

  void nicReverse(type_t newtype);

  bool isNicAck() const;

  uint64_t payloadBytes() const {
    return payload_bytes_;
  }

  NodeId toaddr() const {
    return toaddr_;
  }

  NodeId fromaddr() const {
    return fromaddr_;
  }

  void setupSmsg(void* buf, uint64_t sz){
    payload_bytes_ = 0;
    setFlowSize(sz);
    smsg_buffer_ = buf;
    type_ = payload;
  }

  void setupRdmaPut(void* local_buf, void* remote_buf, uint64_t sz){
    type_ = rdma_put_payload;
    local_buffer_ = local_buf;
    remote_buffer_ = remote_buf;
    payload_bytes_ = sz;
    setFlowSize(sz);
  }

  void setupRdmaGet(void* local_buf, void* remote_buf, uint64_t sz){
    type_ = rdma_get_request;
    local_buffer_ = local_buf;
    remote_buffer_ = remote_buf;
    payload_bytes_ = sz;
  }

  void setType(type_t ty){
    type_ = ty;
  }

  void putOnWire();

  void takeOffWire();

  void intranodeMemmove();

  void memmoveRemoteToLocal();

  void memmoveLocalToRemote();

  void* localBuffer() const { return local_buffer_; }

  void* remoteBuffer() const { return remote_buffer_; }

  void* smsgBuffer() const { return smsg_buffer_; }

  void* wireBuffer() const { return wire_buffer_; }

  bool needsAck() const {
    //only paylods get acked
    return needs_ack_ && type_ >= payload;
  }

  void setNeedsAck(bool flag){
    needs_ack_ = flag;
  }

  virtual void serialize_order(serializer& ser) override;

#if !SSTMAC_INTEGRATED_SST_CORE
  void validate_serialization(serializable *ser) override;
#endif

  sw::AppId aid() const {
    return aid_;
  }

  type_t type() const {
    return type_;
  }

  void reverse();

  void setQoS(int qos){
    qos_ = qos;
  }

 protected:
  //void clone_into(NetworkMessage* cln) const;

  void convertToAck();

  NetworkMessage() : //for serialization
   Flow(-1, 0),
   needs_ack_(true),
   payload_bytes_(0),
   type_(null_netmsg_type)
  {
  }

 private:
  NetworkMessage(
   uint64_t flow_id,
   const std::string& libname,
   sw::AppId aid,
   NodeId to,
   NodeId from,
   uint64_t size,
   uint64_t payload_bytes,
   bool needs_ack,
   void* local_buf,
   void* remote_buf,
   void* smsg_buf,
   type_t ty) :
    Flow(flow_id, size, libname),
    smsg_buffer_(smsg_buf),
    local_buffer_(local_buf),
    remote_buffer_(remote_buf),
    wire_buffer_(nullptr),
    aid_(aid),
    needs_ack_(needs_ack),
    payload_bytes_(payload_bytes),
    toaddr_(to),
    fromaddr_(from),
    type_(ty),
    qos_(0)
  {
  }

  void putBufferOnWire(void* buf, uint64_t sz);

  void takeBufferOffWire(void* buf, uint64_t sz);

  void* smsg_buffer_;

  void* local_buffer_;

  void* remote_buffer_;

  /**
   * @brief wire_buffer Represents a payload injected on the wire
 */
  void* wire_buffer_;

  sw::AppId aid_;

  bool needs_ack_;

  uint64_t payload_bytes_;

  NodeId toaddr_;

  NodeId fromaddr_;

  type_t type_;

  int qos_;

};

}
}
#endif // NETWORK_MESSAGE_H
