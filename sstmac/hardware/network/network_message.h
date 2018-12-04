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

class network_message : public flow
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
  network_message(
   uint64_t flow_id,
   const std::string& libname,
   sw::app_id aid,
   node_id to,
   node_id from,
   uint64_t size,
   bool needs_ack,
   void* buf,
   header ctor_tag) :
    network_message(flow_id, libname, aid, to, from,
                    size, 0, needs_ack, nullptr, nullptr, buf,
                    payload)
  {
  }

  network_message(
   uint64_t flow_id,
   const std::string& libname,
   sw::app_id aid,
   node_id to,
   node_id from,
   uint64_t payload_size,
   bool needs_ack,
   void* local_buf,
   void* remote_buf,
   rdma_get ctor_tag) :
    network_message(flow_id, libname, aid, to, from,
                    64/*default to 64 bytes for now*/,
                    payload_size, needs_ack, local_buf, remote_buf, nullptr,
                    rdma_get_request)
  {
  }

  network_message(
   uint64_t flow_id,
   const std::string& libname,
   sw::app_id aid,
   node_id to,
   node_id from,
   uint64_t payload_size,
   bool needs_ack,
   void* local_buf,
   void* remote_buf,
   rdma_put ctor_tag) :
    network_message(flow_id, libname, aid, to, from,
                    payload_size, payload_size, needs_ack, local_buf, remote_buf, nullptr,
                    rdma_put_payload)
  {
  }



  virtual std::string to_string() const override {
    return "network message";
  }

  virtual ~network_message();

  static const char* tostr(nic_event_t mut);

  static const char* tostr(type_t ty);

  const char* type_str() const {
    return tostr(type_);
  }

  bool is_metadata() const;

  virtual network_message* clone_injection_ack() const = 0;

  void nic_reverse(type_t newtype);

  bool is_nic_ack() const;

  uint64_t payload_bytes() const {
    return payload_bytes_;
  }

  node_id toaddr() const {
    return toaddr_;
  }

  node_id fromaddr() const {
    return fromaddr_;
  }

  void setup_smsg(void* buf, uint64_t sz){
    payload_bytes_ = 0;
    set_flow_size(sz);
    smsg_buffer_ = buf;
    type_ = payload;
  }

  void setup_rdma_put(void* local_buf, void* remote_buf, uint64_t sz){
    type_ = rdma_put_payload;
    local_buffer_ = local_buf;
    remote_buffer_ = remote_buf;
    payload_bytes_ = sz;
    set_flow_size(sz);
  }

  void setup_rdma_get(void* local_buf, void* remote_buf, uint64_t sz){
    type_ = rdma_get_request;
    local_buffer_ = local_buf;
    remote_buffer_ = remote_buf;
    payload_bytes_ = sz;
  }

  void set_type(type_t ty){
    type_ = ty;
  }

  void put_on_wire();

  void take_off_wire();

  void intranode_memmove();

  void memmove_remote_to_local();

  void memmove_local_to_remote();

  void* local_buffer() const { return local_buffer_; }

  void* remote_buffer() const { return remote_buffer_; }

  void* smsg_buffer() const { return smsg_buffer_; }

  void* wire_buffer() const { return wire_buffer_; }

  bool needs_ack() const {
    //only paylods get acked
    return needs_ack_ && type_ >= payload;
  }

  void set_needs_ack(bool flag){
    needs_ack_ = flag;
  }

  virtual void serialize_order(serializer& ser) override;

  sw::app_id aid() const {
    return aid_;
  }

  type_t type() const {
    return type_;
  }

  void reverse();

 protected:
  //void clone_into(network_message* cln) const;

  void convert_to_ack();

  network_message() : //for serialization
   flow(-1, 0),
   needs_ack_(true),
   payload_bytes_(0),
   type_(null_netmsg_type)
  {
  }

 private:
  network_message(
   uint64_t flow_id,
   const std::string& libname,
   sw::app_id aid,
   node_id to,
   node_id from,
   uint64_t size,
   uint64_t payload_bytes,
   bool needs_ack,
   void* local_buf,
   void* remote_buf,
   void* smsg_buf,
   type_t ty) :
    flow(flow_id, size, libname),
    smsg_buffer_(smsg_buf),
    local_buffer_(local_buf),
    remote_buffer_(remote_buf),
    wire_buffer_(nullptr),
    aid_(aid),
    needs_ack_(needs_ack),
    payload_bytes_(payload_bytes),
    toaddr_(to),
    fromaddr_(from),
    type_(ty)
  {
  }

  void put_buffer_on_wire(void* buf, uint64_t sz);

  void take_buffer_off_wire(void* buf, uint64_t sz);

  void* smsg_buffer_;

  void* local_buffer_;

  void* remote_buffer_;

  /**
   * @brief wire_buffer Represents a payload injected on the wire
 */
  void* wire_buffer_;

  uint64_t flow_id_;

  sw::app_id aid_;

  bool needs_ack_;

  uint64_t payload_bytes_;

  node_id toaddr_;

  node_id fromaddr_;

  type_t type_;

};

}
}
#endif // NETWORK_MESSAGE_H
