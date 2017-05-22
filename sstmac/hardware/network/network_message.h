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

#ifndef sstmac_hardware_network_NETWORK_MESSAGE_H
#define sstmac_hardware_network_NETWORK_MESSAGE_H

#include <sstmac/common/messages/sst_message.h>
#include <sstmac/hardware/router/routing_enum.h>
#include <sstmac/hardware/network/network_id.h>
#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>

namespace sstmac {
namespace hw {

class network_message :
  public message
{
  ImplementSerializable(network_message)

 public:
  typedef enum {
    RDMA_GET_FAILED,
    RDMA_GET_REQ_TO_RSP,
    NVRAM_GET_REQ_TO_RSP
  } nic_event_t;

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
   sw::app_id aid,
   node_id to,
   node_id from,
   long payload_bytes) :
    aid_(aid),
    needs_ack_(true),
    toaddr_(to),
    fromaddr_(from),
    bytes_(payload_bytes),
    type_(null_netmsg_type)
  {
  }

  network_message(node_id toaddr, node_id fromaddr, int num_bytes) :
    bytes_(num_bytes),
    toaddr_(toaddr),
    fromaddr_(fromaddr)
  {
  }

  network_message(sw::app_id aid, long payload_bytes) :
   bytes_(payload_bytes),
   needs_ack_(true),
   type_(null_netmsg_type),
   aid_(aid)
  {
  }

  network_message() //for serialization
   : needs_ack_(true),
    type_(null_netmsg_type),
    bytes_(0)
  {
  }

  virtual std::string
  to_string() const override {
    return "network message";
  }

  virtual ~network_message() {}

  static const char* tostr(nic_event_t mut);

  static const char* tostr(type_t ty);

  bool is_metadata() const;

  virtual network_message*
  clone_injection_ack() const {
    network_message* cln = new network_message;
    clone_into(cln);
    return cln;
  }

  message* clone_ack() const override {
    return clone_injection_ack();
  }

  virtual void nic_reverse(type_t newtype);

  bool is_nic_ack() const;

  node_id toaddr() const override {
    return toaddr_;
  }

  virtual void put_on_wire();

  node_id fromaddr() const override {
    return fromaddr_;
  }

  void set_toaddr(node_id addr) {
    toaddr_ = addr;
  }

  void set_fromaddr(node_id addr) {
    fromaddr_ = addr;
  }

  void set_needs_ack(bool n) {
    needs_ack_ = n;
  }

  virtual bool needs_ack() const override {
    //only paylods get acked
    return needs_ack_ && type_ >= payload;
  }

  virtual void serialize_order(serializer& ser) override;

  void convert_to_ack();

  void set_flow_id(uint64_t id) {
    flow_id_ = id;
  }

  uint64_t flow_id() const override {
    return flow_id_;
  }

  sw::app_id aid() const {
    return aid_;
  }

  type_t type() const {
    return type_;
  }

  void set_type(type_t ty){
    type_ = ty;
  }

  virtual void reverse();

  long byte_length() const override;

  node_id toaddr_;

  node_id fromaddr_;

 protected:
  void clone_into(network_message* cln) const;

 protected:
  uint64_t flow_id_;

  sw::app_id aid_;

  bool needs_ack_;

  long bytes_;

  type_t type_;

};

}
}
#endif // NETWORK_MESSAGE_H