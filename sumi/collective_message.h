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

#ifndef sumi_api_COLLECTIVE_MESSAGE_H
#define sumi_api_COLLECTIVE_MESSAGE_H

#include <sumi/message.h>
#include <sumi/collective.h>
#include <sprockit/thread_safe_new.h>

namespace sumi {

/**
 * @class collective_done_message
 * The message that is actually delivered when calling #sumi::comm_poll
 * This encapsulates all the information about a collective that has completed in the background
 */
class collective_done_message :
  public message,
  public sprockit::thread_safe_new<collective_done_message>
{

 public:
  std::string to_string() const override {
    return "collective done message";
  }

  collective_done_message(int tag, collective::type_t ty, communicator* dom, uint8_t cq_id) :
    message(-1, -1, cq_id, cq_id, collective_done,
            -1, "", -1, -1, -1, -1, false, nullptr, network_message::header{}),
    tag_(tag), result_(0), vote_(0), type_(ty),
    dom_(dom)
  {
  }

  int tag() const {
    return tag_;
  }

  collective::type_t type() const {
    return type_;
  }

  communicator* dom() const {
    return dom_;
  }

  void set_type(collective::type_t ty) {
    type_ = ty;
  }

  void set_result(void* buf){
    result_ = buf;
  }

  void* result() {
    return result_;
  }

  void set_vote(int v){
    vote_ = v;
  }

  int vote() const {
    return vote_;
  }

  sstmac::hw::network_message* clone_injection_ack() const override {
    auto* cln = new collective_done_message(*this);
    cln->convert_to_ack();
    return cln;
  }

  int comm_rank() const {
    return comm_rank_;
  }

  void set_comm_rank(int rank){
    comm_rank_ = rank;
  }

 protected:
  int tag_;
  void* result_;
  int vote_;
  collective::type_t type_;
  int comm_rank_;
  communicator* dom_;
};

/**
 * @class collective_work_message
 * Main message type used by collectives
 */
class collective_work_message :
  public message
{
  ImplementSerializable(collective_work_message)
 public:
  typedef enum {
    eager, get, put
  } protocol_t;

 public:
  template <class... Args>
  collective_work_message(
    collective::type_t type,
    int dom_sender, int dom_recver,
    protocol_t p,
    int tag, int round,
    void* buffer,
    Args&&... args) :
    message(std::forward<Args>(args)...),
    tag_(tag),
    type_(type),
    round_(round),
    dom_sender_(dom_sender),
    dom_recver_(dom_recver),
    protocol_(p),
    buffer_(buffer)
  {
    if (this->class_type() != collective){
      spkt_abort_printf("collective work message is not of type collect");
    }
  }

  void reverse(){
    std::swap(dom_sender_, dom_recver_);
  }

  virtual std::string to_string() const override;

  static const char* tostr(protocol_t p);

  virtual void serialize_order(sstmac::serializer& ser) override;

  protocol_t protocol() const {
    return protocol_;
  }

  int tag() const {
    return tag_;
  }

  int dom_sender() const {
    return dom_sender_;
  }

  int dom_recver() const {
    return dom_recver_;
  }

  int dom_target_rank() const {
    switch (network_message::type()){
     case network_message::payload:
     case network_message::rdma_get_payload:
     case network_message::rdma_put_payload:
     case network_message::rdma_get_nack:
      return dom_recver_;
     case network_message::payload_sent_ack:
     case network_message::rdma_get_sent_ack:
     case network_message::rdma_put_sent_ack:
      return dom_sender_;
     default:
      spkt_abort_printf("Bad payload type %d to CQ id", network_message::type());
      return -1;
    }
  }

  int round() const {
    return round_;
  }

  void* buffer() const {
    return buffer_;
  }

  collective::type_t type() const {
    return type_;
  }

  sstmac::hw::network_message* clone_injection_ack() const override {
    collective_work_message* cln = new collective_work_message(*this);
    cln->convert_to_ack();
    return cln;
  }

 protected:
  collective_work_message(){} //for serialization

 private:
  int tag_;

  collective::type_t type_;

  int round_;

  int dom_sender_;

  int dom_recver_;

  protocol_t protocol_;

  void* buffer_;

};

}


#endif // COLLECTIVE_MESSAGE_H
