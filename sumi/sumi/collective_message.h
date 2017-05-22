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

#ifndef sumi_api_COLLECTIVE_MESSAGE_H
#define sumi_api_COLLECTIVE_MESSAGE_H

#include <sumi/message.h>
#include <sumi/collective.h>
#include <sumi/thread_safe_set.h>

namespace sumi {

/**
 * @class collective_done_message
 * The message that is actually delivered when calling #sumi::comm_poll
 * This encapsulates all the information about a collective that has completed in the background
 */
class collective_done_message :
  public message
{
 public:
  typedef sprockit::refcount_ptr<collective_done_message> ptr;

 public:
  std::string
  to_string() const override {
    return "collective done message";
  }

  collective_done_message(int tag, collective::type_t ty,
                          communicator* dom) :
    message(collective_done),
    tag_(tag), result_(0), vote_(0), type_(ty),
    all_ranks_know_failure_(false), dom_(dom)
  {
  }

  int
  tag() const {
    return tag_;
  }

  collective::type_t
  type() const {
    return type_;
  }

  communicator*
  dom() const {
    return dom_;
  }

  void
  set_type(collective::type_t ty) {
    type_ = ty;
  }

  bool
  failed() const {
    return !failed_procs_.empty();
  }

  bool
  succeeded() const {
    return failed_procs_.empty();
  }

  void
  append_failed(int proc) {
    failed_procs_.insert(proc);
  }

  void
  append_failed(const std::set<int>& procs){
    failed_procs_.insert(procs.begin(), procs.end());
  }

  const thread_safe_set<int>&
  failed_procs() const {
    return failed_procs_;
  }

  bool
  all_ranks_know_failure() const {
    return all_ranks_know_failure_;
  }

  void
  set_all_ranks_know_failure(bool flag) {
    all_ranks_know_failure_ = true;
  }

  void
  set_result(void* buf){
    result_ = buf;
  }

  void*
  result() {
    return result_;
  }

  void
  set_vote(int v){
    vote_ = v;
  }

  int
  vote() const {
    return vote_;
  }

  message*
  clone() const override;

  int comm_rank() const {
    return comm_rank_;
  }

  void
  set_comm_rank(int rank){
    comm_rank_ = rank;
  }

 protected:
  int tag_;
  void* result_;
  int vote_;
  collective::type_t type_;
  thread_safe_set<int> failed_procs_;
  bool all_ranks_know_failure_;
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
  typedef sprockit::refcount_ptr<collective_work_message> ptr;

  typedef enum {
    get_data, //recver gets data
    put_data, //sender puts data
    rdma_get_header, //sender sends a header to recver to configure RDMA get
    rdma_put_header, //recver sends a header to sender to configure RDMA put
    eager_payload, //for small messages, no recv header - just send payload
    nack_get_ack,
    nack_put_payload,
    nack_eager,
    nack_get_header, //collective has failed, send fake message nack instead of real one
    nack_put_header //collective has failed, send fake message nack instead of real one
  } action_t;


 public:
  collective_work_message(
    collective::type_t type,
    action_t action,
    size_t nbytes,
    int tag, int round,
    int src, int dst) :
    message(nbytes, collective),
    tag_(tag),
    type_(type),
    round_(round),
    dense_sender_(src),
    dense_recver_(dst),
    action_(action)
  {
  }

  collective_work_message(
    collective::type_t type,
    action_t action,
    int tag, int round,
    int src, int dst) :
    message(collective),
    tag_(tag),
    type_(type),
    round_(round),
    dense_sender_(src),
    dense_recver_(dst),
    action_(action)
  {
  }

  collective_work_message(){} //for serialization


  virtual std::string
  to_string() const override;

  static const char*
  tostr(action_t action);

  virtual void
  serialize_order(sumi::serializer& ser) override;

  action_t
  action() const {
    return action_;
  }

  void
  set_action(action_t a) {
    action_ = a;
  }

  int
  tag() const {
    return tag_;
  }

  int
  round() const {
    return round_;
  }

  int
  dense_sender() const {
    return dense_sender_;
  }

  int
  dense_recver() const {
    return dense_recver_;
  }

  void
  reverse() override;

  collective::type_t
  type() const {
    return type_;
  }

  bool
  is_failure_notice() const {
    return !failed_procs_.empty();
  }

  void
  append_failed(int proc) {
    failed_procs_.insert(proc);
  }

  void
  append_failed(const thread_safe_set<int>& failed);

  const std::set<int>&
  failed_procs() const {
    return failed_procs_;
  }

  message*
  clone() const override {
    collective_work_message* cln = new collective_work_message;
    clone_into(cln);
    return cln;
  }

 protected:
  void
  clone_into(collective_work_message* cln) const;

 protected:
  int tag_;

  collective::type_t type_;

  int round_;

  int dense_sender_;

  int dense_recver_;

  action_t action_;

  std::set<int> failed_procs_;

};

}


#endif // COLLECTIVE_MESSAGE_H