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

#ifndef sumi_api_VOTE_H
#define sumi_api_VOTE_H

#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>
#include <sumi/collective.h>
#include <sumi/thread_safe_set.h>

namespace sumi {


class dynamic_tree_vote_message :
 public collective_work_message,
 public sumi::serializable_type<dynamic_tree_vote_message>
{
 ImplementSerializable(dynamic_tree_vote_message)

 public:
  dynamic_tree_vote_message(){} //for serialization

  typedef sprockit::refcount_ptr<dynamic_tree_vote_message> ptr;

  typedef enum {
    up_vote,
    down_vote,
    request
  } type_t;

  type_t
  type() const {
    return type_;
  }

  int
  vote() const {
    return vote_;
  }

  void*
  recv_buffer() const;

  static const char*
  tostr(type_t);

  virtual void
  serialize_order(sumi::serializer &ser) override;

  dynamic_tree_vote_message(int vote, type_t ty, int tag, int virtual_src, int virtual_dst) :
    //0 = buffer
    //0 = nelems
    //0 = type_size
    //-1 = not done by round
    collective_work_message(collective::dynamic_tree_vote, eager_payload,
      sizeof(int) + sizeof(std::set<int>),
      tag, -1, virtual_src, virtual_dst),
    type_(ty),
    vote_(vote)
  {
  }

 protected:
  type_t type_;

  int vote_;

};

class dynamic_tree_vote_actor :
  public collective_actor
{

 public:

  typedef enum {
    recv_vote,
    up_vote,
    down_vote
  } stage_t;

  static const char*
  tostr(stage_t stage);

  std::string
  to_string() const override {
    return "vote actor";
  }

  void
  recv(const dynamic_tree_vote_message::ptr& msg);

  dynamic_tree_vote_actor(int vote,
    vote_fxn fxn, int tag,
    transport* my_api,
    communicator* dom,
    int context);

  stage_t
  stage() const {
    return stage_;
  }

  bool
  complete() const {
    return complete_;
  }

  int
  vote() const {
    return vote_;
  }

  void start();

 protected:
  void put_done_notification();

  void up_partner_failed();

  void down_partner_failed(int rank);

  void
  dense_partner_ping_failed(int virtual_rank) override;

  virtual bool
  check_neighbor(int phys_rank) override;

  virtual void
  stop_check_neighbor(int phys_rank) override;

  void
  send_down_votes();

  void
  send_up_votes();

  /**
   * Receive a regular down vote.
   * This vote is FINAL, immutable. Any new failures detected
   * must be ignored for consistency.
   * @param msg
   */
  void
  recv_down_vote(const dynamic_tree_vote_message::ptr& msg);

  /**
   * Receive an up vote. This vote is mutable - new failures can be added.
   * If all up votes received, forward an up vote to my parent.
   * @param msg
   */
  void
  recv_up_vote(const dynamic_tree_vote_message::ptr& msg);

  /**
   * Receive a request from a new parent.
   * If our parent has failed (but we don't know it yet),
   * we might get an adoption request from a grandparent or great-grandparent.
   * This declares the parent failed and reconnects to the new parent.
   * @param msg
   */
  void
  recv_adoption_request(const dynamic_tree_vote_message::ptr& msg);

  /**
   * Receive a vote from a known child. See #recv_unexpected_up_vote
   * @param msg
   */
  void
  recv_expected_up_vote(const dynamic_tree_vote_message::ptr& msg);

  /**
   * Recv unexpected up vote. This comes from a granchild,great-granchild,etc node
   * we were not expected to hear from.
   * This indicates that a child node has failed and we must reconnect.
   * @param msg
   */
  void
  recv_unexpected_up_vote(const dynamic_tree_vote_message::ptr& msg);

  /**
   * If processing up votes, merge the incoming up vote with our known results.
   * At this point, the vote is mutable and new information can be added
   * @param msg
   */
  void
  merge_result(const dynamic_tree_vote_message::ptr& msg);

  /**
   * If processing down votes, receive and overwrite with incoming down vote.
   * At this point, the vote is immutable to guarantee global agreement.
   * @param msg
   */
  void
  recv_result(const dynamic_tree_vote_message::ptr& msg);

  void append_down_partners(int position);

  void send_message(dynamic_tree_vote_message::type_t ty, int dst);

  void send_messages(dynamic_tree_vote_message::type_t ty, const thread_safe_set<int>& partners);

  void finish();

  int rank(int level, int branch);

  void position(int rank, int& level, int& branch);

  void level_stats(int level, int& start, int& size);

  int up_partner(int rank);

  int up_partner(int level, int branch);

  int down_partner(int level, int branch);

  int level_lower_bound(int level);

  void contact_new_down_partner(int rank);

  void add_new_down_partner(int rank);

  bool
  received_up_vote(int rank) const {
    return up_votes_recved_.count(rank);
  }

  bool
  up_vote_ready();

  /**
   * Respond to a failure notification, but first check to see if we already responded to it
   * This a universal response for all notifications where they be pings, messages, NACKS, etc
   * It is safe for any failure notifier to call this
   * @param rank
   */
  void
  handle_dense_partner_failure(int dense_rank);

  /**
   * If this failure is not previously handled,
   * rebuild the tree to work around the failure.
   * This should only be called ONCE for each failure.
   * If this is called multiple times for the same failure,
   * the behavior is undefined.  In general, only use
   * #handle_virtual_partner_failure for safety.
   * @param virtual_rank
   */
  void
  rebalance_around_dense_partner_failure(int dense_rank);

  bool
  is_my_child(int dense_rank) const {
    return down_partners_.count(dense_rank);
  }

 protected:
  typedef thread_safe_set<int>::const_iterator partner_iterator;
  int up_partner_;
  thread_safe_set<int> down_partners_;

  int my_level_;
  int my_branch_;
  int my_level_size_;
  int my_level_start_;
  int bottom_level_;

  int vote_;
  vote_fxn fxn_;

  int tag_;

  stage_t stage_;

  thread_safe_set<int> agreed_upon_failures_;
  thread_safe_set<int> failures_handled_;
  thread_safe_set<int> up_votes_recved_;
};



class dynamic_tree_vote_collective :
  public collective
{

 public:
  std::string
  to_string() const override {
    return "sumi persistent resilient vote collective";
  }

  void
  recv(int target, const collective_work_message::ptr& msg) override;

  void
  start() override;

  bool
  persistent() const override {
    return true;
  }

  dynamic_tree_vote_collective(
    int redundant_tree_vote,
    vote_fxn fxn,
    int tag,
    transport* my_api,
    communicator* comm,
    int context);

 protected:
  void put_done_notification();

 protected:
  // map dense rank to actor
  typedef std::map<int, dynamic_tree_vote_actor*> actor_map;
  actor_map actors_;
  int vote_;

  vote_fxn fxn_;

};

}


#endif // VOTE_H