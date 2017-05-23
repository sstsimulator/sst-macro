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

#include <sumi/dynamic_tree_vote.h>
#include <sumi/transport.h>
#include <sumi/communicator.h>
#include <sprockit/stl_string.h>

/*
#undef debug_printf
#define debug_printf(flags, ...) \
  if (tag_ == 221) std::cout << sprockit::spkt_printf(__VA_ARGS__) << std::endl
*/

using namespace sprockit::dbg;

namespace sumi {

#define enumcase(x) case x: return #x
const char*
dynamic_tree_vote_message::tostr(type_t ty)
{
  switch(ty)
  {
  enumcase(down_vote);
  enumcase(up_vote);
  enumcase(request);
  }
  spkt_throw_printf(sprockit::value_error,
      "dynamic_tree_vote_message: unknown type %d", ty);
}

const char*
dynamic_tree_vote_actor::tostr(stage_t stage)
{
  switch(stage)
  {
  enumcase(recv_vote);
  enumcase(up_vote);
  enumcase(down_vote);
  }
  spkt_throw_printf(sprockit::value_error,
      "dynamic_tree_vote_actor: unknown stage %d", stage);
}

void*
dynamic_tree_vote_message::recv_buffer() const
{
  spkt_throw(sprockit::unimplemented_error,
    "dynamic_tree_vote_message: does not have recv buffer");
}

void
dynamic_tree_vote_message::serialize_order(sumi::serializer &ser)
{
  ser & vote_;
  ser & type_;
  collective_work_message::serialize_order(ser);
}

void
dynamic_tree_vote_actor::stop_check_neighbor(int phys_rank)
{
  my_api_->cancel_ping(phys_rank, timeout_);
}

bool
dynamic_tree_vote_actor::check_neighbor(int phys_rank)
{
  return my_api_->ping(phys_rank, timeout_);
}

void
dynamic_tree_vote_actor::position(int rank, int &level, int &branch)
{
  level = 0;
  int level_upper_bound = 1;
  int stride = 1;
  while (rank >= level_upper_bound){
    ++level;
    stride *= 2;
    level_upper_bound += stride;
  }
  int level_lower_bound = level_upper_bound - stride;
  branch = rank - level_lower_bound;
}

void
dynamic_tree_vote_actor::level_stats(int level, int& start, int& size)
{
  int lower_bound = 0;
  int stride = 1;
  for (int i=0; i < level; ++i){
    lower_bound += stride;
    stride *= 2;
  }
  start = lower_bound;
  size = stride;
}

int
dynamic_tree_vote_actor::level_lower_bound(int level)
{
  int bound, ignore;
  level_stats(level, bound, ignore);
  return bound;
}

int
dynamic_tree_vote_actor::down_partner(int level, int branch)
{
  int down_branch = branch*2;
  int down_level_start = level_lower_bound(level + 1);
  return down_level_start + down_branch;
}

int
dynamic_tree_vote_actor::up_partner(int level, int branch)
{
  int up_branch = branch / 2;
  int up_level_start = level_lower_bound(level - 1);
  return up_level_start + up_branch;
}

int
dynamic_tree_vote_actor::up_partner(int rank)
{
  int level, branch;
  position(rank, level, branch);
  return up_partner(level, branch);
}

dynamic_tree_vote_actor::dynamic_tree_vote_actor(int vote,
    vote_fxn fxn,
    int tag,
    transport* my_api,
    communicator* dom,
    int context) :
  collective_actor(my_api, dom, tag, context, true), //true for always fault-aware
  vote_(vote),
  fxn_(fxn),
  tag_(tag),
  stage_(recv_vote)
{
  int ignore;
  position(dense_me_, my_level_, my_branch_);
  //figure out the bottom level from nproc
  position(dense_nproc_-1,bottom_level_,ignore);
  level_stats(my_level_, my_level_start_, my_level_size_);

  if (my_level_ != bottom_level_){
    int partner = down_partner(my_level_, my_branch_);
    if (partner < dense_nproc_){
      down_partners_.insert(partner);
    }
    ++partner;
    if (partner < dense_nproc_){
      down_partners_.insert(partner);
    }
  }

  if (my_level_ == 0){
    up_partner_ = -1;
  }
  else {
    up_partner_ = up_partner(my_level_, my_branch_);
  }

  debug_printf(sumi_collective | sumi_vote,
    "Rank %s from nproc=%d(%d) is at level=%d start=%d bottom=%d in branch=%d up=%d down=%s on tag=%d ",
    rank_str().c_str(), dense_nproc_, my_api_->nproc(),
    my_level_, my_level_start_, bottom_level_ + 1,
    my_branch_, up_partner_, down_partners_.to_string().c_str(), tag_);
}

void
dynamic_tree_vote_actor::up_partner_failed()
{
  int up_level, up_branch;

  position(up_partner_, up_level, up_branch);
  if (up_level == 0){
    spkt_throw(sprockit::illformed_error,
        "dynamic_vote_actor::catastrophic_error: coordinator failed");
  }
  //I take over sending to the up partner
  int old_partner = up_partner_;
  up_partner_ = up_partner(up_level, up_branch);

  debug_printf(sumi_collective | sumi_vote,
    "Rank %s got up failure from rank %d on stage %s on tag=%d: partners are now down=%s up=%d",
    rank_str().c_str(), old_partner, tostr(stage_), tag_,
    down_partners_.to_string().c_str(), up_partner_);

  bool failed = ping_neighbor(up_partner_);
  if (failed){
    handle_dense_partner_failure(up_partner_);
  }
  else if (stage_ == up_vote){
    //yay, partner is still alive
    send_message(dynamic_tree_vote_message::up_vote, up_partner_);
  }

}

void
dynamic_tree_vote_actor::contact_new_down_partner(int rank)
{
  if (stage_ == recv_vote){
    if (!received_up_vote(rank)){
      send_message(dynamic_tree_vote_message::request, rank);
    }
  }
  else if (stage_ == down_vote){
    //send new down votes on down
      send_message(dynamic_tree_vote_message::down_vote, rank);
  }
}

void
dynamic_tree_vote_actor::add_new_down_partner(int rank)
{
  debug_printf(sumi_collective | sumi_vote,
    "Rank %s adding new down partner %d on tag=%d ",
    rank_str().c_str(), rank, tag_);

  down_partners_.insert(rank);
  bool failed = ping_neighbor(rank);
  if (failed){
    //well, that's a downer
    handle_dense_partner_failure(rank);
  }
  else {
    contact_new_down_partner(rank);
  }
}

void
dynamic_tree_vote_actor::down_partner_failed(int rank)
{
  down_partners_.erase(rank);

  int down_level, down_branch;
  position(rank, down_level, down_branch);

  debug_printf(sumi_collective | sumi_vote,
    "Rank %s got down failure from rank=%d level=%d branch=%d on tag=%d : partners are now down=%s up=%d, votes_recved=%s",
    rank_str().c_str(),
    rank, down_level, down_branch,
    tag_,
    down_partners_.to_string().c_str(),
    up_partner_,
    up_votes_recved_.to_string().c_str());

  int new_partner = down_partner(down_level, down_branch);
  if (new_partner < dense_nproc_){
    add_new_down_partner(new_partner);
  }
  ++new_partner;
  if (new_partner < dense_nproc_){
    add_new_down_partner(new_partner);
  }

  //it could happen that partner failing means we have everything we need
  if (stage_ == recv_vote && up_vote_ready()){
    send_up_votes();
  }

}

void
dynamic_tree_vote_actor::rebalance_around_dense_partner_failure(int rank)
{
  failures_handled_.insert(rank);
  failed_ranks_.insert(rank);
  if (stage_ == recv_vote){
    agreed_upon_failures_.insert(rank); //I'm only allowed to add new info in the recv stage
  }

  if (complete_){
    spkt_throw_printf(sprockit::illformed_error,
        "vote_actor: on rank %d, partner %d failed but collective is complete on tag %d",
          my_api_->rank(), rank, tag_);
  }

  if (rank == up_partner_){
    up_partner_failed();
  }
  else {
    down_partner_failed(rank);
  }
}

void
dynamic_tree_vote_actor::dense_partner_ping_failed(int dense_rank)
{
  cancel_ping(dense_rank);
  handle_dense_partner_failure(dense_rank);
}

void
dynamic_tree_vote_actor::handle_dense_partner_failure(int rank)
{
  if (failures_handled_.count(rank)){
    debug_printf(sumi_collective | sumi_vote,
        "Rank %s already handled failure of rank %d on tag=%d ",
         rank_str().c_str(), rank, tag_);
  }
  else {
    debug_printf(sumi_collective | sumi_vote,
       "Rank %s handling new failure of rank %d on tag=%d ",
       rank_str().c_str(), rank, tag_);
    rebalance_around_dense_partner_failure(rank);
  }
}

void
dynamic_tree_vote_actor::send_messages(dynamic_tree_vote_message::type_t ty, const thread_safe_set<int>& partners)
{
  partner_iterator it, end = partners.start_iteration();
  for (it = partners.begin(); it != end; ++it){
    send_message(ty, *it);
  }
  partners.end_iteration();
}

bool
dynamic_tree_vote_actor::up_vote_ready()
{
  //do a set diff - we may actually get MORE votes than we need
  //due to weird failure scenarios
  if (down_partners_.size() > up_votes_recved_.size()){
    return false; //might as well do an easy check
  }

  //this produces down_partners_ - up_votes_recved_
  //in other words, if up_votes_receved contains all the down partners
  //the result set will be empty
  std::set<int> pending;
  set_difference(down_partners_, up_votes_recved_, pending);
  return pending.empty();
}

void
dynamic_tree_vote_actor::send_up_votes()
{
  if (my_level_ == 0){
    //nothing to do - go straight to sending down
    send_down_votes();
  }
  else if (stage_ == recv_vote) {
    stage_ = up_vote;
    //it could happen that we are triggered to send up votes
    //because we acquired new down partners
    //when in reailty we have already sent the up vote
    debug_printf(sumi_collective | sumi_vote,
      "Rank %s sending up vote %d to partner=%d on tag=%d ",
      rank_str().c_str(), vote_, up_partner_, tag_);
    send_message(dynamic_tree_vote_message::up_vote, up_partner_);
  }
}

void
dynamic_tree_vote_actor::start()
{
  stage_ = recv_vote;

  //this set can get modified - make a temporary
  std::set<int> down_partners_at_start = down_partners_.get_copy();
  partner_iterator it, end = down_partners_at_start.end();
  for (it=down_partners_at_start.begin(); it != end; ++it){
    int rank = *it;
    bool failed = ping_neighbor(rank);
    if (failed){ //uh oh!
      handle_dense_partner_failure(rank);
    }
  }
  if (up_partner_ >= 0){
    bool failed = ping_neighbor(up_partner_);
    if (failed){
      handle_dense_partner_failure(up_partner_);
    }
  }

  //we might be at the bottom of tree or because of failures
  //we can just go ahead and sent up votes
  if (up_vote_ready()){
    send_up_votes();
  }
}

void
dynamic_tree_vote_actor::send_message(dynamic_tree_vote_message::type_t ty, int virtual_dst)
{
  dynamic_tree_vote_message::ptr msg = new dynamic_tree_vote_message(vote_, ty, tag_, dense_me_, virtual_dst);
  if (stage_ == up_vote){  //If I am up-voting, go ahead and vote for as many failures as I know
    msg->append_failed(failed_ranks_);
  }
  else {
    // I can only send the failures that everyone has agreed upon
    msg->append_failed(agreed_upon_failures_);
  }

  int global_phys_dst = global_rank(virtual_dst);
  debug_printf(sumi_collective | sumi_vote,
    "Rank %s sending vote %d of type %s to %s on tag=%d for failed=%s,msg_failed=%s",
    rank_str().c_str(),
    vote_, dynamic_tree_vote_message::tostr(ty), 
    rank_str(virtual_dst).c_str(),
    tag_, failed_ranks_.to_string().c_str(),
    stl_string(msg->failed_procs()).c_str());
  my_api_->send_payload(global_phys_dst, msg);
}

void
dynamic_tree_vote_actor::recv_result(const dynamic_tree_vote_message::ptr&msg)
{
  debug_printf(sumi_collective | sumi_vote,
      "Rank %d receving result vote=%d, failed=%s on tag=%d",
      my_api_->rank(), vote_,
      stl_string(msg->failed_procs()).c_str(), tag_);
  const std::set<int> extra_failed = msg->failed_procs();
  vote_ = msg->vote();
  agreed_upon_failures_ = msg->failed_procs();
  failed_ranks_.insert_all(extra_failed);
}

void
dynamic_tree_vote_actor::merge_result(const dynamic_tree_vote_message::ptr& msg)
{
  const std::set<int> extra_failed = msg->failed_procs();

  if (stage_ == recv_vote){
    debug_printf(sumi_collective | sumi_vote,
      "Rank %d merging result vote=%d failed=%s on tag=%d into final result vote=%d failed=%s",
      my_api_->rank(), msg->vote(),
      stl_string(msg->failed_procs()).c_str(), tag_,
      vote_, failed_ranks_.to_string().c_str());
    agreed_upon_failures_.insert_all(extra_failed);
    (fxn_)(vote_, msg->vote());
  }

  failed_ranks_.insert_all(extra_failed);
}

void
dynamic_tree_vote_actor::put_done_notification()
{
  collective_done_message::ptr msg = new collective_done_message(tag_, collective::dynamic_tree_vote, comm_);
  msg->set_comm_rank(comm_->my_comm_rank());
  //convert the virtual ranks to physical ranks
  thread_safe_set<int>::const_iterator it, end = agreed_upon_failures_.start_iteration();
  for (it = agreed_upon_failures_.begin(); it != end; ++it){
    msg->append_failed(global_rank(*it));
  }
  agreed_upon_failures_.end_iteration();
  msg->set_vote(vote_);
  debug_printf(sumi_collective | sumi_vote,
    "Rank %d has completed with vote=%d failed=%s tag=%d ",
      my_api_->rank(), msg->vote(),
      msg->failed_procs().to_string().c_str(), tag_);

  my_api_->notify_collective_done(msg);
}

void
dynamic_tree_vote_actor::finish()
{
  if (complete_){
    spkt_throw_printf(sprockit::illformed_error,
        "dynamic_vote_actor::finish: tag=%d already complete on rank %d",
	tag_, my_api_->rank());
  }
  complete_ = true;

  if (up_partner_ >= 0)
    cancel_ping(up_partner_);

  down_partners_.start_iteration();
  partner_iterator it, end = down_partners_.find(up_partner_);
  for (it=down_partners_.begin(); it != end; ++it){
    cancel_ping(*it);
  }
  down_partners_.end_iteration();

  //okay - this is a bit weird, but we have to "erase" our knowledge
  //of failures we haven't agreed to
  failed_ranks_ = agreed_upon_failures_;

  put_done_notification();
  timeout_ = 0;

  collective_actor::validate_pings_cleared();
}


void
dynamic_tree_vote_actor::recv_down_vote(const dynamic_tree_vote_message::ptr& msg)
{
  debug_printf(sumi_collective | sumi_vote,
    "Rank %s got down vote %d on stage %s from rank=%d tag=%d for failed=%s",
     rank_str().c_str(), msg->vote(), tostr(stage_),
     msg->dense_sender(), tag_,
     stl_string(msg->failed_procs()).c_str());
     

  if (stage_ == down_vote){
    if (vote_ != msg->vote()){
      spkt_throw(sprockit::value_error,
        "dynamic_vote_actor::inconsistent_down_vote: %d %d",
        msg->vote(), vote_);
    }
    return;  //already got this
  }

  recv_result(msg);
  //where I got this from is totally irrelevant - I'm good to go
  send_down_votes();
}

void
dynamic_tree_vote_actor::send_down_votes()
{
  debug_printf(sumi_collective | sumi_vote,
    "Rank %s sending down vote %d to down=%s on tag=%d ",
    rank_str().c_str(), vote_,
    down_partners_.to_string().c_str(), tag_);

  stage_ = down_vote;
  send_messages(dynamic_tree_vote_message::down_vote, down_partners_);

  // I'm sort of done here...
  finish();
}

void
dynamic_tree_vote_actor::recv_unexpected_up_vote(const dynamic_tree_vote_message::ptr& msg)
{
  if (complete()){
    // somebody failed after I sent out my messages - so somebody new
    // is requesting a message from me
    debug_printf(sumi_collective | sumi_vote,
      "Rank %s responding after completion to %d on tag=%d ",
      rank_str().c_str(), msg->dense_sender(), tag_);
    send_message(dynamic_tree_vote_message::down_vote, msg->dense_sender());
  }
  else {  
    //now treat this essentially as a failure notification
    int child = msg->dense_sender();
    int failed_parent = up_partner(child);
    int failures[bottom_level_];
    int num_failures = 0;
    while (failed_parent != dense_me_){
      if (is_my_child(failed_parent)){
        //I am pinging this - cancel the ping
        cancel_ping(failed_parent);
      }
      debug_printf(sumi_collective | sumi_vote,
        "Rank %s inferring down failure %d via up vote from %d on tag=%d ",
        rank_str().c_str(), failed_parent, msg->dense_sender(), tag_);
      failures[num_failures++] = failed_parent;
      //move on up the tree
      child = failed_parent;
      failed_parent = up_partner(child);
    }

    //now that we have a list of failed parents, fail them
    for (int i=(num_failures-1); i >= 0; --i){
      handle_dense_partner_failure(failures[i]);
    }
  }

}

void
dynamic_tree_vote_actor::recv_expected_up_vote(const dynamic_tree_vote_message::ptr& msg)
{
  debug_printf(sumi_collective | sumi_vote,
    "Rank %s got expected up vote %d on stage %s from rank=%d on tag=%d - need %s, have %s",
    rank_str().c_str(), msg->vote(), tostr(stage_), msg->dense_sender(), tag_,
    down_partners_.to_string().c_str(), up_votes_recved_.to_string().c_str());

  if (up_vote_ready()){
    send_up_votes();
  }
}

void
dynamic_tree_vote_actor::recv_up_vote(const dynamic_tree_vote_message::ptr& msg)
{
  debug_printf(sumi_collective | sumi_vote,
    "Rank %s got up vote %d on stage %s from rank=%d on tag=%d ",
    rank_str().c_str(), msg->vote(), tostr(stage_), msg->dense_sender(), tag_);

  merge_result(msg);

  int src = msg->dense_sender();
  //if (is_failed(src)){
    //just ignore this - dangling message that got delivered
    //while its parent node failed in transit
  //  return; //just ignore
 //}

  up_votes_recved_.insert(src);
  if (down_partners_.count(src) == 0){
    //I got a message I wasn't expecting - somebody underneath me died!
    recv_unexpected_up_vote(msg);
  }
  else {
    recv_expected_up_vote(msg);
  }
}

void
dynamic_tree_vote_actor::recv_adoption_request(const dynamic_tree_vote_message::ptr& msg)
{
  //we got this if our up partner failed - this guy is requesting an up vote
  //treat this essentially as a failure notification

  //search up our tree until we find this guy
  int failed_guy = msg->dense_sender();
  while (failed_guy != up_partner_){
    debug_printf(sumi_collective | sumi_vote | sumi_collective_sendrecv,
      "Rank %s inferring up failure %d on stage %s via request from %d on tag=%d ",
      rank_str().c_str(), up_partner_, tostr(stage_), msg->dense_sender(), tag_);
    cancel_ping(up_partner_);
    handle_dense_partner_failure(up_partner_);
  }
}

void
dynamic_tree_vote_actor::recv(const dynamic_tree_vote_message::ptr&msg)
{
  if (is_failed(msg->dense_sender())){
    debug_printf(sumi_collective | sumi_vote | sumi_collective_sendrecv,
      "Rank %s skipping message from %d:%d on tag=%d because that rank is already dead",
      rank_str().c_str(), msg->dense_sender(), msg->sender(), tag_);
    //ignore this - sometimes messages from dead nodes get caught in transit
    return;
  }

  switch (msg->type())
  {
  case dynamic_tree_vote_message::up_vote:
    recv_up_vote(msg);
    break;
  case dynamic_tree_vote_message::down_vote:
    recv_down_vote(msg);
    break;
  case dynamic_tree_vote_message::request:
    recv_adoption_request(msg);
    break;
  }
}

dynamic_tree_vote_collective::dynamic_tree_vote_collective(
  int vote, vote_fxn fxn, int tag,
  transport* my_api, communicator* dom,
  int context) :
  collective(collective::dynamic_tree_vote, my_api, dom, tag, context),
  vote_(vote),
  fxn_(fxn)
{
  actors_[dense_me_] = new dynamic_tree_vote_actor(vote, fxn, tag, my_api, dom, context);
  refcounts_[comm_->my_comm_rank()] = actors_.size();
}

void
dynamic_tree_vote_collective::recv(int target, const collective_work_message::ptr&msg)
{
  actor_map::iterator it = actors_.find(target);
  if (it == actors_.end()){
    spkt_throw_printf(sprockit::value_error,
        "vote_collective::recv: invalid virtual destination %d on rank %d for tag=%d ",
        target, my_api_->rank(), tag_);
  }

  dynamic_tree_vote_actor* schauspieler = it->second;
  schauspieler->recv(ptr_safe_cast(dynamic_tree_vote_message, msg));
}

void
dynamic_tree_vote_collective::start()
{
  actor_map::iterator it, end = actors_.end();
  for (it=actors_.begin(); it != end; ++it){
    dynamic_tree_vote_actor* actor = it->second;
    actor->start();
  }
}

}