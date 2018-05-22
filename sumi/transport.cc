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

#include <cstring>
#include <sumi/transport.h>
#include <sumi/dynamic_tree_vote.h>
#include <sumi/allreduce.h>
#include <sumi/reduce.h>
#include <sumi/allgather.h>
#include <sumi/allgatherv.h>
#include <sumi/alltoall.h>
#include <sumi/alltoallv.h>
#include <sumi/communicator.h>
#include <sumi/bcast.h>
#include <sumi/gather.h>
#include <sumi/scatter.h>
#include <sumi/gatherv.h>
#include <sumi/scatterv.h>
#include <sumi/scan.h>
#include <sprockit/stl_string.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
 { "lazy_watch", "whether failure notifications can be receive without active pinging" },
 { "eager_cutoff", "what message size in bytes to switch from eager to rendezvous" },
 { "use_put_protocol", "whether to use a put or get protocol for pt2pt sends" },
 { "algorithm", "the specific algorithm to use for a given collecitve" },
 { "comm_sync_stats", "whether to track synchronization stats for communication" },
 { "smp_single_copy_size", "the minimum size of message for single-copy protocol" },
 { "max_eager_msg_size", "the maximum size for using eager pt2pt protocol" },
 { "max_vshort_msg_size", "the maximum size for mailbox protocol" },
);

RegisterDebugSlot(sumi);

namespace sumi {

const int options::initial_context = -2;

collective_algorithm_selector* transport::allgather_selector_ = nullptr;
collective_algorithm_selector* transport::alltoall_selector_ = nullptr;
collective_algorithm_selector* transport::alltoallv_selector_ = nullptr;
collective_algorithm_selector* transport::allreduce_selector_ = nullptr;
collective_algorithm_selector* transport::allgatherv_selector_ = nullptr;
collective_algorithm_selector* transport::bcast_selector_ = nullptr;
collective_algorithm_selector* transport::gather_selector_ = nullptr;
collective_algorithm_selector* transport::gatherv_selector_ = nullptr;
collective_algorithm_selector* transport::reduce_selector_ = nullptr;
collective_algorithm_selector* transport::scan_selector_ = nullptr;
collective_algorithm_selector* transport::scatter_selector_ = nullptr;
collective_algorithm_selector* transport::scatterv_selector_ = nullptr;

transport::transport(sprockit::sim_parameters* params) :
#ifdef FEATURE_TAG_SUMI_RESILIENCE
  lazy_watch_(false),
  heartbeat_active_(false),
  heartbeat_running_(false),
  is_dead_(false),
  monitor_(nullptr),
  nspares_(0),
  recovery_lock_(0),
#endif
  completion_queues_(1), //guaranteed one to begin with
  inited_(false),
  system_collective_tag_(-1), //negative tags reserved for special system work
  finalized_(false),
  eager_cutoff_(512),
  use_put_protocol_(false),
  use_hardware_ack_(false),
  global_domain_(nullptr)
{
#ifdef FEATURE_TAG_SUMI_RESILIENCE
  heartbeat_tag_start_ = 1e9;
  heartbeat_tag_stop_ = heartbeat_tag_start_ + 10000;
  heartbeat_tag_ = heartbeat_tag_start_;

  monitor_ = activity_monitor::factory::get_optional_param("activity_monitor", "ping",
                                        params, this);
  lazy_watch_ = params->get_optional_bool_param("lazy_watch", true);
#endif
  eager_cutoff_ = params->get_optional_int_param("eager_cutoff", 512);
  use_put_protocol_ = params->get_optional_bool_param("use_put_protocol", false);
}

void
transport::wait_barrier(int tag)
{
  if (nproc_ == 1) return;
  barrier(tag);
  collective_block(collective::barrier, tag);
}

int
transport::allocate_cq()
{
  uint8_t ret = completion_queues_.size();
  completion_queues_.emplace_back();
  return ret;
}

void
transport::validate_api()
{
  if (!inited_ || finalized_){
    sprockit::abort("SUMI transport calling function while not inited or already finalized");
  }
}

void
transport::set_use_hardware_ack(bool flag)
{
  if (flag && !supports_hardware_ack()){
    sprockit::abort("transport::chosen transport does not support hardware acks");
  }
  use_hardware_ack_ = flag;
}

void
transport::init()
{
  //THIS SHOULD ONLY BE CALLED AFTER RANK and NPROC are known
  inited_ = true;
  global_domain_ = new global_communicator(this);
}

void
transport::finish()
{
  clean_up();
  //this should really loop through and kill off all the pings
  //so none of them execute
  finalized_ = true;
#ifdef FEATURE_TAG_SUMI_RESILIENCE
  debug_printf(sprockit::dbg::sumi,
      "Rank %d sending finalize terminate to %s",
      rank_, failed_ranks_.to_string().c_str());

  thread_safe_set<int>::iterator it, end = failed_ranks_.start_iteration();
  for (it=failed_ranks_.begin(); it != end; ++it){
    int dst = *it;
    debug_printf(sprockit::dbg::sumi,
        "Rank %d sending finalize terminate to %d",
        rank_, dst);
    send_terminate(dst);
  }
  failed_ranks_.end_iteration();

  monitor_->validate_done();
  stop_heartbeat();
  delete monitor_;
  monitor_ = nullptr;
#endif
}

message*
transport::poll(message::payload_type_t ty, bool blocking, int cq_id, double timeout)
{
  auto& queue = completion_queues_[cq_id];
  auto end = queue.end();
  for (auto iter = cq_begin(cq_id); iter != end; ++iter){
    message* msg = *iter;
    if (msg->payload_type() == ty){
      queue.erase(iter);
      return msg;
    }
  }

  return poll_new(ty,blocking,cq_id,timeout);
}

message*
transport::poll(bool blocking, double timeout)
{
  for (auto& queue : completion_queues_){
    if (!queue.empty()){
      message* ret = queue.front();
      queue.pop_front();
      return ret;
    }
  }
  return poll_new(blocking, timeout);
}

message*
transport::poll(bool blocking, int cq_id, double timeout)
{
  auto& queue = completion_queues_[cq_id];
  debug_printf(sprockit::dbg::sumi,
               "Rank %d polling for message on cq %d with time %f",
               rank_, cq_id, timeout);

  if (!queue.empty()){
    message* ret = queue.front();
    queue.pop_front();
    return ret;
  }

  return poll_new(blocking, cq_id, timeout);
}

message*
transport::poll_new(bool blocking, int cq_id, double timeout)
{
  while (1){
    message* msg = poll_new(blocking, timeout);
    if (msg){
      if (msg->cq_id() == cq_id){
        debug_printf(sprockit::dbg::sumi,
                     "Rank %d got hit %p on CQ %d", rank_, msg, cq_id);
        return msg;
      } else {
        completion_queues_[msg->cq_id()].push_back(msg);
      }
    }
    //if I got here and am non-blocking, return null
    if (!blocking || timeout > 0) return nullptr;
  }
}

message*
transport::poll_new(message::payload_type_t ty, bool blocking, int cq_id, double timeout)
{
  while (1){
    message* msg = poll_new(blocking, timeout);
    if (msg){
      if (msg->cq_id() == cq_id && msg->payload_type() == ty){
        return msg;
      } else {
        completion_queues_[msg->cq_id()].push_back(msg);
      }
    }
    //if I got here and am non-blocking, return null
    if (!blocking) return nullptr;
  }
}

message*
transport::poll_new(bool blocking, double timeout)
{
  debug_printf(sprockit::dbg::sumi, "Rank %d polling for NEW message", rank_);
  while (1){
    transport_message* tmsg = poll_pending_messages(blocking, timeout);
    if (tmsg){
      message* cq_notifier = handle(tmsg->take_payload());
      delete tmsg;
      if (cq_notifier || !blocking){
        return cq_notifier;
      }
    } else {
      return nullptr;
    }
  }
}

message*
transport::blocking_poll(int cq_id, double timeout)
{
  return poll(true, cq_id, timeout); //blocking
}

void
transport::notify_collective_done(collective_done_message* msg)
{
  collective* coll = collectives_[msg->type()][msg->tag()];
  if (!coll){
    spkt_throw_printf(sprockit::value_error,
      "transport::notify_collective_done: invalid collective of type %s, tag %d",
       collective::tostr(msg->type()), msg->tag());   
  }
  finish_collective(coll, msg);
}

void
transport::clean_up()
{
  std::list<collective*>::iterator it, end = todel_.end();
  for (it=todel_.begin(); it != end; ++it){
    delete *it;
  }
  todel_.clear();
}

message*
transport::handle(message* msg)
{
  debug_printf(sprockit::dbg::sumi,
    "Rank %d got message %p of class %s, payload %s for sender %d, recver %d, send cq %d, recv cq %d",
     rank_, msg,
     message::tostr(msg->class_type()), message::tostr(msg->payload_type()),
     msg->sender(), msg->recver(),
     msg->send_cq(), msg->recv_cq());

  //we might have collectives to delete and cleanup
  if (!todel_.empty()){
    clean_up();
  }

  switch (msg->class_type())
  {
  case message::bcast:
    //root initiated global broadcast of some metadata
    system_bcast(msg);
    return msg;
  case message::terminate:
  case message::collective_done:
  case message::pt2pt: {
    switch (msg->payload_type()){
      case message::rdma_get:
      case message::rdma_put:
      case message::eager_payload:
        msg->write_sync_value();
        if (msg->needs_recv_ack()){
          return msg;
        }
        break;
      case message::rdma_get_ack:
      case message::rdma_put_ack:
      case message::eager_payload_ack:
        if (msg->needs_send_ack()){
          return msg;
        }
        break;
      default:
        break;
    }
    return msg;
  }
  case message::collective: {
    collective_work_message* cmsg = dynamic_cast<collective_work_message*>(msg);
    if (cmsg->send_cq() == -1 && cmsg->recv_cq() == -1) abort();
    int tag = cmsg->tag();
    collective::type_t ty = cmsg->type();
    tag_to_collective_map::iterator it = collectives_[ty].find(tag);
    if (it == collectives_[ty].end()){
      debug_printf(sprockit::dbg::sumi_collective_sendrecv,
        "Rank %d, queuing %p %s %s from %d on tag %d for type %s",
        rank_, msg,
        message::tostr(msg->payload_type()),
        message::tostr(msg->class_type()),
        msg->sender(),
        tag, collective::tostr(ty));
        //message for collective we haven't started yet
        pending_collective_msgs_[ty][tag].push_back(cmsg);
        return nullptr;
    } else {
      collective* coll = it->second;
      auto& queue = completion_queues_[cmsg->cq_id()];
      int old_queue_size = queue.size();
      coll->recv(cmsg);
      if (queue.size() != old_queue_size){
        //oh, a new collective finished
        message* dmsg = queue.back();
        queue.pop_back();
        return dmsg;
      } else {
        return nullptr;
      }
    }
  }
#ifdef FEATURE_TAG_SUMI_RESILIENCE
  case message::ping: {  
    monitor_->message_received(msg);
    return nullptr;
  }
#endif
  case message::no_class: {
      spkt_throw_printf(sprockit::value_error,
        "transport::handle: got message %s with no class of type %s",
        msg->to_string().c_str(),
        message::tostr(msg->payload_type()));
  }
  default: {
      spkt_throw_printf(sprockit::value_error,
        "transport::handle: got unknown message class %d",
        msg->class_type());
  }
  }
  return nullptr;
}

void
transport::system_bcast(message* msg)
{
  auto bmsg = dynamic_cast<system_bcast_message*>(msg);
  int root = bmsg->root();
  int my_effective_rank = (rank_ - root + nproc_) % nproc_;

  /**
   0->1
   0->2
   1->3
   0->4
   1->5
   2->6
   3->7
   ... and so on
  */

  int partner_gap = 1;
  int rank_check = rank_;
  while (rank_check > 0){
    partner_gap *= 2;
    rank_check /= 2;
  }

  int effective_target = my_effective_rank + partner_gap;
  while (effective_target < nproc_){
    int target = (effective_target + root) % nproc_;
    message* next_msg = new system_bcast_message(bmsg->action(), bmsg->root());
    send_header(target, next_msg, message::no_ack, message::default_cq);
    partner_gap *= 2;
    effective_target = my_effective_rank + partner_gap;
  }
}

void
transport::send_self_terminate()
{
  message* msg = new message();
  msg->set_class_type(message::terminate);
  send_header(rank_, msg, message::no_ack, message::default_cq); //send to self
}

transport::~transport()
{
#ifdef FEATURE_TAG_SUMI_RESILIENCE
  if (monitor_) delete monitor_;
#endif
  if (global_domain_) delete global_domain_;
}

void
transport::dynamic_tree_vote(int vote, int tag, vote_fxn fxn, collective::config cfg)
{
  if (cfg.dom == nullptr) cfg.dom = global_domain_;

  if (cfg.dom->nproc() == 1){
    auto dmsg = new collective_done_message(tag, collective::dynamic_tree_vote, cfg.dom, cfg.cq_id);
    dmsg->set_comm_rank(0);
    dmsg->set_vote(vote);
    vote_done(cfg.context, dmsg);
    return;
  }

  dynamic_tree_vote_collective* voter = new dynamic_tree_vote_collective(vote, fxn, tag, this, cfg);
  start_collective(voter);
}

template <class Map, class Val, class Key>
bool pull_from_map(Val& val, const Map& m, const Key& k)
{
  typedef typename Map::iterator iterator;
  iterator it = m.find(k);
  if (m.find(k) == m.end()){
    return false;
  } else {
    val = it->second;
    m.erase(it);
    return true;
  }
}

template <class Map, class Val, class Key1, class Key2>
bool
pull_from_map(Val& val, const Map& m, const Key1& k1, const Key2& k2)
{
  typedef typename Map::iterator iterator;
  iterator it = m.find(k1);
  if (it == m.end()) return false;

  bool ret = check_map(val, it->second, k2);
  if (it->second.empty()) m.erase(it);
  return ret;
}

template <class Map, class Val, class Key1, class Key2, class Key3>
bool
pull_from_map(Val& val, const Map& m, const Key1& k1, const Key2& k2, const Key3& k3)
{
  typedef typename Map::iterator iterator;
  iterator it = m.find(k1);
  if (it == m.end()) return false;

  bool ret = check_map(val, it->second, k2, k3);
  if (it->second.empty()) m.erase(it);
  return ret;
}


void
transport::deliver_pending(collective* coll, int tag, collective::type_t ty)
{
  std::list<collective_work_message*> pending = pending_collective_msgs_[ty][tag];
  pending_collective_msgs_[ty].erase(tag);
  std::list<collective_work_message*>::iterator it, end = pending.end();

  for (it = pending.begin(); it != end; ++it){
    collective_work_message* msg = *it;
    coll->recv(msg);
  }
}

void
transport::vote_done(int context, collective_done_message* dmsg)
{
#ifdef FEATURE_TAG_SUMI_RESILIENCE
  //this requires some extra processing
  //and doesn't always generate an operation done
  votes_done_[dmsg->tag()] = vote_result(dmsg->vote(), dmsg->failed_procs());
  vote_result& prev_context = votes_done_[context];
  //merge the previous context
  votes_done_[dmsg->tag()].failed_ranks.insert_all(prev_context.failed_ranks);
  failed_ranks_.insert_all(votes_done_[dmsg->tag()].failed_ranks);

  //if we have some failures, let the watchers know that things have failed
  thread_safe_set<int>::iterator it, end = dmsg->failed_procs().start_iteration();
  for (it=dmsg->failed_procs().begin(); it != end; ++it){
    fail_watcher(*it);
  }
  dmsg->failed_procs().end_iteration();

  if (is_heartbeat(dmsg)){
    dmsg->set_type(collective::heartbeat);
    if (heartbeat_active_){
      schedule_next_heartbeat();
    }
    heartbeat_running_ = false;
  } else
#endif
    completion_queues_[dmsg->cq_id()].push_back(dmsg);
}

void
transport::validate_collective(collective::type_t ty, int tag)
{
  tag_to_collective_map::iterator it = collectives_[ty].find(tag);
  if (it == collectives_[ty].end()){
    return; // all good
  }

  collective* coll = it->second;
  if (!coll){
   spkt_throw_printf(sprockit::illformed_error,
    "sumi_api::validate_collective: lingering null collective of type %s with tag %d",
    collective::tostr(ty), tag);
  }

  if (coll->persistent() && coll->complete()){
    return; // all good
  }

  spkt_throw_printf(sprockit::illformed_error,
    "sumi_api::validate_collective: cannot overwrite collective of type %s with tag %d",
    collective::tostr(ty), tag);
}

void
transport::start_collective(collective* coll)
{
  coll->init_actors();
  int tag = coll->tag();
  collective::type_t ty = coll->type();
  //validate_collective(ty, tag);
  collective*& existing = collectives_[ty][tag];
  if (existing){
    coll->start();
    existing->add_actors(coll);
    delete coll;
  } else {
    existing = coll;
    coll->start();
    deliver_pending(coll, tag, ty);
  }
}

void
transport::deadlock_check()
{
  collective_map::iterator it, end = collectives_.end();
  for (it=collectives_.begin(); it != end; ++it){
    tag_to_collective_map& next = it->second;
    tag_to_collective_map::iterator cit, cend = next.end();
    for (cit=next.begin(); cit != cend; ++cit){
      collective* coll = cit->second;
      if (!coll->complete()){
        coll->deadlock_check();
      }
    }
  }
}

bool
transport::skip_collective(collective::type_t ty,
  collective::config& cfg,
  void* dst, void *src,
  int nelems, int type_size,
  int tag)
{
  if (cfg.dom == nullptr) cfg.dom = global_domain_;

  if (cfg.dom->nproc() == 1){
    if (dst && src && (dst != src)){
      ::memcpy(dst, src, nelems*type_size);
    }
    auto dmsg = new collective_done_message(tag, ty, cfg.dom, cfg.cq_id);
    dmsg->set_comm_rank(0);
    dmsg->set_result(dst);
    completion_queues_[cfg.cq_id].push_back(dmsg);
    return true; //null indicates no work to do
  } else {
    return false;
  }
}

void
transport::allreduce(void* dst, void *src, int nelems, int type_size, int tag, reduce_fxn fxn,
                     collective::config cfg)
{
  if (skip_collective(collective::allreduce, cfg, dst, src, nelems, type_size, tag))
    return;

  dag_collective* coll = allreduce_selector_ == nullptr
      ? new wilke_halving_allreduce
      : allreduce_selector_->select(cfg.dom->nproc(), nelems);
  coll->init(collective::allreduce, this, dst, src, nelems, type_size, tag, cfg);
  coll->init_reduce(fxn);
  start_collective(coll);
}

void
transport::scan(void* dst, void* src, int nelems, int type_size, int tag, reduce_fxn fxn,
                collective::config cfg)
{
  if (skip_collective(collective::scan, cfg, dst, src, nelems, type_size, tag))
    return;

  dag_collective* coll = scan_selector_ == nullptr
      ? new simultaneous_btree_scan
      : scan_selector_->select(cfg.dom->nproc(), nelems);

  coll->init(collective::scan, this, dst, src, nelems, type_size, tag, cfg);
  coll->init_reduce(fxn);
  start_collective(coll);
}


void
transport::reduce(int root, void* dst, void *src, int nelems, int type_size, int tag, reduce_fxn fxn,
                  collective::config cfg)
{
  if (skip_collective(collective::reduce, cfg, dst, src, nelems, type_size, tag))
    return;

  dag_collective* coll = reduce_selector_ == nullptr
      ? new wilke_halving_reduce
      : reduce_selector_->select(cfg.dom->nproc(), nelems);
  coll->init(collective::reduce, this, dst, src, nelems, type_size, tag, cfg);
  coll->init_root(root);
  coll->init_reduce(fxn);
  start_collective(coll);
}

void
transport::bcast(int root, void *buf, int nelems, int type_size, int tag, collective::config cfg)
{
  if (skip_collective(collective::bcast, cfg, buf, buf, nelems, type_size, tag))
    return;

  dag_collective* coll = bcast_selector_ == nullptr
      ? new binary_tree_bcast_collective
      : bcast_selector_->select(cfg.dom->nproc(), nelems);

  coll->init(collective::bcast, this, buf, buf, nelems, type_size, tag, cfg);
  coll->init_root(root);
  start_collective(coll);
}

void
transport::gatherv(int root, void *dst, void *src,
                   int sendcnt, int *recv_counts,
                   int type_size, int tag,
                   collective::config cfg)
{
  if (skip_collective(collective::gatherv, cfg, dst, src, sendcnt, type_size, tag))
    return;

  dag_collective* coll = gatherv_selector_ == nullptr
      ? new btree_gatherv
      : gatherv_selector_->select(cfg.dom->nproc(), recv_counts);
  coll->init(collective::gatherv, this, dst, src, sendcnt, type_size, tag, cfg);
  coll->init_root(root);
  coll->init_recv_counts(recv_counts);
  sprockit::abort("gatherv");
  start_collective(coll);
}

void
transport::gather(int root, void *dst, void *src, int nelems, int type_size, int tag,
                  collective::config cfg)
{
  if (skip_collective(collective::gather, cfg, dst, src, nelems, type_size, tag))
    return;

  dag_collective* coll = gather_selector_ == nullptr
      ? new btree_gather
      : gather_selector_->select(cfg.dom->nproc(), nelems);

  coll->init(collective::gather, this, dst, src, nelems, type_size, tag, cfg);
  coll->init_root(root);
  start_collective(coll);
}

void
transport::scatter(int root, void *dst, void *src, int nelems, int type_size, int tag,
                   collective::config cfg)
{
  if (skip_collective(collective::scatter, cfg, dst, src, nelems, type_size, tag))
    return;

  dag_collective* coll = scatter_selector_ == nullptr
      ? new btree_scatter
      : scatter_selector_->select(cfg.dom->nproc(), nelems);

  coll->init(collective::scatter, this, dst, src, nelems, type_size, tag, cfg);
  coll->init_root(root);
  start_collective(coll);
}

void
transport::scatterv(int root, void *dst, void *src, int* send_counts, int recvcnt, int type_size, int tag,
                    collective::config cfg)
{
  if (skip_collective(collective::scatterv, cfg, dst, src, recvcnt, type_size, tag))
    return;

  dag_collective* coll = scatterv_selector_ == nullptr
      ? new btree_scatterv
      : scatterv_selector_->select(cfg.dom->nproc(), send_counts);

  coll->init(collective::scatterv, this, dst, src, recvcnt, type_size, tag, cfg);
  coll->init_root(root);
  coll->init_send_counts(send_counts);
  sprockit::abort("scatterv");
  start_collective(coll);
}

void
transport::alltoall(void *dst, void *src, int nelems, int type_size, int tag,
                    collective::config cfg)
{
  if (skip_collective(collective::alltoall, cfg, dst, src, nelems, type_size, tag))
    return;

  dag_collective* coll = alltoall_selector_ == nullptr
      ? new bruck_alltoall_collective
      : alltoall_selector_->select(cfg.dom->nproc(), nelems);

  coll->init(collective::alltoall, this, dst, src, nelems, type_size, tag, cfg);
  start_collective(coll);
}

void
transport::alltoallv(void *dst, void *src, int* send_counts, int* recv_counts, int type_size, int tag,
                     collective::config cfg)
{
  if (skip_collective(collective::alltoallv, cfg, dst, src, send_counts[0], type_size, tag))
    return;

  dag_collective* coll = alltoallv_selector_ == nullptr
      ? new direct_alltoallv_collective
      : alltoallv_selector_->select(cfg.dom->nproc(), send_counts);

  coll->init(collective::alltoallv, this, dst, src, 0, type_size, tag, cfg);
  coll->init_recv_counts(recv_counts);
  coll->init_send_counts(send_counts);
  start_collective(coll);
}

void
transport::allgather(void *dst, void *src, int nelems, int type_size, int tag,
                     collective::config cfg)
{
  if (skip_collective(collective::allgather, cfg, dst, src, nelems, type_size, tag))
    return;

  dag_collective* coll = allgather_selector_ == nullptr
      ? new bruck_allgather_collective
      : allgather_selector_->select(cfg.dom->nproc(), nelems);

  coll->init(collective::allgather, this, dst, src, nelems, type_size, tag, cfg);
  start_collective(coll);
}

void
transport::allgatherv(void *dst, void *src, int* recv_counts, int type_size, int tag,
                      collective::config cfg)
{
  //if the allgatherv is skipped, we have a single recv count
  int nelems = *recv_counts;
  if (skip_collective(collective::allgatherv, cfg, dst, src, nelems, type_size, tag))
    return;

  dag_collective* coll = allgatherv_selector_ == nullptr
      ? new bruck_allgatherv_collective
      : allgatherv_selector_->select(cfg.dom->nproc(), recv_counts);

  //for the time being, ignore size and use the small-message algorithm
  coll->init(collective::allgatherv, this, dst, src, 0, type_size, tag, cfg);
  coll->init_recv_counts(recv_counts);
  start_collective(coll);
}

void
transport::barrier(int tag, collective::config cfg)
{
  if (skip_collective(collective::barrier, cfg, 0, 0, 0, 0, tag))
    return;

  dag_collective* coll = new bruck_allgather_collective;
  coll->init(collective::barrier, this, 0, 0, 0, 0, tag, cfg);
  start_collective(coll);
}

void
transport::finish_collective(collective* coll, collective_done_message* dmsg)
{
  bool deliver_cq_msg; bool delete_collective;
  coll->actor_done(dmsg->comm_rank(), deliver_cq_msg, delete_collective);
  debug_printf(sprockit::dbg::sumi,
    "Rank %d finishing collective of type %s tag %d",
    rank_, collective::tostr(dmsg->type()), dmsg->tag());

  if (!deliver_cq_msg)
    return;

  coll->complete();
  collective::type_t ty = dmsg->type();
  int tag = dmsg->tag();
  if (delete_collective && !coll->persistent()){ //otherwise collective must exist FOREVER
    collectives_[ty].erase(tag);
    todel_.push_back(coll);
  }

  if (ty == collective::dynamic_tree_vote){
    vote_done(coll->context(), dmsg);
  } else {
    completion_queues_[dmsg->recv_cq()].push_back(dmsg);
  }


  pending_collective_msgs_[ty].erase(tag);
  debug_printf(sprockit::dbg::sumi,
    "Rank %d finished collective of type %s tag %d",
    rank_, collective::tostr(dmsg->type()), dmsg->tag());
}


void
transport::smsg_send(int dst, message::payload_type_t ev, message* msg,
                     int send_cq, int recv_cq)
{
  configure_send(dst, ev, msg);
  msg->set_send_cq(send_cq);
  msg->set_recv_cq(recv_cq);

  if (send_cq == -1 && recv_cq == -1) abort();

  debug_printf(sprockit::dbg::sumi,
    "Rank %d SUMI sending short message to %d, send ack %srequested ",
    rank_, dst,
    (msg->needs_send_ack() ? "" : "NOT "));

  if (dst == rank_) {
    //deliver to self
    debug_printf(sprockit::dbg::sumi,
      "Rank %d SUMI sending self message", rank_);

    if (msg->needs_recv_ack()){
      completion_queues_[msg->recv_cq()].push_back(msg);
    }
    if (msg->needs_send_ack()){
      message* ack = msg->clone(message::eager_payload_ack);
      ack->set_payload_type(message::eager_payload_ack);
      completion_queues_[msg->send_cq()].push_back(ack);
    }
  } else {
    do_smsg_send(dst, msg);
  }
#if SSTMAC_COMM_SYNC_STATS
  msg->set_time_sent(wall_time());
#endif
}

void
transport::rdma_get(int src, message* msg,
                    int send_cq, int recv_cq)
{
  if (send_cq == -1 && recv_cq == -1){
    spkt_abort_printf("no completion queues specified for message %s", msg->to_string().c_str());
  }

  configure_send(src, message::rdma_get, msg);
  msg->set_send_cq(send_cq);
  msg->set_recv_cq(recv_cq);
  do_rdma_get(src, msg);
}

void
transport::rdma_put(int dst, message* msg,
                    int send_cq, int recv_cq)
{
  configure_send(dst, message::rdma_put, msg);
  msg->set_send_cq(send_cq);
  msg->set_recv_cq(recv_cq);
  do_rdma_put(dst, msg);
}
void
transport::nvram_get(int src, message* msg)
{
  configure_send(src, message::nvram_get, msg);
  do_nvram_get(src, msg);
}

void
transport::configure_send(int dst, message::payload_type_t ev, message* msg)
{
  switch(ev)
  {
  //this is a bit weird here
  //we want to maintain notation of send/recv dst/src as in MPI
  //for an RDMA get the recver sends a request - thus it is sort of a sender
  //however we want to maintain the notion that the source of the message is where the data lives
  //thus the destination sends a request to the source
  case message::rdma_get:
    msg->set_sender(dst);
    msg->set_recver(rank_);
    break;
  default:
    msg->set_sender(rank_);
    msg->set_recver(dst);
    break;
  }
  msg->set_payload_type(ev);

  if (msg->class_type() == message::no_class){
    spkt_throw_printf(sprockit::value_error,
        "sending message %s with no class",
        msg->to_string().c_str());
  }
}

void
transport::send_header(int dst, message* msg, int send_cq, int recv_cq)
{
  smsg_send(dst, message::header, msg, send_cq, recv_cq);
}

void
transport::send_payload(int dst, message* msg, int send_cq, int recv_cq)
{
  smsg_send(dst, message::eager_payload, msg, send_cq, recv_cq);
}

#ifdef FEATURE_TAG_SUMI_RESILIENCE
void
transport::start_heartbeat(double interval)
{
  if (heartbeat_active_){
    spkt_throw_printf(sprockit::illformed_error,
        "sumi_api::start_heartbeat: heartbeat already active");
    return;
  }

  heartbeat_active_ = true;
  heartbeat_interval_ = interval;
  do_heartbeat(options::initial_context);
}

void
transport::stop_heartbeat()
{
  heartbeat_active_ = false;
}

void
transport::next_heartbeat()
{
  //because of weirdness in scheduling
  //we might get a heartbeat request after finalizing
  if (!finalized_)
    do_heartbeat(heartbeat_tag_);
}

void
transport::do_heartbeat(int prev_context)
{
  CHECK_IF_I_AM_DEAD(return);

  heartbeat_running_ = true;
  if (heartbeat_tag_ == heartbeat_tag_stop_){
    heartbeat_tag_ = heartbeat_tag_start_;
  }
  else {
    ++heartbeat_tag_;
  }

  if (nproc() == 1)
    return;

  int vote = 1;
  dynamic_tree_vote_collective* voter = new dynamic_tree_vote_collective(
    vote, &And<int>::op, heartbeat_tag_, this, global_domain_, prev_context);
  collectives_[collective::dynamic_tree_vote][heartbeat_tag_] = voter;
  voter->start();
  deliver_pending(voter, heartbeat_tag_, collective::dynamic_tree_vote);

}

void
transport::cancel_ping(int dst, timeout_function* func)
{
  monitor_->cancel_ping(dst, func);
}

bool
transport::ping(int dst, timeout_function* func)
{
  CHECK_IF_I_AM_DEAD(return false);
  validate_api();
  if (is_failed(dst)){
    return true;
  }
  else {
    monitor_->ping(dst, func);
    return false;
  }
}

void
transport::stop_watching(int dst, timeout_function* func)
{
  if (!lazy_watch_){
    cancel_ping(dst, func);
    return;
  }

  watcher_map::iterator it = watchers_.find(dst);
  if (it==watchers_.end()){
    spkt_throw_printf(sprockit::value_error,
      "transport not watching %d, cannot erase", dst);
  }
  function_set& fset = it->second;
  int refcount = fset.erase(func);
  if (refcount == 0){
    watchers_.erase(it);
  }
}

bool
transport::start_watching(int dst, timeout_function *func)
{
  if (!lazy_watch_){
    return ping(dst, func);
  }

  validate_api();
  if (is_failed(dst)){
    return true;
  }
  else {
    debug_printf(sprockit::dbg::sumi | sprockit::dbg::sumi_ping,
      "Rank %d start watching %d", rank_, dst);
    function_set& fset = watchers_[dst];
    fset.append(func);
    return false;
  }
}

void
transport::fail_watcher(int dst)
{
  if (!lazy_watch_)
    return;

  std::map<int, function_set>::iterator it = watchers_.find(dst);
  if (it == watchers_.end())
    return;

  debug_printf(sprockit::dbg::sumi | sprockit::dbg::sumi_ping,
    "Rank %d failing watcher for %d", rank_, dst);
  function_set& fset = it->second;
  fset.timeout_all_listeners(dst);
  watchers_.erase(dst);
}

void
transport::die()
{
  is_dead_ = true;
  go_die();
  throw terminate_exception();
}

void
transport::revive()
{
  is_dead_ = false;
  go_revive();
}

void
transport::renew_pings()
{
  monitor_->renew_pings(wall_time());
}

static const thread_safe_set<int> empty_set;

const thread_safe_set<int>&
transport::failed_ranks(int context) const
{
  if (context == options::initial_context){
    return empty_set;
  }

  auto it = votes_done_.find(context);
  if (it == votes_done_.end()){
    spkt_throw_printf(sprockit::value_error,
        "sumi_api::failed_rank: unknown or uncommitted context %d on rank %d",
        context, rank_);
  }
  return it->second.failed_ranks;
}
#endif

}
