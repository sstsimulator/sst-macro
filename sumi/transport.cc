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
#include <sumi/reduce_scatter.h>
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
#include <sstmac/common/event_callback.h>
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
{ "post_rdma_delay", "the time it takes to post an RDMA operation" },
{ "post_header_delay", "the time it takes to send an eager message" },
{ "poll_delay", "the time it takes to poll for an incoming message" },
{ "rdma_pin_latency", "the latency for each RDMA pin information" },
{ "rdma_page_delay", "the per-page delay for RDMA pinning" },
);

#include <sstmac/common/sstmac_config.h>
#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/event.h>
#endif
#include <sumi/transport.h>
#include <sumi/message.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sprockit/output.h>

using namespace sprockit::dbg;

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
collective_algorithm_selector* transport::reduce_scatter_selector_ = nullptr;
collective_algorithm_selector* transport::scan_selector_ = nullptr;
collective_algorithm_selector* transport::scatter_selector_ = nullptr;
collective_algorithm_selector* transport::scatterv_selector_ = nullptr;

class sumi_server :
  public sstmac::sw::service
{

 public:
  sumi_server(transport* tport)
    : service(tport->server_libname(),
       sstmac::sw::software_id(-1, -1), //belongs to no application
       tport->os())
  {
  }

  void register_proc(int rank, transport* proc){
    int app_id = proc->sid().app_;
    debug_printf(sprockit::dbg::sumi,
                 "sumi_server registering rank %d for app %d",
                 rank, app_id);
    transport*& slot = procs_[app_id][rank];
    if (slot){
      spkt_abort_printf("sumi_server: already registered rank %d for app %d on node %d",
                        rank, app_id, os_->addr());
    }
    slot = proc;

    auto iter = pending_.begin();
    auto end = pending_.end();
    while (iter != end){
      auto tmp = iter++;
      message* msg = *tmp;
      if (msg->target_rank() == rank && msg->aid() == proc->sid().app_){
        pending_.erase(tmp);
        proc->incoming_message(msg);
      }
    }
  }

  bool unregister_proc(int rank, transport* proc){
    int app_id = proc->sid().app_;
    auto iter = procs_.find(app_id);
    auto& subMap = iter->second;
    subMap.erase(rank);
    if (subMap.empty()){
      procs_.erase(iter);
    }
    return procs_.empty();
  }

  void incoming_event(sstmac::event *ev){
    message* smsg = safe_cast(message, ev);
    debug_printf(sprockit::dbg::sumi,
                 "sumi_server %d: incoming %s",
                 os_->addr(), smsg->to_string().c_str());
    transport* tport = procs_[smsg->aid()][smsg->target_rank()];
    if (!tport){
      debug_printf(sprockit::dbg::sumi,
                  "sumi_server %d: message pending to app %d, target %d",
                  os_->addr(), smsg->aid(), smsg->target_rank());
      pending_.push_back(smsg);
    } else {
      tport->incoming_message(smsg);
    }
  }

 private:
  std::map<int, std::map<int, transport*> > procs_;
  std::list<message*> pending_;

};

transport::transport(sprockit::sim_parameters* params,
                               sstmac::sw::software_id sid,
                               sstmac::sw::operating_system* os) :
 transport(params, "sumi", sid, os)
{
}

transport::transport(sprockit::sim_parameters* params,
               const char* prefix,
               sstmac::sw::software_id sid,
               sstmac::sw::operating_system* os) :
  transport(params, standard_lib_name(prefix, sid), sid, os)
{
}

transport::transport(sprockit::sim_parameters* params,
               sstmac::sw::software_id sid,
               sstmac::sw::operating_system* os,
               const std::string& prefix,
               const std::string& server_name) :
  transport(params, standard_lib_name(prefix.c_str(), sid), sid, os, server_name)
{
}

transport::transport(sprockit::sim_parameters* params,
                     const std::string& libname,
                     sstmac::sw::software_id sid,
                     sstmac::sw::operating_system* os,
                     const std::string& server_name) :
  //the name of the transport itself should be mapped to a unique name
  api(params, libname, sid, os),
  //the server is what takes on the specified libname
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
  global_domain_(nullptr),
  server_libname_(server_name),
  user_lib_time_(nullptr),
  spy_num_messages_(nullptr),
  spy_bytes_(nullptr),
  collective_cq_id_(1), //this gets assigned elsewhere
  pt2pt_cq_id_(0) //put pt2pt sends on the default cq
{

  collective_cq_id_ = allocate_cq();
  rank_ = sid.task_;
  auto* server_lib = os_->lib(server_libname_);
  sumi_server* server;
  // only do one server per app per node
  if (server_lib == nullptr) {
    server = new sumi_server(this);
    server->start();
  } else {
    server = safe_cast(sumi_server, server_lib);
  }

  post_rdma_delay_ = params->get_optional_time_param("post_rdma_delay", 0);
  post_header_delay_ = params->get_optional_time_param("post_header_delay", 0);
  poll_delay_ = params->get_optional_time_param("poll_delay", 0);
  user_lib_time_ = new sstmac::sw::lib_compute_time(params, "sumi-user-lib-time", sid, os);

  rdma_pin_latency_ = params->get_optional_time_param("rdma_pin_latency", 0);
  rdma_page_delay_ = params->get_optional_time_param("rdma_page_delay", 0);
  pin_delay_ = rdma_pin_latency_.ticks() || rdma_page_delay_.ticks();
  page_size_ = params->get_optional_byte_length_param("rdma_page_size", 4096);

  rank_mapper_ = sstmac::sw::task_mapping::global_mapping(sid.app_);
  nproc_ = rank_mapper_->nproc();
  component_id_ = os_->component_id();

  global_domain_ = new global_communicator(this);

  server->register_proc(rank_, this);

  spy_num_messages_ = sstmac::optional_stats<sstmac::stat_spyplot>(des_scheduler(),
        params, "traffic_matrix", "ascii", "num_messages");
  spy_bytes_ = sstmac::optional_stats<sstmac::stat_spyplot>(des_scheduler(),
        params, "traffic_matrix", "ascii", "bytes");

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
transport::init()
{
  //THIS SHOULD ONLY BE CALLED AFTER RANK and NPROC are known
  inited_ = true;
}

void
transport::finish()
{
  clean_up();
  //this should really loop through and kill off all the pings
  //so none of them execute
  finalized_ = true;
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
#if SSTMAC_SANITY_CHECK
        if (msg->class_type() == message::collective){
          spkt_abort_printf("collective message should never be pushed back on CQ");
        }
#endif
        completion_queues_[msg->cq_id()].push_back(msg);
      }
    }
    //if I got here and am non-blocking, return null
    if (!blocking || timeout > 0) return nullptr;
  }
}

message*
transport::poll_new(bool blocking, double timeout)
{
  debug_printf(sprockit::dbg::sumi, "Rank %d polling for NEW message", rank_);
  while (1){
    message* tmsg = poll_pending_messages(blocking, timeout);
    if (tmsg){
      message* cq_notifier = handle(tmsg);
      if (cq_notifier || !blocking){
        return cq_notifier;
      }
    } else {
      return nullptr;
    }
  }
}

message*
transport::poll_pending_messages(bool blocking, double timeout)
{
  if (poll_delay_.ticks_int64()) {
    user_lib_time_->compute(poll_delay_);
  }

  sstmac::sw::thread* thr = os_->active_thread();
  if (pending_messages_.empty() && blocking) {
    blocked_threads_.push_back(thr);
    debug_printf(sprockit::dbg::sumi,
                 "Rank %d sumi queue %p has no pending messages, blocking poller %p: %s",
                 rank(), this, thr, timeout >= 0. ? "with timeout" : "no timeout");
    if (timeout >= 0.){
      os_->block_timeout(timeout);
    } else {
      os_->block();
    }
    if (timeout <= 0){
      if (pending_messages_.empty()){
        spkt_abort_printf("SUMI transport rank %d unblocked with no messages and no timeout",
          rank_, timeout);
      }
    } else {
      if (pending_messages_.empty()){
        //timed out, erase blocker from list
        auto iter = blocked_threads_.begin();
        while (*iter != thr) ++iter;
        blocked_threads_.erase(iter);
        return nullptr;
      }
    }
  } else if (pending_messages_.empty()){
    return nullptr;
  }

  //we have been unblocked because a message has arrived
  message* msg = pending_messages_.front();
  debug_printf(sprockit::dbg::sumi,
               "Rank %d sumi queue %p has no pending messages - blocking poller %p",
               rank(), this, thr);
  pending_messages_.pop_front();

  debug_printf(sprockit::dbg::sumi,
               "rank %d polling on %p returned msg %s",
               rank(), this, msg->to_string().c_str());

  return msg;
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
  for (collective* coll : todel_){
    delete coll;
  }
  todel_.clear();
}

message*
transport::handle(message* msg)
{
  debug_printf(sprockit::dbg::sumi,
    "Rank %d got message %s %p of class %s for sender %d, recver %d, target %d, send cq %d, recv cq %d",
     rank_, msg->sstmac::hw::network_message::type_str(), msg,
     message::tostr(msg->class_type()),
     msg->sender(), msg->recver(), msg->target_rank(),
     msg->send_cq(), msg->recv_cq());

  //we might have collectives to delete and cleanup
  if (!todel_.empty()){
    clean_up();
  }

  switch (msg->class_type())
  {
  case message::bcast:
    spkt_abort_printf("sumi::transport: cannot do system bcast");
    //root initiated global broadcast of some metadata
    //system_bcast(msg);
    return msg;
  case message::terminate:
  case message::collective_done:
  case message::pt2pt: {
    switch (msg->sstmac::hw::network_message::type()){
      case sstmac::hw::network_message::rdma_get_payload:
      case sstmac::hw::network_message::rdma_put_payload:
      case sstmac::hw::network_message::payload:
        msg->write_sync_value();
        if (msg->needs_recv_ack()){
          return msg;
        }
        break;
      case sstmac::hw::network_message::rdma_get_sent_ack:
      case sstmac::hw::network_message::rdma_put_sent_ack:
      case sstmac::hw::network_message::payload_sent_ack:
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
    if (cmsg->send_cq() == -1 && cmsg->recv_cq() == -1){
      spkt_abort_printf("both CQs are invalid for %s", msg->to_string().c_str())
    }
    int tag = cmsg->tag();
    collective::type_t ty = cmsg->type();
    tag_to_collective_map::iterator it = collectives_[ty].find(tag);
    if (it == collectives_[ty].end()){
      debug_printf(sprockit::dbg::sumi_collective_sendrecv,
        "Rank %d, queuing %p %s from %d on tag %d for type %s",
        rank_, msg,
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
        "transport::handle: got message %s with no class",
        msg->to_string().c_str());
  }
  default: {
      spkt_throw_printf(sprockit::value_error,
        "transport::handle: got unknown message class %d",
        msg->class_type());
  }
  }
  return nullptr;
}

#if 0
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
#endif

void
transport::send_self_terminate()
{
  smsg_send<message>(rank_, 0, nullptr,
                     message::no_ack, message::default_cq,
                     message::terminate);
}

transport::~transport()
{
#ifdef FEATURE_TAG_SUMI_RESILIENCE
  if (monitor_) delete monitor_;
#endif
  if (global_domain_) delete global_domain_;

  delete user_lib_time_;
  sumi_server* server = safe_cast(sumi_server, os_->lib(server_libname_));
  bool del = server->unregister_proc(rank_, this);
  if (del) delete server;

  //if (spy_bytes_) delete spy_bytes_;
  //if (spy_num_messages_) delete spy_num_messages_;
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
transport::reduce_scatter(void* dst, void *src, int nelems, int type_size, int tag, reduce_fxn fxn,
                     collective::config cfg)
{
  if (skip_collective(collective::reduce_scatter, cfg, dst, src, nelems, type_size, tag))
    return;

  dag_collective* coll = reduce_scatter_selector_ == nullptr
      ? new halving_reduce_scatter
      : reduce_scatter_selector_->select(cfg.dom->nproc(), nelems);
  coll->init(collective::reduce_scatter, this, dst, src, nelems, type_size, tag, cfg);
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
transport::pin_rdma(uint64_t bytes)
{
  int num_pages = bytes / page_size_;
  if (bytes % page_size_) ++num_pages;
  sstmac::timestamp pin_delay = rdma_pin_latency_ + num_pages*rdma_page_delay_;
  compute(pin_delay);
}

sstmac::event_scheduler*
transport::des_scheduler() const
{
  return os_->node();
}

void
transport::memcopy(uint64_t bytes)
{
  os_->current_thread()->parent_app()->compute_block_memcpy(bytes);
}

void
transport::incoming_event(sstmac::event *ev)
{
  spkt_abort_printf("sumi_transport::incoming_event: should not directly handle events");
}

void
transport::shutdown_server(int dest_rank, sstmac::node_id dest_node, int dest_app)
{
  spkt_abort_printf("transport::shutdown_server");
  /**
  auto msg = new sumi::system_bcast_message(sumi::system_bcast_message::shutdown, dest_rank);
  client_server_send(dest_rank, dest_node, dest_app, msg);
  */
}

void
transport::client_server_send(
  int dst_task,
  sstmac::node_id dst_node,
  int dst_app,
  sumi::message* msg)
{
  spkt_abort_printf("transport::client_server_send");
  /**
  msg->set_send_cq(sumi::message::no_ack);
  msg->set_recv_cq(sumi::message::default_cq);
  send(msg->byte_length(),
       dst_task, dst_node, dst_app, msg,
       sstmac::hw::network_message::payload);
  */
}

void
transport::client_server_rdma_put(
  int dst_task,
  sstmac::node_id dst_node,
  int dst_app,
  sumi::message* msg)
{
  spkt_abort_printf("transport::client_rdma_put");
  /**
  msg->set_send_cq(sumi::message::no_ack);
  msg->set_recv_cq(sumi::message::default_cq);
  send(msg->byte_length(),
       dst_task, dst_node, dst_app, msg,
       sstmac::hw::network_message::rdma_put_payload);
  */
}

int*
transport::nidlist() const
{
  //just cast an int* - it's fine
  //the types are the same size and the bits can be
  //interpreted correctly
  return (int*) rank_mapper_->rank_to_node().data();
}

void
transport::compute(sstmac::timestamp t)
{
  user_lib_time_->compute(t);
}

double
transport::wall_time() const
{
  return now().sec();
}

void
transport::send(message* m)
{
#if SSTMAC_COMM_SYNC_STATS
  msg->set_time_sent(wall_time());
#endif
  if (spy_num_messages_) spy_num_messages_->add_one(m->sender(), m->recver());
  if (spy_bytes_){
    switch(m->sstmac::hw::network_message::type()){
    case sstmac::hw::network_message::payload:
      spy_bytes_->add(m->sender(), m->recver(), m->byte_length());
      break;
    case sstmac::hw::network_message::rdma_get_request:
    case sstmac::hw::network_message::rdma_put_payload:
      spy_bytes_->add(m->sender(), m->recver(), m->payload_bytes());
      break;
    default:
      break;
    }
  }

  switch(m->sstmac::hw::network_message::type()){
    case sstmac::hw::network_message::payload:
      if (m->recver() == rank_){
        //deliver to self
        debug_printf(sprockit::dbg::sumi,
          "Rank %d SUMI sending self message", rank_);
        if (m->needs_recv_ack()){
          os_->send_now_self_event_queue(sstmac::new_callback(os_->component_id(), this, &transport::incoming_message, m));
        }
        if (m->needs_send_ack()){
          auto* ack = m->clone_injection_ack();
          os_->send_now_self_event_queue(
            sstmac::new_callback(os_->component_id(), this, &transport::incoming_message, static_cast<message*>(ack)));
        }
      } else {
        if (post_header_delay_.ticks_int64()) {
          user_lib_time_->compute(post_header_delay_);
        }
        sstmac::sw::library::os_->execute(sstmac::ami::COMM_SEND, m);
      }
      break;
    case sstmac::hw::network_message::rdma_get_request:
    case sstmac::hw::network_message::rdma_put_payload:
      if (post_rdma_delay_.ticks_int64()) {
        user_lib_time_->compute(post_rdma_delay_);
      }
      sstmac::sw::library::os_->execute(sstmac::ami::COMM_SEND, m);
      break;
    default:
      spkt_abort_printf("attempting to initiate send with invalid type %d",
                        m->type())
  }
}

void
transport::smsg_send_response(message* m, uint64_t size, void* buffer, int local_cq, int remote_cq)
{
  //reverse both hardware and software info
  m->sstmac::hw::network_message::reverse();
  m->reverse();
  m->setup_smsg(buffer, size);
  m->set_send_cq(local_cq);
  m->set_recv_cq(remote_cq);
  m->sstmac::hw::network_message::set_type(message::payload);
  send(m);
}

void
transport::rdma_get_request_response(message* m, uint64_t size,
                                     void* local_buffer, void* remote_buffer,
                                     int local_cq, int remote_cq)
{
  //do not reverse send/recver - this is hardware reverse, not software reverse
  m->sstmac::hw::network_message::reverse();
  m->setup_rdma_get(local_buffer, remote_buffer, size);
  m->set_send_cq(remote_cq);
  m->set_recv_cq(local_cq);
  m->sstmac::hw::network_message::set_type(message::rdma_get_request);
  send(m);
}

void
transport::rdma_get_response(message* m, uint64_t size, int local_cq, int remote_cq)
{
  smsg_send_response(m, size, nullptr, local_cq, remote_cq);
}

void
transport::rdma_put_response(message* m, uint64_t payload_bytes,
                 void* loc_buffer, void* remote_buffer, int local_cq, int remote_cq)
{
  m->reverse();
  m->sstmac::hw::network_message::reverse();
  m->setup_rdma_put(loc_buffer, remote_buffer, payload_bytes);
  m->set_send_cq(local_cq);
  m->set_recv_cq(remote_cq);
  m->sstmac::hw::network_message::set_type(message::rdma_put_payload);
  send(m);
}

uint64_t
transport::allocate_flow_id()
{
  return os_->node()->allocate_unique_id();
}

sumi::collective_done_message*
transport::collective_block(sumi::collective::type_t ty, int tag, int cq_id)
{
  //first we have to loop through the completion queue to see if it already exists
  while(1){
    auto& collective_cq = completion_queues_[collective_cq_id_];
    auto end = collective_cq.end();
    for (auto it=collective_cq.begin(); it != end; ++it){
      sumi::message* msg = *it;
      if (msg->class_type() == sumi::message::collective_done){
        //this is a collective done message
        auto cmsg = dynamic_cast<sumi::collective_done_message*>(msg);
        if (tag == cmsg->tag() && ty == cmsg->type()){  //done!
          collective_cq.erase(it);
          return cmsg;
        }
      }
    }

    sumi::message* msg = poll_new(true); //blocking
    if (msg->class_type() == sumi::message::collective_done){
      //this is a collective done message
      auto cmsg = dynamic_cast<sumi::collective_done_message*>(msg);
      if (tag == cmsg->tag() && ty == cmsg->type()){  //done!
        return cmsg;
      }
      collective_cq.push_back(msg);
    } else {
      completion_queues_[pt2pt_cq_id_].push_back(msg);
    }
  }
}

void
transport::incoming_message(message *msg)
{
#if SSTMAC_COMM_SYNC_STATS
  if (msg){
    msg->get_payload()->set_time_arrived(wall_time());
  }
#endif
  pending_messages_.push_back(msg);

  if (!blocked_threads_.empty()) {
    sstmac::sw::thread* next_thr = blocked_threads_.front();
    debug_printf(sprockit::dbg::sumi,
                 "sumi queue %p unblocking poller %p to handle message %s",
                  this, next_thr, msg->to_string().c_str());
    blocked_threads_.pop_front();
    os_->unblock(next_thr);
  } else {
    debug_printf(sprockit::dbg::sumi,
                 "sumi queue %p has no pollers to unblock for message %s",
                  this, msg->to_string().c_str());
  }
}


}
