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
  inited_(false),
  finalized_(false),
  server_libname_(server_name),
  user_lib_time_(nullptr),
  spy_num_messages_(nullptr),
  spy_bytes_(nullptr),
  completion_queues_(1),
  cq_blocked_threads_(1)
{
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

  server->register_proc(rank_, this);

  spy_num_messages_ = sstmac::optional_stats<sstmac::stat_spyplot>(des_scheduler(),
        params, "traffic_matrix", "ascii", "num_messages");
  spy_bytes_ = sstmac::optional_stats<sstmac::stat_spyplot>(des_scheduler(),
        params, "traffic_matrix", "ascii", "bytes");
}

void
transport::make_engine()
{
  if (!engine_) engine_ = new collective_engine(params_, this);
}

int
transport::allocate_cq()
{
  uint8_t ret = completion_queues_.size();
  completion_queues_.emplace_back();
  cq_blocked_threads_.emplace_back();
  auto iter = held_.find(ret);
  if (iter != held_.end()){
    auto& list = iter->second;
    for (message* m : list){
      completion_queues_[ret].push_back(m);
    }
    held_.erase(iter);
  }
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
  //this should really loop through and kill off all the pings
  //so none of them execute
  finalized_ = true;
}

message*
transport::find_message()
{
  for (auto& queue : completion_queues_){
    if (!queue.empty()){
      message* ret = queue.front();
      queue.pop_front();
      return ret;
    }
  }
  return nullptr;
}

message*
transport::poll(bool blocking, double timeout)
{
  message* msg = find_message();
  if (!msg && blocking){
    block(blocked_threads_, timeout);
    return find_message();
  } else {
    return msg;
  }
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

  if (blocking){
    block(cq_blocked_threads_[cq_id], timeout);
    auto& queue = completion_queues_[cq_id];
    if (queue.empty()){ //oh, must have timed out
      return nullptr;
    } else {
      message* msg = queue.front();
      queue.pop_front();
      return msg;
    }
  } else {
    return nullptr;
  }
}

void
transport::block(std::list<sstmac::sw::thread*>& blocked, double timeout)
{
  sstmac::sw::thread* thr = os_->active_thread();
  blocked.push_back(thr);
  if (timeout >= 0.){
    os_->block_timeout(timeout);
  } else {
    os_->block();
  }
}

transport::~transport()
{
#ifdef FEATURE_TAG_SUMI_RESILIENCE
  if (monitor_) delete monitor_;
#endif

  delete user_lib_time_;
  sumi_server* server = safe_cast(sumi_server, os_->lib(server_libname_));
  bool del = server->unregister_proc(rank_, this);
  if (del) delete server;

  if (engine_) delete engine_;

  //if (spy_bytes_) delete spy_bytes_;
  //if (spy_num_messages_) delete spy_num_messages_;
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

void
transport::incoming_message(message *msg)
{
#if SSTMAC_COMM_SYNC_STATS
  if (msg){
    msg->get_payload()->set_time_arrived(wall_time());
  }
#endif

  int cq = msg->is_nic_ack() ? msg->send_cq() : msg->recv_cq();
  if (cq != message::no_ack){
    if (cq >= completion_queues_.size()){
      held_[cq].push_back(msg);
    } else {
      completion_queues_[cq].push_back(msg);
      auto& queue = cq_blocked_threads_[cq];
      if (!queue.empty()){
        sstmac::sw::thread* next_thr = queue.front();
        debug_printf(sprockit::dbg::sumi,
                     "sumi queue %p unblocking CQ %d poller %p to handle %s",
                      this, cq, next_thr, msg->to_string().c_str());
        queue.pop_front();
        os_->unblock(next_thr);
      } else if (!blocked_threads_.empty()){
        //there is someone blocked looking for something on any completion queue
        auto* next_thr = blocked_threads_.front();
        debug_printf(sprockit::dbg::sumi,
                     "sumi queue %p poller %p to handle %s",
                      this, next_thr, msg->to_string().c_str());
        blocked_threads_.pop_front();
        os_->unblock(next_thr);
      }
    }
  }
}

collective_engine::collective_engine(sprockit::sim_parameters *params, transport *tport) :
  system_collective_tag_(-1), //negative tags reserved for special system work
  eager_cutoff_(512),
  use_put_protocol_(false),
  global_domain_(nullptr),
  tport_(tport)
{
  global_domain_ = new global_communicator(tport);
  eager_cutoff_ = params->get_optional_int_param("eager_cutoff", 512);
  use_put_protocol_ = params->get_optional_bool_param("use_put_protocol", false);
}

collective_engine::~collective_engine()
{
  if (global_domain_) delete global_domain_;
}

void
collective_engine::notify_collective_done(int rank, collective::type_t ty, int tag)
{
  collective* coll = collectives_[ty][tag];
  if (!coll){
    spkt_throw_printf(sprockit::value_error,
      "transport::notify_collective_done: invalid collective of type %s, tag %d",
       collective::tostr(ty), tag);
  }
  finish_collective(coll, rank, ty, tag);
}

void
collective_engine::deadlock_check()
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

collective_done_message*
collective_engine::skip_collective(collective::type_t ty,
  int cq_id, communicator* comm,
  void* dst, void *src,
  int nelems, int type_size,
  int tag)
{
  if (!comm) comm = global_domain_;
  if (comm->nproc() == 1){
    if (dst && src && (dst != src)){
      ::memcpy(dst, src, nelems*type_size);
    }
    return new collective_done_message(tag, ty, comm, cq_id);
  } else {
    return nullptr;
  }
}

collective_done_message*
collective_engine::allreduce(void* dst, void *src, int nelems, int type_size, int tag, reduce_fxn fxn,
                             int cq_id, communicator* comm)
{
 auto* msg = skip_collective(collective::allreduce, cq_id, comm, dst, src, nelems, type_size, tag);
 if (msg) return msg;

  if (!comm) comm = global_domain_;
  dag_collective* coll = new wilke_halving_allreduce(this, dst, src, nelems, type_size, tag, fxn, cq_id, comm);
  return start_collective(coll);
}

sumi::collective_done_message*
collective_engine::reduce_scatter(void* dst, void *src, int nelems, int type_size, int tag, reduce_fxn fxn,
                                  int cq_id, communicator* comm)
{
  auto* msg = skip_collective(collective::reduce_scatter, cq_id, comm, dst, src, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  dag_collective* coll = new halving_reduce_scatter(this, dst, src, nelems, type_size, tag, fxn, cq_id, comm);
  return start_collective(coll);
}

sumi::collective_done_message*
collective_engine::scan(void* dst, void* src, int nelems, int type_size, int tag, reduce_fxn fxn,
                        int cq_id, communicator* comm)
{
  auto* msg = skip_collective(collective::scan, cq_id, comm, dst, src, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  dag_collective* coll = new simultaneous_btree_scan(this, dst, src, nelems, type_size, tag, fxn, cq_id, comm);
  return start_collective(coll);
}


collective_done_message*
collective_engine::reduce(int root, void* dst, void *src, int nelems, int type_size, int tag, reduce_fxn fxn,
                          int cq_id, communicator* comm)
{
  auto* msg = skip_collective(collective::reduce, cq_id, comm, dst, src, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  dag_collective* coll = new wilke_halving_reduce(this, root, dst, src, nelems, type_size, tag, fxn, cq_id, comm);
  return start_collective(coll);
}

collective_done_message*
collective_engine::bcast(int root, void *buf, int nelems, int type_size, int tag,
                         int cq_id, communicator* comm)
{
  auto* msg = skip_collective(collective::bcast, cq_id, comm, buf, buf, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  dag_collective* coll = new binary_tree_bcast_collective(this, root, buf, nelems, type_size, tag, cq_id, comm);
  return start_collective(coll);
}

collective_done_message*
collective_engine::gatherv(int root, void *dst, void *src,
                   int sendcnt, int *recv_counts,
                   int type_size, int tag, int cq_id, communicator* comm)
{
  auto* msg = skip_collective(collective::gatherv, cq_id, comm, dst, src, sendcnt, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  dag_collective* coll = new btree_gatherv(this, root, dst, src, sendcnt, recv_counts, type_size, tag, cq_id, comm);
  sprockit::abort("gatherv");
  return start_collective(coll);
}

collective_done_message*
collective_engine::gather(int root, void *dst, void *src, int nelems, int type_size, int tag,
                          int cq_id, communicator* comm)
{
  auto* msg = skip_collective(collective::gather, cq_id, comm, dst, src, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  dag_collective* coll = new btree_gather(this, root, dst, src, nelems, type_size, tag, cq_id, comm);
  return start_collective(coll);
}

collective_done_message*
collective_engine::scatter(int root, void *dst, void *src, int nelems, int type_size, int tag,
                           int cq_id, communicator* comm)
{
  auto* msg = skip_collective(collective::scatter, cq_id, comm, dst, src, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  dag_collective* coll = new btree_scatter(this, root, dst, src, nelems, type_size, tag, cq_id, comm);
  return start_collective(coll);
}

collective_done_message*
collective_engine::scatterv(int root, void *dst, void *src, int* send_counts, int recvcnt, int type_size, int tag,
                            int cq_id, communicator* comm)
{
  auto* msg = skip_collective(collective::scatterv, cq_id, comm, dst, src, recvcnt, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  dag_collective* coll = new btree_scatterv(this, root, dst, src, send_counts, recvcnt, type_size, tag, cq_id, comm);
  sprockit::abort("scatterv");
  return start_collective(coll);
}

collective_done_message*
collective_engine::alltoall(void *dst, void *src, int nelems, int type_size, int tag,
                            int cq_id, communicator* comm)
{
  auto* msg = skip_collective(collective::alltoall, cq_id, comm, dst, src, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  dag_collective* coll = new bruck_alltoall_collective(this, dst, src, nelems, type_size, tag, cq_id, comm);
  return start_collective(coll);
}

collective_done_message*
collective_engine::alltoallv(void *dst, void *src, int* send_counts, int* recv_counts, int type_size, int tag,
                             int cq_id, communicator* comm)
{
  auto* msg = skip_collective(collective::alltoallv, cq_id, comm, dst, src, send_counts[0], type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  dag_collective* coll = new direct_alltoallv_collective(this, dst, src, send_counts, recv_counts, type_size, tag, cq_id, comm);
  return start_collective(coll);
}

collective_done_message*
collective_engine::allgather(void *dst, void *src, int nelems, int type_size, int tag,
                             int cq_id, communicator* comm)
{
 auto* msg = skip_collective(collective::allgather, cq_id, comm, dst, src, nelems, type_size, tag);
 if (msg) return msg;

  if (!comm) comm = global_domain_;
  dag_collective* coll = new bruck_allgather_collective(
        collective::allgather, this, dst, src, nelems, type_size, tag, cq_id, comm);
  return start_collective(coll);
}

collective_done_message*
collective_engine::allgatherv(void *dst, void *src, int* recv_counts, int type_size, int tag,
                              int cq_id, communicator* comm)
{
  //if the allgatherv is skipped, we have a single recv count
  int nelems = *recv_counts;
  auto* msg = skip_collective(collective::allgatherv, cq_id, comm, dst, src, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  dag_collective* coll = new bruck_allgatherv_collective(this, dst, src, recv_counts, type_size, tag, cq_id, comm);
  return start_collective(coll);
}

collective_done_message*
collective_engine::barrier(int tag, int cq_id, communicator* comm)
{
  auto* msg = skip_collective(collective::barrier, cq_id, comm, 0, 0, 0, 0, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  dag_collective* coll = new bruck_allgather_collective(collective::barrier, this, nullptr, nullptr, 0, 0, tag, cq_id, comm);
  return start_collective(coll);
}

collective_done_message*
collective_engine::deliver_pending(collective* coll, int tag, collective::type_t ty)
{
  std::list<collective_work_message*> pending = pending_collective_msgs_[ty][tag];
  pending_collective_msgs_[ty].erase(tag);
  std::list<collective_work_message*>::iterator it, end = pending.end();

  collective_done_message* dmsg = nullptr;
  for (it = pending.begin(); it != end; ++it){
    collective_work_message* msg = *it;
    dmsg = coll->recv(msg);
  }
  return dmsg;
}

void
collective_engine::validate_collective(collective::type_t ty, int tag)
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

collective_done_message*
collective_engine::start_collective(collective* coll)
{
  coll->init_actors();
  int tag = coll->tag();
  collective::type_t ty = coll->type();
  //validate_collective(ty, tag);
  collective*& existing = collectives_[ty][tag];
  if (existing){
    coll->start();
    auto* msg = existing->add_actors(coll);
    delete coll;
    return msg;
  } else {
    existing = coll;
    coll->start();
    return deliver_pending(coll, tag, ty);
  }
}

void
collective_engine::finish_collective(collective* coll, int rank, collective::type_t ty, int tag)
{
  bool deliver_cq_msg; bool delete_collective;
  coll->actor_done(rank, deliver_cq_msg, delete_collective);
  debug_printf(sprockit::dbg::sumi,
    "Rank %d finishing collective of type %s tag %d",
    tport_->rank(), ty, tag);

  if (!deliver_cq_msg)
    return;

  coll->complete();
  if (delete_collective && !coll->persistent()){ //otherwise collective must exist FOREVER
    collectives_[ty].erase(tag);
    todel_.push_back(coll);
  }

  pending_collective_msgs_[ty].erase(tag);
  debug_printf(sprockit::dbg::sumi,
    "Rank %d finished collective of type %s tag %d",
    tport_->rank(), ty, tag);
}

void
collective_engine::wait_barrier(int tag)
{
  if (tport_->nproc() == 1) return;
  barrier(tag, message::default_cq);
  auto* dmsg = block_until_next(message::default_cq);
}

void
collective_engine::clean_up()
{
  for (collective* coll : todel_){
    delete coll;
  }
  todel_.clear();
}

collective_done_message*
collective_engine::incoming(message* msg)
{
  clean_up();

  collective_work_message* cmsg = dynamic_cast<collective_work_message*>(msg);
  if (cmsg->send_cq() == -1 && cmsg->recv_cq() == -1){
    spkt_abort_printf("both CQs are invalid for %s", msg->to_string().c_str())
  }
  int tag = cmsg->tag();
  collective::type_t ty = cmsg->type();
  tag_to_collective_map::iterator it = collectives_[ty].find(tag);
  if (it == collectives_[ty].end()){
    debug_printf(sprockit::dbg::sumi_collective,
      "Rank %d, queuing %p %s from %d on tag %d for type %s",
      tport_->rank(), msg,
      message::tostr(msg->class_type()),
      msg->sender(),
      tag, collective::tostr(ty));
      //message for collective we haven't started yet
      pending_collective_msgs_[ty][tag].push_back(cmsg);
      return nullptr;
  } else {
    collective* coll = it->second;
    auto* dmsg = coll->recv(cmsg);
    return dmsg;
  }
}

collective_done_message*
collective_engine::block_until_next(int cq_id)
{
  collective_done_message* dmsg = nullptr;
  while (dmsg == nullptr){
    auto* msg = tport_->blocking_poll(cq_id);
    dmsg = incoming(msg);
  }
  return dmsg;
}


}
