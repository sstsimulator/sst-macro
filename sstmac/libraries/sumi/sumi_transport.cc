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

#include <sstmac/common/sstmac_config.h>
#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/event.h>
#endif
#include <sstmac/libraries/sumi/sumi_transport.h>
#include <sstmac/libraries/sumi/message.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sumi/message.h>
#include <sprockit/output.h>

using namespace sprockit::dbg;

RegisterKeywords(
{ "post_rdma_delay", "the time it takes to post an RDMA operation" },
{ "post_header_delay", "the time it takes to send an eager message" },
{ "poll_delay", "the time it takes to poll for an incoming message" },
{ "rdma_pin_latency", "the latency for each RDMA pin information" },
{ "rdma_page_delay", "the per-page delay for RDMA pinning" },
);

namespace sstmac {

class sumi_server :
  public sstmac::sw::service
{

 public:
  sumi_server(sumi_transport* tport)
    : service(tport->server_libname(),
       sstmac::sw::software_id(-1, -1), //belongs to no application
       tport->os())
  {
  }

  void register_proc(int rank, sumi_transport* proc){
    int app_id = proc->sid().app_;
    debug_printf(sprockit::dbg::sumi,
                 "sumi_server registering rank %d for app %d",
                 rank, app_id);
    sumi_transport*& slot = procs_[app_id][rank];
    if (slot){
      spkt_abort_printf("sumi_server: already registered rank %d for app %d on node %d",
                        rank, app_id, os_->addr());
    }
    slot = proc;

    auto iter = pending_.begin();
    auto end = pending_.end();
    while (iter != end){
      auto tmp = iter++;
      transport_message* msg = *tmp;
      if (msg->dest_rank() == rank && msg->dest_app() == proc->sid().app_){
        pending_.erase(tmp);
        proc->incoming_message(msg);
      }
    }
  }

  bool unregister_proc(int rank, sumi_transport* proc){
    int app_id = proc->sid().app_;
    auto iter = procs_.find(app_id);
    auto& subMap = iter->second;
    subMap.erase(rank);
    if (subMap.empty()){
      procs_.erase(iter);
    }
    return procs_.empty();
  }

  void incoming_event(event *ev){
    transport_message* smsg = safe_cast(transport_message, ev);
    debug_printf(sprockit::dbg::sumi,
                 "sumi_server %d: incoming message %s",
                 os_->addr(), smsg->get_payload()->to_string().c_str());
    sumi_transport* tport = procs_[smsg->dest_app()][smsg->dest_rank()];
    if (!tport){
      pending_.push_back(smsg);
    } else {
      tport->incoming_message(smsg);
    }
  }

 private:
  std::map<int, std::map<int, sumi_transport*> > procs_;
  std::list<transport_message*> pending_;

};

sumi_transport::sumi_transport(sprockit::sim_parameters* params,
               const char* prefix,
               sstmac::sw::software_id sid,
               sstmac::sw::operating_system* os) :
  sumi_transport(params, standard_lib_name(prefix, sid), sid, os)
{
}

sumi_transport::sumi_transport(sprockit::sim_parameters* params,
               sstmac::sw::software_id sid,
               sstmac::sw::operating_system* os) :
  sumi_transport(params, "sumi", sid, os)
{
}


sumi_transport::sumi_transport(sprockit::sim_parameters* params,
    const std::string& libname, sstmac::sw::software_id sid,
    sstmac::sw::operating_system* os) :
  //the name of the transport itself should be mapped to a unique name
  api(params, libname, sid, os),
  //the server is what takes on the specified libname
  server_libname_("sumi_server"),
  transport(params),
  user_lib_time_(nullptr),
  spy_num_messages_(nullptr),
  spy_bytes_(nullptr),
  collective_cq_id_(1), //this gets assigned elsewhere
  pt2pt_cq_id_(0) //put pt2pt sends on the default cq
{
  collective_cq_id_ = allocate_cq();
  rank_ = sid.task_;
  library* server_lib = os_->lib(server_libname_);
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
sumi_transport::pin_rdma(uint64_t bytes)
{
  int num_pages = bytes / page_size_;
  if (bytes % page_size_) ++num_pages;
  sstmac::timestamp pin_delay = rdma_pin_latency_ + num_pages*rdma_page_delay_;
  compute(pin_delay);
}

void
sumi_transport::ctor_common(sstmac::sw::software_id sid)
{
  rank_ = sid.task_;
  sw::thread* thr = sw::operating_system::current_thread();
  sw::app* my_app = safe_cast(sw::app, thr);
  my_app->compute(timestamp(1e-6));
}

sumi_transport::~sumi_transport()
{
  delete user_lib_time_;
  sumi_server* server = safe_cast(sumi_server, os_->lib(server_libname_));
  bool del = server->unregister_proc(rank_, this);
  if (del) delete server;

  //if (spy_bytes_) delete spy_bytes_;
  //if (spy_num_messages_) delete spy_num_messages_;
}

event_scheduler*
sumi_transport::des_scheduler() const
{
  return os_->node();
}

void
sumi_transport::memcopy(uint64_t bytes)
{
  os_->current_thread()->parent_app()->compute_block_memcpy(bytes);
}

void
sumi_transport::incoming_event(event *ev)
{
  spkt_abort_printf("sumi_transport::incoming_event: should not directly handle events");
}

void
sumi_transport::shutdown_server(int dest_rank, node_id dest_node, int dest_app)
{
  auto msg = new sumi::system_bcast_message(sumi::system_bcast_message::shutdown, dest_rank);
  client_server_send(dest_rank, dest_node, dest_app, msg);
}

void
sumi_transport::client_server_send(
  int dst_task,
  node_id dst_node,
  int dst_app,
  sumi::message* msg)
{
  msg->set_send_cq(sumi::message::no_ack);
  msg->set_recv_cq(sumi::message::default_cq);
  send(msg->byte_length(),
       dst_task, dst_node, dst_app, msg,
       hw::network_message::payload);
}

void
sumi_transport::client_server_rdma_put(
  int dst_task,
  node_id dst_node,
  int dst_app,
  sumi::message* msg)
{
  msg->set_send_cq(sumi::message::no_ack);
  msg->set_recv_cq(sumi::message::default_cq);
  send(msg->byte_length(),
       dst_task, dst_node, dst_app, msg,
       hw::network_message::rdma_put_payload);
}

void
sumi_transport::process(sstmac::transport_message* smsg)
{
  sumi::message* my_msg = smsg->get_payload();
  debug_printf(sprockit::dbg::sumi,
     "sumi transport rank %d in app %d processing message of type %s: %s",
     rank(), sid().app_, transport_message::tostr(smsg->type()), my_msg->to_string().c_str());
  switch (smsg->type())
  {
   //depending on the type, we might have to mutate the incoming message
   case sstmac::hw::network_message::failure_notification:
    my_msg->set_payload_type(sumi::message::failure);
    break;
   case sstmac::hw::network_message::rdma_get_nack:
    my_msg->set_payload_type(sumi::message::rdma_get_nack);
    break;
   case sstmac::hw::network_message::payload_sent_ack:
    my_msg->set_payload_type(sumi::message::eager_payload_ack);
    break;
   case sstmac::hw::network_message::rdma_get_sent_ack:
    my_msg->set_payload_type(sumi::message::rdma_get_ack);
    break;
   case sstmac::hw::network_message::rdma_put_sent_ack:
    my_msg->set_payload_type(sumi::message::rdma_put_ack);
    break;
   default:
    break; //do nothing
  }
}

void
sumi_transport::init()
{
  transport::init();
}

void
sumi_transport::finish()
{
  debug_printf(sprockit::dbg::sumi, "Rank %d finalizing", rank_);
  transport::finish();
}

int*
sumi_transport::nidlist() const
{
  //just cast an int* - it's fine
  //the types are the same size and the bits can be
  //interpreted correctly
  return (int*) rank_mapper_->rank_to_node().data();
}

void
sumi_transport::send(
  uint64_t byte_length,
  sumi::message* msg,
  int sendType,
  int dst_rank)
{
  node_id dst_node = rank_mapper_->rank_to_node(dst_rank);
  send(byte_length, dst_rank, dst_node, sid().app_, msg, sendType);
}

void
sumi_transport::send(
  uint64_t byte_length,
  int dst_task,
  node_id dst_node,
  int dst_app,
  sumi::message* msg,
  int ty)
{
  sstmac::sw::app_id aid = sid().app_;
  transport_message* tmsg = new transport_message(server_libname_, aid, msg, byte_length);
  tmsg->hw::network_message::set_type((hw::network_message::type_t)ty);
  tmsg->toaddr_ = dst_node;
  tmsg->set_src_rank(rank_);
  tmsg->set_dest_rank(dst_task);
  //send intra-app
  tmsg->set_apps(sid().app_, dst_app);
  tmsg->set_needs_ack(msg->needs_send_ack());

  if (spy_num_messages_) spy_num_messages_->add_one(rank_, dst_task);
  if (spy_bytes_) spy_bytes_->add(rank_, dst_task, byte_length);

  sw::library::os_->execute(ami::COMM_SEND, tmsg);
}

void
sumi_transport::compute(timestamp t)
{
  user_lib_time_->compute(t);
}

void
sumi_transport::go_die()
{
  os_->kill_node();
}

void
sumi_transport::go_revive()
{
  sprockit::abort("SST cannot revive a dead process currently");
}

void
sumi_transport::do_smsg_send(int dst, sumi::message* msg)
{
  if (post_header_delay_.ticks_int64()) {
    user_lib_time_->compute(post_header_delay_);
  }
  send(msg->byte_length(), msg,
    sstmac::hw::network_message::payload, dst);
}

double
sumi_transport::wall_time() const
{
  return now().sec();
}

void
sumi_transport::do_rdma_get(int dst, sumi::message* msg)
{
  if (post_rdma_delay_.ticks_int64()) {
    user_lib_time_->compute(post_rdma_delay_);
  }

  send(msg->byte_length(), msg,
    sstmac::hw::network_message::rdma_get_request, dst);
}

void
sumi_transport::do_rdma_put(int dst, sumi::message* msg)
{
  if (post_rdma_delay_.ticks_int64()) {
    user_lib_time_->compute(post_rdma_delay_);
  }
  send(msg->byte_length(), msg,
    sstmac::hw::network_message::rdma_put_payload, dst);
}

void
sumi_transport::do_nvram_get(int dst, sumi::message* msg)
{
  send(msg->byte_length(), msg,
    sstmac::hw::network_message::nvram_get_request, dst);
}

sumi::transport_message*
sumi_transport::poll_pending_messages(bool blocking, double timeout)
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
  transport_message* msg = pending_messages_.front();
  debug_printf(sprockit::dbg::sumi,
               "Rank %d sumi queue %p has no pending messages - blocking poller %p",
               rank(), this, thr);
  pending_messages_.pop_front();
  process(msg);

  debug_printf(sprockit::dbg::sumi,
               "rank %d polling on %p returned msg %s",
               rank(), this, msg->to_string().c_str());

  return msg;
}

sumi::collective_done_message*
sumi_transport::collective_block(sumi::collective::type_t ty, int tag, int cq_id)
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
sumi_transport::incoming_message(transport_message *msg)
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
                  this, next_thr, msg->get_payload()->to_string().c_str());
    blocked_threads_.pop_front();
    os_->unblock(next_thr);
  } else {
    debug_printf(sprockit::dbg::sumi,
                 "sumi queue %p has no pollers to unblock for message %s",
                  this, msg->get_payload()->to_string().c_str());
  }
}

void
sumi_transport::send_terminate(int dst)
{
  //make a no-op
}

#ifdef FEATURE_TAG_SUMI_RESILIENCE
void
sumi_transport::send_ping_request(int dst)
{
  sumi::message* msg = new sumi::message>();
  msg->set_class_type(sumi::message::ping);
  //here, a simple rdma get
  rdma_get(dst, msg);
}

void
sumi_transport::schedule_next_heartbeat()
{
  schedule_delay(heartbeat_interval_,
         new_callback(loc_, this, &sumi_transport::next_heartbeat));
}

void
sumi_transport::schedule_ping_timeout(sumi::pinger* pnger, double to)
{
  sstmac::timestamp next_ping_time = api::now() + to;
  sstmac::callback* cb_event = sstmac::new_callback(
        loc_, this, &sumi_transport::ping_timeout, pnger);
  api::schedule(next_ping_time, cb_event);
}

void
sumi_transport::ping_timeout(sumi::pinger* pnger)
{
  pnger->execute();
}
#endif

}
