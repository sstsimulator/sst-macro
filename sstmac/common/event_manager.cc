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

#include <sstmac/common/event_manager.h>
#include <sstmac/common/sst_event.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/software/threading/threading_interface.h>
#include <sstmac/software/threading/stack_alloc.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/handler_event_queue_entry.h>
#include <sprockit/util.h>
#include <sprockit/output.h>
#include <sprockit/thread_safe_new.h>
#include <limits>

RegisterDebugSlot(event_manager);

#define prll_debug(...) \
  debug_printf(sprockit::dbg::parallel, "LP %d: %s", rt_->me(), sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {

const timestamp event_manager::no_events_left_time(std::numeric_limits<int64_t>::max() - 100, timestamp::exact);

class stop_event : public event_queue_entry
{
 public:
  virtual ~stop_event() {}

  void execute(){
    cout0 << "--- STOP event -----" << std::endl;
    man_->stop();
  }

  stop_event(event_manager* man) :
    man_(man),
    event_queue_entry(-1, -1)
  {
  }

 protected:
  event_manager* man_;

};


event_manager* event_manager::global = nullptr;

struct spin_up_config {
  event_manager* mgr;
  void* args;
  void(*fxn)(void*);
};

void run_event_manager_thread(void* argPtr)
{
  auto cfg = (spin_up_config*) argPtr;
  (*cfg->fxn)(cfg->args);
  cfg->mgr->spin_down();
}

event_manager::event_manager(sprockit::sim_parameters *params, parallel_runtime *rt) :
  rt_(rt),
  nthread_(rt->nthread()),
  me_(rt->me()),
  nproc_(rt->nproc()),
  pending_slot_(0),
  complete_(false),
  thread_id_(0),
  stopped_(false),
  interconn_(nullptr)
{
  for (int i=0; i < num_pending_slots; ++i){
    pending_events_[i].resize(nthread_);
  }
  if (nthread_ == 0){
    sprockit::abort("Have zero worker threads! Cannot do any work");
  }
  sprockit::sim_parameters* os_params = params->get_optional_namespace("node")->get_optional_namespace("os");
  sw::stack_alloc::init(os_params);

  des_context_ = sw::thread_context::factory::get_optional_param(
                  "context", sw::thread_context::default_threading(), os_params);

  sprockit::thread_stack_size<int>() = sw::stack_alloc::stacksize();

  //make sure there's a good bit of space
  pending_serialization_.reserve(1024);
}

event_manager::~event_manager()
{
  if (des_context_) delete des_context_;
}

void
event_manager::stop()
{
  for (event_queue_entry* ev : event_queue_){
    delete ev;
  }
  event_queue_.clear();
  min_ipc_time_ = no_events_left_time;
  stopped_ = true;
}

timestamp
event_manager::run_events(timestamp event_horizon)
{
  register_pending();
  min_ipc_time_ = no_events_left_time;
  while (!event_queue_.empty()){
    auto iter = event_queue_.begin();
    event_queue_entry* ev = *iter;

    if (ev->time() < now_){
      spkt_abort_printf("Time went backwards on thread %d: %lu < %lu",
                        thread_id_, ev->time().ticks(), now_.ticks());
    }

    if (ev->time() >= event_horizon){
      timestamp ret = std::min(min_ipc_time_, ev->time());
      return ret;
    } else {
      now_ = ev->time();
      event_queue_.erase(iter);
      ev->execute();
      delete ev;
    }
  }
  return min_ipc_time_;
}

sw::thread_context*
event_manager::clone_thread() const
{
  return des_context_->copy();
}

void
event_manager::cancel_all_messages(uint32_t component_id)
{
  sprockit::abort("canceling messages not currently supported");
}

void
event_manager::set_interconnect(hw::interconnect* interconn)
{
  lookahead_ = interconn->lookahead();
  interconn_ = interconn;
}

int
event_manager::serialize_schedule(char* buf)
{
  serializer ser;
  uint32_t bigSize = 1 << 30;
  ser.start_unpacking(buf, bigSize); //just pass in a huge number
  ipc_event_t iev;
  parallel_runtime::run_serialize(ser, &iev);
  schedule_incoming(&iev);
  size_t size = ser.unpacker().size();
  align64(size);
  return size;
}

void
event_manager::schedule_incoming(ipc_event_t* iev)
{
  auto comp = interconn_->component(iev->dst);
  event_handler* dst_handler = iev->credit ? comp->credit_handler(iev->port) : comp->payload_handler(iev->port);
  prll_debug("thread %d scheduling incoming event at %12.8e to device %d:%s, payload? %d:   %s",
    thread_id_, iev->t.sec(), iev->dst, dst_handler->to_string().c_str(),
    iev->ev->is_payload(), sprockit::to_string(iev->ev).c_str());
  auto qev = new handler_event_queue_entry(iev->ev, dst_handler, iev->src);
  qev->set_seqnum(iev->seqnum);
  qev->set_time(iev->t);
  schedule(qev);
}

void
event_manager::register_pending()
{
  for (char* buf : pending_serialization_){
    serialize_schedule(buf);
  }
  pending_serialization_.clear();

  int idx = 0;
  for (auto& pendingVec : pending_events_[pending_slot_]){
    for (event_queue_entry* ev : pendingVec){
      if (ev->time() < now_){
        spkt_abort_printf("Thread %d scheduling event on thread %d in the past: %lu < %lu",
                          idx, thread_id_, ev->time().ticks(), now_.ticks());
      }
      schedule(ev);
    }
    pendingVec.clear();
    ++idx;
  }
  pending_slot_ = (pending_slot_+1) % num_pending_slots;
}

void
event_manager::spin_up(void(*fxn)(void*), void* args)
{
  void* stack = sw::stack_alloc::alloc();
  sstmac::thread_info::set_thread_id(stack, thread_id_);
  main_thread_ = des_context_->copy();
  main_thread_->init_context();
  spin_up_config cfg;
  cfg.mgr = this;
  cfg.fxn = fxn;
  cfg.args = args;
  des_context_->start_context(thread_id_, stack, sw::stack_alloc::stacksize(),
                              &run_event_manager_thread, &cfg, nullptr, nullptr,
                              main_thread_);
  delete main_thread_;
}

void
event_manager::spin_down()
{
  des_context_->complete_context(main_thread_);
}

void
event_manager::run()
{
  interconn_->setup();
  register_pending();

  run_events(no_events_left_time);

  final_time_ = now_;
}

void
event_manager::ipc_schedule(ipc_event_t* iev)
{
  rt_->send_event(iev);
}

void
event_manager::schedule_stop(timestamp until)
{
  stop_event* ev = new stop_event(this);
  ev->set_time(until);
  ev->set_seqnum(0);
  event_queue_.insert(ev);
}

partition*
event_manager::topology_partition() const
{
  return rt_->topology_partition();
}

static stat_descr_t default_descr;

void
event_manager::register_stat(
  stat_collector* stat,
  stat_descr_t* descr)
{
  if (stat->registered())
    return;

  if (!descr) descr = &default_descr;

  stats_entry& entry = stats_[stat->fileroot()];
  entry.collectors.push_back(stat);
  entry.reduce_all = descr->reduce_all;
  entry.dump_all = descr->dump_all;
  entry.dump_main = descr->dump_main;
  entry.need_delete = descr->need_delete;
  stat->set_registered(true);
}

void
event_manager::finish_stats(stat_collector* main, const std::string& name)
{
  stats_entry& entry = stats_[name];
  for (stat_collector* next : entry.collectors){
    if (entry.dump_all)
      next->dump_local_data();

    if (entry.reduce_all)
      main->reduce(next);

    next->clear();
  }
}

void
event_manager::register_unique_stat(stat_collector* stat, stat_descr_t* descr)
{
  stats_entry& entry = unique_stats_[descr->unique_tag->id];
  if (descr->dump_all){
    sprockit::abort("unique stat should not specify dump all");
  }
  if (!descr->reduce_all){
    sprockit::abort("unique stat should always specify reduce all");
  }
  entry.dump_all = false;
  entry.dump_main = descr->dump_main;
  entry.reduce_all = true;
  entry.main_collector = stat;
}

void
event_manager::finish_unique_stat(int unique_tag, stats_entry& entry)
{
  entry.main_collector->global_reduce(rt_);
  if (rt_->me() == 0) entry.main_collector->dump_global_data();
  delete entry.main_collector;
}


void
event_manager::finish_stats()
{
  std::map<std::string, stats_entry>::iterator it, end = stats_.end();
  for (it = stats_.begin(); it != end; ++it){
    std::string name = it->first;
    stats_entry& entry = it->second;
    bool main_allocated = false;
    if (entry.collectors.empty()){
      spkt_throw_printf(sprockit::value_error,
        "there is a stats slot named %s, but there are no collectors",
        name.c_str());
    }

    if (!entry.main_collector){
      if (entry.collectors.size() == 1){
        entry.main_collector = entry.collectors.front();
        entry.collectors.clear();
      } else {
        stat_collector* first = entry.collectors.front();
        entry.main_collector = first->clone();
        main_allocated = true;
      }
    }

    finish_stats(entry.main_collector, name);

    if (entry.reduce_all){
      entry.main_collector->global_reduce(rt_);
      if (rt_->me() == 0){
        entry.main_collector->dump_global_data();
      }
    }

    if (main_allocated) delete entry.main_collector;
    for (stat_collector* coll : entry.collectors){
      delete coll;
    }
  }

  for (auto& pair : unique_stats_){
    finish_unique_stat(pair.first, pair.second);
  }
}

}
