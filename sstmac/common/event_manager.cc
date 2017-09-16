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

#include <sstmac/common/event_manager.h>
#include <sstmac/common/sst_event.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sprockit/util.h>
#include <sprockit/output.h>

RegisterDebugSlot(event_manager);

namespace sstmac {

const timestamp event_manager::no_events_left_time(std::numeric_limits<int64_t>::max() - 100, timestamp::exact);

#if 0
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
#endif


event_manager* event_manager::global = nullptr;

event_manager::event_manager(sprockit::sim_parameters *params, parallel_runtime *rt) :
  rt_(rt),
  nthread_(rt->nthread()),
  me_(rt->me()),
  nproc_(rt->nproc()),
  complete_(false),
  thread_(0),
  scheduled_(false)
{
  incoming_events_ = &pending_events1_;
  outgoing_events_ = &pending_events2_;
  pending_events_.resize(nthread_);
}

timestamp
event_manager::run_events(timestamp event_horizon)
{
  register_pending();
  while (!event_queue_.empty()){
    auto iter = event_queue_.begin();
    event_queue_entry* ev = *iter;
    if (ev->time() >= event_horizon){
      return ev->time();
    } else {
      now_ = ev->time();
      ev->execute();
      delete ev;
      event_queue_.erase(iter);
    }
  }
  return no_events_left_time;
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

void
event_manager::register_pending()
{
  for (auto& pendingVec : *incoming_events_){
    for (event_queue_entry* ev : pendingVec){
      schedule(ev);
    }
  }
}

void
event_manager::run()
{
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
  sprockit::abort("cannot schedule stop times currently");
}

partition*
event_manager::topology_partition() const
{
  return rt_->topology_partition();
}

stat_collector*
event_manager::register_thread_unique_stat(
  stat_collector *stat,
  stat_descr_t* descr)
{
  std::map<std::string, stats_entry>::iterator it = stats_.find(stat->fileroot());
  if (it != stats_.end()){
    stats_entry& entry = it->second;
    return entry.collectors.front();
  }

  //clone a stat collector for this thread
  stat_collector* cln = stat->clone();
  register_stat(cln, descr);
  return cln;
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
}

}
