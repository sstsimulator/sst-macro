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
    event_queue_entry(device_id::ctrl_event(), device_id::ctrl_event())
  {
  }

 protected:
  event_manager* man_;

};


event_manager* event_manager::global = nullptr;

event_manager::event_manager(sprockit::sim_parameters *params, parallel_runtime *rt) :
  rt_(rt),
  finish_on_stop_(true),
  stopped_(true),
  thread_id_(0),
  nthread_(rt->nthread()),
  me_(rt->me()),
  nproc_(rt->nproc()),
  complete_(false)
{
}

event_manager*
event_manager::ev_man_for_thread(int thread_id) const
{
  //kind of annoying I have to const cast this
  //this is a truly const function, though
  return const_cast<event_manager*>(this);
}

void
event_manager::ipc_schedule(timestamp t,
  device_id dst,
  device_id src,
  uint32_t seqnum,
  event* ev)
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "%s::ipc_schedule: not valid for chosen event manager");
}

void
event_manager::schedule_stop(timestamp until)
{
  event_queue_entry* stopper = new stop_event(this);
  schedule(until, 0, stopper);
}

void
event_manager::multithread_schedule(
    int srcthread,
    int dstthread,
    uint32_t seqnum,
    event_queue_entry* ev)
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "%s::multithread_schedule: not valid for chosen event manager");
}

partition*
event_manager::topology_partition() const
{
  return rt_->topology_partition();
}

void
event_manager::set_now(const timestamp &ts)
{
#if SSTMAC_SANITY_CHECK
  if (ts < now_) {
    spkt_throw_printf(sprockit::spkt_error,
                     "eventmanager::set_now - major error, setting time to %lld which is before now %lld",
                     (long long)ts.ticks_int64(), (long long)now_.ticks_int64());
  }
#endif
  now_ = ts;
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
  stat->set_registered(true);
}

void
event_manager::finish_stats(stat_collector* main, const std::string& name, timestamp t_end)
{
  stats_entry& entry = stats_[name];
  std::list<stat_collector*>::iterator it, end = entry.collectors.end();
  for (it=entry.collectors.begin(); it != end; ++it){
    stat_collector* next = *it;
    next->simulation_finished(t_end);
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

    finish_stats(entry.main_collector, name, now());

    if (entry.reduce_all){
      entry.main_collector->global_reduce(rt_);
      if (rt_->me() == 0){
        entry.main_collector->dump_global_data();
      }
    }

    if (main_allocated) delete entry.main_collector;
  }
}

}