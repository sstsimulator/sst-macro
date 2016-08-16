/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/common/event_manager.h>
#include <sstmac/common/sst_event.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sprockit/util.h>
#include <sprockit/output.h>

RegisterDebugSlot(event_manager);

ImplementFactory(sstmac::event_manager);

namespace sstmac {

class stop_event : public event_queue_entry
{
 public:
  virtual ~stop_event() {}

  void execute(){
    cout0 << "--- STOP event -----" << std::endl;
    man_->stop();
  }

  std::string
  to_string() const {
    return "stop event";
  }

  stop_event(event_manager* man) :
    man_(man),
    event_queue_entry(event_loc_id::null, event_loc_id::null)
  {
  }

 protected:
  event_manager* man_;

};

std::vector<pthread_t> event_manager::pthreads_;
std::vector<pthread_attr_t> event_manager::pthread_attrs_;
event_manager* event_manager::global = 0;


event_manager*
event_manager::ev_man_for_thread(int thread_id) const
{
  //kind of annoying I have to const cast this
  //this is a truly const function, though
  return const_cast<event_manager*>(this);
}

int
event_manager::current_thread_id()
{
  if (pthreads_.size() <= 1){
    return 0;
  }

  for (int i=1; i < pthreads_.size(); ++i){
    if (pthread_equal(pthread_self(), pthreads_[i])){
      return i;
    }
  }
  return 0;
}

void
event_manager::ipc_schedule(timestamp t,
  event_loc_id dst,
  event_loc_id src,
  uint32_t seqnum,
  event* ev)
{
  spkt_throw_printf(sprockit::unimplemented_error,
   "%s::ipc_schedule: not valid for chosen event manager",
   to_string().c_str());
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
    "%s::multithread_schedule: not valid for chosen event manager",
    to_string().c_str());
}

partition*
event_manager::topology_partition() const
{
  return rt_->topology_partition();
}

parallel_runtime*
event_manager::runtime() const
{
  return rt_;
}

void
event_manager::set_interconnect(hw::interconnect* interconn)
{
}

event_manager::~event_manager()
{
}

void
event_manager::init_factory_params(sprockit::sim_parameters* params)
{
  nproc_ = rt_->nproc();
  me_ = rt_->me();
  nthread_ = rt_->nthread();
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
  bool reduce_all,
  bool dump_all,
  bool dump_main)
{
  std::map<std::string, stats_entry>::iterator it = stats_.find(stat->fileroot());
  if (it != stats_.end()){
    stats_entry& entry = it->second;
    return entry.collectors.front();
  }

  //clone a stat collector for this thread
  stat_collector* cln = stat->clone();
  register_stat(cln, reduce_all, dump_all, dump_main);
  return cln;
}

void
event_manager::register_stat(
  stat_collector* stat,
  bool reduce_all,
  bool dump_all,
  bool dump_main)
{
  if (stat->registered())
    return;

  stats_entry& entry = stats_[stat->fileroot()];
  entry.collectors.push_back(stat);
  entry.reduce_all = reduce_all;
  entry.dump_all = dump_all;
  entry.dump_main = dump_main;
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

