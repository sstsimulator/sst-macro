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
#include <sstmac/common/stats/stat_collector.h>
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

RegisterDebugSlot(EventManager);

#define prll_debug(...) \
  debug_printf(sprockit::dbg::parallel, "LP %d: %s", rt_->me(), sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {

const GlobalTimestamp EventManager::no_events_left_time(
  std::numeric_limits<uint64_t>::max(), std::numeric_limits<uint64_t>::max());

class StopEvent : public ExecutionEvent
{
 public:
  virtual ~StopEvent() {}

  void execute(){
    cout0 << "--- STOP event -----" << std::endl;
    man_->stop();
  }

  StopEvent(EventManager* man) :
    man_(man)
  {
  }

 protected:
  EventManager* man_;

};


EventManager* EventManager::global = nullptr;

struct spin_up_config {
  EventManager* mgr;
  void* args;
  void(*fxn)(void*);
};

void runEventmanagerThread(void* argPtr)
{
  auto cfg = (spin_up_config*) argPtr;
  (*cfg->fxn)(cfg->args);
  cfg->mgr->spinDown();
}

EventManager::EventManager(SST::Params& params, ParallelRuntime *rt) :
  rt_(rt),
  nthread_(rt->nthread()),
  me_(rt->me()),
  nproc_(rt->nproc()),
  pendingSlot_(0),
  complete_(false),
  thread_id_(0),
  stopped_(false),
  interconn_(nullptr)
{
  active_stat_group_.push("default");
  for (int i=0; i < num_pendingSlots; ++i){
    pending_events_[i].resize(nthread_);
  }
  if (nthread_ == 0){
    sprockit::abort("Have zero worker threads! Cannot do any work");
  }
  SST::Params os_params = params.find_prefix_params("node").find_prefix_params("os");
  sw::StackAlloc::init(os_params);

  des_context_ = sw::ThreadContext::factory::getOptionalParam(
                  "context", sw::ThreadContext::defaultThreading(), os_params);

  sprockit::thread_stack_size<int>() = sw::StackAlloc::stacksize();

  //make sure there's a good bit of space
  pending_serialization_.reserve(1024);
}

EventManager::~EventManager()
{
  if (des_context_) delete des_context_;
}

void
EventManager::stop()
{
  for (ExecutionEvent* ev : event_queue_){
    delete ev;
  }
  event_queue_.clear();
  min_ipc_time_ = no_events_left_time;
  stopped_ = true;
}

GlobalTimestamp
EventManager::runEvents(GlobalTimestamp event_horizon)
{
  registerPending();
  min_ipc_time_ = no_events_left_time;
  while (!event_queue_.empty()){
    auto iter = event_queue_.begin();
    ExecutionEvent* ev = *iter;

    if (ev->time() < now_){
      spkt_abort_printf("Time went backwards on thread %d", thread_id_);
    }

    if (ev->time() >= event_horizon){
      GlobalTimestamp ret = std::min(min_ipc_time_, ev->time());
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

sw::ThreadContext*
EventManager::cloneThread() const
{
  return des_context_->copy();
}

void
EventManager::setInterconnect(hw::Interconnect* interconn)
{
  lookahead_ = interconn->lookahead();
  interconn_ = interconn;
}

int
EventManager::serializeSchedule(char* buf)
{
  serializer ser;
  uint32_t bigSize = 1 << 30;
  ser.start_unpacking(buf, bigSize); //just pass in a huge number
  IpcEvent iev;
  ParallelRuntime::runSerialize(ser, &iev);
  scheduleIncoming(&iev);
  size_t size = ser.unpacker().size();
  align64(size);
  return size;
}

void
EventManager::registerStatisticCore(StatisticBase* base)
{
  StatGroup& grp = stat_groups_[active_stat_group_.back()];
  grp.stats.push_back(base);
}

void
EventManager::finalizeStatsInit()
{
  for (auto& pair : stat_groups_){
    StatGroup& grp = pair.second;
    grp.output->startRegisterGroup(pair.first);
    for (auto* stat : grp.stats){
      grp.output->startRegisterFields(stat);
      stat->registerOutputFields(grp.output);
      grp.output->stopRegisterFields();
    }
    grp.output->stopRegisterGroup();
  }
}

void
EventManager::finalizeStatsOutput()
{
  for (auto& pair : stat_groups_){
    StatGroup& grp = pair.second;
    grp.output->startOutputGroup(pair.first);
    for (auto* stat : grp.stats){
      grp.output->startOutputEntries(stat);
      stat->outputStatisticData(grp.output, true/*only ever end of sim*/);
      grp.output->stopOutputEntries();
    }
    grp.output->stopOutputGroup();
  }
}

void
EventManager::scheduleIncoming(IpcEvent* iev)
{
  auto comp = interconn_->component(iev->dst);
  EventHandler* dst_handler = iev->credit ? comp->creditHandler(iev->port) : comp->payloadHandler(iev->port);
  prll_debug("thread %d scheduling incoming event at %12.8e to device %d:%s, %s",
    thread_id_, iev->t.sec(), iev->dst, dst_handler->toString().c_str(),
    sprockit::toString(iev->ev).c_str());
  auto qev = new HandlerExecutionEvent(iev->ev, dst_handler);
  qev->setSeqnum(iev->seqnum);
  qev->setTime(iev->t);
  qev->setLink(iev->link);
  schedule(qev);
}

void
EventManager::registerPending()
{
  for (char* buf : pending_serialization_){
    serializeSchedule(buf);
  }
  pending_serialization_.clear();

  int idx = 0;
  for (auto& pendingVec : pending_events_[pendingSlot_]){
    for (ExecutionEvent* ev : pendingVec){
      if (ev->time() < now_){
        spkt_abort_printf("Thread %d scheduling event on thread %d", idx, thread_id_);
      }
      schedule(ev);
    }
    pendingVec.clear();
    ++idx;
  }
  pendingSlot_ = (pendingSlot_+1) % num_pendingSlots;
}

static int nactive_threads = 0;
static thread_lock active_lock;

void
EventManager::spinUp(void(*fxn)(void*), void* args)
{
  active_lock.lock();
  ++nactive_threads;
  active_lock.unlock();
  
  void* stack = sw::StackAlloc::alloc();
  sstmac::ThreadInfo::registerUserSpaceVirtualThread(thread_id_, stack, nullptr, nullptr);
  main_thread_ = des_context_->copy();
  main_thread_->initContext();
  spin_up_config cfg;
  cfg.mgr = this;
  cfg.fxn = fxn;
  cfg.args = args;
  des_context_->startContext(stack, sw::StackAlloc::stacksize(),
                              &runEventmanagerThread, &cfg, main_thread_);
  delete main_thread_;
}

void
EventManager::spinDown()
{
  active_lock.lock();
  --nactive_threads;
  if (nactive_threads == 0){
    //delete here while we are still on a user-space thread
    //annoying but necessary
    hw::Interconnect::clear_staticInterconnect();
  }
  active_lock.unlock();
  des_context_->completeContext(main_thread_);
}

void
EventManager::run()
{
  interconn_->setup();

  finalizeStatsInit();

  registerPending();

  runEvents(no_events_left_time);

  final_time_ = now_;

  finalizeStatsOutput();
}

void
EventManager::ipcSchedule(IpcEvent* iev)
{
  rt_->sendEvent(iev);
}

void
EventManager::scheduleStop(GlobalTimestamp until)
{
  StopEvent* ev = new StopEvent(this);
  ev->setTime(until);
  ev->setSeqnum(0);
  event_queue_.insert(ev);
}

Partition*
EventManager::topologyPartition() const
{
  return rt_->topologyPartition();
}

/**
void
EventManager::registerStat(
  StatCollector* stat,
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
EventManager::finishStats(StatCollector* main, const std::string& name)
{
  stats_entry& entry = stats_[name];
  for (StatCollector* next : entry.collectors){
    next->finalize(now_);
    if (entry.dump_all)
      next->dumpLocalData();

    if (entry.reduce_all)
      main->reduce(next);

    next->clear();
  }
}

void
EventManager::finishUniqueStat(int unique_tag, stats_entry& entry)
{
  entry.main_collector->globalReduce(rt_);
  if (rt_->me() == 0) entry.main_collector->dumpGlobalData();
  delete entry.main_collector;
}
*/


void
EventManager::finishStats()
{
/**
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
        //see if any of the existing ones should be the main
        for (StatCollector* stat : entry.collectors){
          if (stat->isMain()){
            entry.main_collector = stat;
            break;
          }
        }
        if (!entry.main_collector){
          StatCollector* first = entry.collectors.front();
          entry.main_collector = first->clone();
          main_allocated = true;
        }
      }
    }

    finishStats(entry.main_collector, name);

    if (entry.reduce_all){
      entry.main_collector->globalReduce(rt_);
      if (rt_->me() == 0){
        entry.main_collector->dumpGlobalData();
      }
    }

    if (main_allocated) delete entry.main_collector;
    for (StatCollector* coll : entry.collectors){
      delete coll;
    }
  }

  for (auto& pair : unique_stats_){
    finishUniqueStat(pair.first, pair.second);
  }
*/
}

}
