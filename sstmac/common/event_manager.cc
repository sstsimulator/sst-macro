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

#define __STDC_FORMAT_MACROS
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

#include <cinttypes>

RegisterDebugSlot(event_manager);

#define prll_debug(...) \
  debug_printf(sprockit::dbg::parallel, "LP %d: %s", rt_->me(), sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {

const Timestamp EventManager::no_events_left_time(0, std::numeric_limits<uint64_t>::max());

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
  for (int i=0; i < num_pendingSlots; ++i){
    pending_events_[i].resize(nthread_);
  }
  if (nthread_ == 0){
    sprockit::abort("Have zero worker threads! Cannot do any work");
  }
  SST::Params os_params = params.find_scoped_params("node").find_scoped_params("os");
  sw::StackAlloc::init(os_params);

  auto threading = os_params.find<std::string>("context", sw::ThreadContext::defaultThreading());
  des_context_ = sprockit::create<sw::ThreadContext>("macro", threading);

  sprockit::thread_stack_size<int>() = sw::StackAlloc::stacksize();

  //make sure there's a good bit of space
  pending_serialization_.reserve(1024);
}

EventManager::~EventManager()
{
  if (des_context_) delete des_context_;
  for (auto& pair : stat_groups_){
    StatisticGroup* grp = pair.second;
    for (auto* stat : grp->stats){
      if (stat) delete stat;
    }
    delete grp->output;
    delete grp;
  }
}

void
EventManager::addLinkHandler(uint64_t linkId, EventHandler *handler)
{
  if (linkId >= link_handlers_.size()){
    link_handlers_.resize(linkId + 1);
  }
  link_handlers_[linkId] = handler;
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

int
EventManager::epoch() const
{
  return rt_->epoch();
}

Timestamp
EventManager::runEvents(Timestamp event_horizon)
{
  registerPending();
  min_ipc_time_ = no_events_left_time;
  prll_debug("manager %d:%d running to horizon %10.5e with %llu events in queue on epoch %d",
             me_, thread_id_, event_horizon.sec(), event_queue_.size(), epoch());
  while (!event_queue_.empty()){
    auto iter = event_queue_.begin();
    ExecutionEvent* ev = *iter;

#if SSTMAC_SANITY_CHECK
    if (ev->time() < now_){
      spkt_abort_printf("Time went backwards on manager %d:%d to t=%10.6e for link=%" PRIu64 " for seqnum=%" PRIu32,
                        me_, thread_id_, ev->time().sec(), ev->linkId(), ev->seqnum());
    }
#endif

    if (ev->time() >= event_horizon){
      Timestamp ret = std::min(min_ipc_time_, ev->time());
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
EventManager::registerStatisticCore(StatisticBase* base, SST::Params& params)
{
  StatisticGroup* grp = stat_groups_[base->groupName()];
  if (!grp){
    grp = new StatisticGroup(base->groupName());
    stat_groups_[base->groupName()] = grp;
  }

  if (grp->outputName.empty()){
    //get the output name from the statistic
    grp->outputName = base->output();
  } else if (grp->outputName != base->output()){
    spkt_abort_printf("group %s requested output name %s does not match previously requested %s",
                      base->groupName().c_str(), grp->outputName.c_str(), base->output().c_str());
  }

  if (!grp->output){
    grp->output = sprockit::create<StatisticOutput>("macro", grp->outputName, params);
  }

  grp->stats.push_back(base);

  base->setGroup(grp);
  grp->output->registerStatistic(base);
}

void
EventManager::finalizeStatsInit()
{
}

void
EventManager::finalizeStatsOutput()
{
  for (auto& pair : stat_groups_){
    StatisticGroup* grp = pair.second;
    grp->output->startOutputGroup(grp);
    for (auto* stat : grp->stats){
      grp->output->output(stat, true);
      //stat->outputStatisticData(grp.output, true/*only ever end of sim*/);
      //grp.output->stopOutputEntries();
    }
    grp->output->stopOutputGroup();
  }
}

void
EventManager::scheduleIncoming(IpcEvent* iev)
{
#if SSTMAC_SANITY_CHECK
  if (iev->link >= link_handlers_.size()){
    spkt_abort_printf("Rank %d received event for bad link %" PRIu64,
                      me(), iev->link);
  }
#endif

  EventHandler* dst_handler = link_handlers_[iev->link];
  prll_debug("manager %d:%d scheduling incoming event on link=%" PRIu64 " seq=%" PRIu32 " at %12.8e on epoch %d",
             me_, thread_id_, iev->link, iev->seqnum, iev->t.sec(), epoch());
#if SSTMAC_SANITY_CHECK
  if (iev->t < now_){
    spkt_abort_printf("manager %d:%d got incoming event on link=%" PRIu64 " seq=%" PRIu32 " at t=%12.8e < now=%12.8e",
                      me_, thread_id_, iev->link, iev->seqnum, iev->t.sec(), now_.sec());
  }
  if (!dst_handler){
    spkt_abort_printf("Rank %d received event for null link %" PRIu64,
                      me(), iev->link);
  }
#endif
  auto qev = new HandlerExecutionEvent(iev->ev, dst_handler);
  qev->setSeqnum(iev->seqnum);
  qev->setTime(iev->t);
  qev->setLink(iev->link);
  size_t prev_size = event_queue_.size();
  schedule(qev);
#if SSTMAC_SANITY_CHECK
  if (event_queue_.size() == prev_size){
    spkt_abort_printf("event queue lost event while scheduling! identical events added on link %" PRIu64, iev->link);
  }
#endif
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
#if SSTMAC_SANITY_CHECK
      if (ev->time() < now_){
        spkt_abort_printf("Thread %d scheduling event in the past on thread %d", idx, thread_id_);
      }
#endif
      prll_debug("manager %d:%d scheduling event %" PRIu32 " from link %" PRIu64 " at t=%10.7e on epoch %d",
                 me_, thread_id_, ev->seqnum(), ev->linkId(), ev->time().sec(), epoch());
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
    hw::Interconnect::clearStaticInterconnect();
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
EventManager::scheduleStop(Timestamp until)
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


void
EventManager::finishStats()
{
}

}
