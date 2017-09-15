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

#include <sstmac/common/sstmac_config.h>
#if !SSTMAC_INTEGRATED_SST_CORE

#include <sstmac/backends/native/multithreaded_event_container.h>
#include <unistd.h>
#include <execinfo.h>
#include <dlfcn.h>
#include <signal.h>
#include <iostream>
#include <sstream>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sprockit/keyword_registration.h>

RegisterDebugSlot(multithread_event_manager);
RegisterDebugSlot(cpu_affinity);

RegisterKeywords(
  "cpu_affinity",
);

namespace sstmac {
namespace native {

static void run_thread_iteration(thread_queue* q, int32_t numToRun)
{
  q->minTime = event_scheduler::no_events_left_time;
  for (int i=0; i < numToRun; ++i){
    event_scheduler* next = q->get(i);
    next->set_thread(q->threadId);
    q->mgr->run_scheduler(q->threadId, next, q->timeHorizon, q->minTime);
    next->set_scheduled(false);
  }
  q->clear();
}

static void*
pthread_run_worker_thread(void* args)
{
  thread_queue* q = (thread_queue*) args;
  bool done = false;
  while(!done){
    bool hasTask = q->query();
    if (hasTask){
      run_thread_iteration(q,*q->numTasks);
      int32_t marker = 1;
      int32_t newValue = 0;
      //swap to zero to signal to parent we are done
      OSAtomicCompareAndSwap32(marker, newValue, q->signalToParentWaveDone);
    }
    done = OSAtomicCompareAndSwap32(0, 1, q->signalFromParentAllDone);
  }
  return 0;
}

static void
print_backtrace(int sig)
{
  void* array[40];
  char cmd[1024];
  char debug[1024];
  int size = backtrace(array, 40);
  std::stringstream sstr;
  for (int i=0; i < size; ++i){
    void* addr = array[i];
    Dl_info info;
    int err = dladdr(addr, &info);
    sstr << sprockit::printf("backtrace[%2d] = %p : %s %p\n",
                  i, addr, info.dli_fname, info.dli_fbase);
  }
  std::cout << sstr.str() << std::endl;
  sleep(1);
  exit(1);
}

multithreaded_event_container::multithreaded_event_container(
  sprockit::sim_parameters* params, parallel_runtime* rt) :
  clock_cycle_event_map(params, rt)
{
  //set the signal handler
  signal(SIGSEGV, print_backtrace);
  signal(SIGABRT, print_backtrace);
  signal(SIGBUS, print_backtrace);
  signal(SIGFPE, print_backtrace);
  signal(SIGILL, print_backtrace);

  int nthread_ = nthread();
  me_ = rt_->me();
  nproc_ = rt_->nproc();
  if (params->has_param("cpu_affinity")) {
    params->get_vector_param("cpu_affinity", cpu_affinity_);
    //it would be nice to check that size of cpu_offsets matches task per node
  }

  pending_events_.resize(nthread_);
  queues_.resize(nthread_);
  pthreads_.resize(nthread_);
  pthread_attrs_.resize(nthread_);

  for (int i=0; i < queues_.size(); ++i){
    queues_[i].threadId = i;
    queues_[i].mgr = this;
  }

  for (int i=1; i < nthread_; ++i){
    int status = pthread_attr_init(&pthread_attrs_[i]);
    if (status != 0){
      sprockit::abort("multithreaded_event_container::run: failed creating pthread attributes");
    }
  }
}
void
multithreaded_event_container::register_pending()
{
  for (auto& eventVec : pending_events_){
    for (auto& evPair : eventVec){
      event_scheduler* es = evPair.second;
      event_queue_entry* ev = evPair.first;
      es->schedule(ev->time(), ev);
    }
    eventVec.clear();
  }
  event_manager::register_pending();
}

void
multithreaded_event_container::run_loop()
{
  register_pending();

  timestamp lower_bound;

  std::vector<thread_queue*> active_queues;
  active_queues.reserve(nthread_);
  //always active
  queues_[0].active = true;

  while (lower_bound != event_scheduler::no_events_left_time){
    timestamp horizon = lower_bound + lookahead_;
    timestamp min_time = min_registry_time();
    int queue_rotater = 0;
    while (min_time < horizon){
      auto iter = registry_.begin();
      event_scheduler* es = iter->second;
      if (min_time < lower_bound){
        spkt_abort_printf("time went backwards on component %p: %lu < %lu",
                          es, min_time.ticks(), lower_bound.ticks());
      }
      registry_.erase(iter);
      if (!es->scheduled()){
        //int thr = nthread_-1-queue_rotater%nthread_;
        //es->set_thread(thr);
        int thr = es->thread();
        es->set_scheduled(true);
        thread_queue& q = queues_[thr];
        q.append(es);
        q.timeHorizon = horizon;
        if (!q.active){
          q.active = true;
          active_queues.push_back(&q);
        }
        //loop backwards through the queues in round robin
        //minimize the work on thread 0 (main thread)
        //queue_rotater++;
      }
      min_time = min_registry_time();
    }
    /**
    int numActiveThreads = std::min(nthread_, queue_rotater);
    int iterStop = numActiveThreads;
    if (numActiveThreads == nthread_) iterStop--;

    for (int i=1; i <= iterStop; ++i){
      thread_queue& q = queues_[nthread_-i];
      int32_t numTasks = q.to_run.size();
      //increment - this will set worker running
      OSAtomicAdd32(numTasks, q.numTasks);
    }
    */
    for (thread_queue* q : active_queues){
      int32_t numTasks = q->to_run.size();
      //increment - this will set worker running
      OSAtomicAdd32(numTasks, q->numTasks);
    }

    thread_queue& qZero = queues_[0];
    run_thread_iteration(&qZero, qZero.to_run.size());
    //critical here to take min with min_time
    //it could be that the minimum time is something waiting in registry_
    //these times are not considered when finding min in the next wave
    lower_bound = std::min(min_time, qZero.minTime);
    /**
    for (int i=1; i <= iterStop; ++i){
      //thread_queue& q = queues_[nthread_-i];
    */
    for (thread_queue* q : active_queues){
      int32_t marker = 0;
      int32_t newValue = 1;
      bool done = false;
      while (!done){
        //child will set signal to zero - check and set to 1 for completion
        done = OSAtomicCompareAndSwap32(marker, newValue, q->signalToParentWaveDone);
      }
      q->active = false;
      lower_bound = std::min(q->minTime, lower_bound);
    }
    active_queues.clear();
    lower_bound = receive_incoming_events(lower_bound);
    register_pending();
  }
}

void
multithreaded_event_container::run()
{
  int nthread_ = nthread();
  debug_printf(sprockit::dbg::event_manager,
    "starting %d event manager threads",
    nthread_);
 
#if SSTMAC_USE_CPU_AFFINITY
  if (!cpu_affinity_.size()) {
    sprockit::abort("cpu_affinity array is required with cpu affinity enabled");
  }
  int proc_per_node = cpu_affinity_.size();
  int task_affinity = cpu_affinity_[me_ % proc_per_node];

  debug_printf(sprockit::dbg::parallel | sprockit::dbg::cpu_affinity,
               "PDES rank %i: setting user-specified task affinity %i",
               me_, task_affinity);

  cpu_set_t cpuset;
  CPU_ZERO(&cpuset);
  CPU_SET(task_affinity, &cpuset);
  sched_setaffinity(0,sizeof(cpu_set_t), &cpuset);
#endif

  //launch all the subthreads - don't launch zero
  //main thread will do zero's work
  int status;
  int thread_affinity;
  for (int i=1; i < nthread_; ++i){
#if SSTMAC_USE_CPU_AFFINITY
    //pin the pthread to core base+i
    thread_affinity = task_affinity + i;

    debug_printf(sprockit::dbg::parallel | sprockit::dbg::cpu_affinity,
                 "PDES rank %i: setting thread %i affinity %i",
                 me_, i, thread_affinity);

    cpu_set_t cpuset;
    CPU_ZERO(&cpuset);
    CPU_SET(thread_affinity, &cpuset);
    status = pthread_attr_setaffinity_np(&pthread_attrs_[i],
                                         sizeof(cpu_set_t), &cpuset);
    if (status != 0){
        sprockit::abort("multithreaded_event_container::run: failed setting pthread affinity");
    }
#endif
    status = pthread_create(&pthreads_[i], &pthread_attrs_[i],
        pthread_run_worker_thread, &queues_[i]);
    if (status != 0){
        spkt_abort_printf("multithreaded_event_container::run: failed creating pthread=%d:\n%s",
                        errno, ::strerror(errno));
    }
  }

  run_loop();

  for (int i=1; i < nthread_; ++i){
    int32_t marker = 1;
    int32_t newValue = 0;
    //CAS the sentinel value to terminate child
    OSAtomicCompareAndSwap32(marker, newValue, queues_[i].signalFromParentAllDone);
  }

  for (int i=1; i < nthread_; ++i){
    void* ignore;
    int status = pthread_join(pthreads_[i], &ignore);
    if (status != 0){
        sprockit::abort("multithreaded_event_container::run: failed joining pthread");
    }
  }

  compute_final_time();
}


}
}

#endif // !SSTMAC_INTEGRATED_SST_CORE
