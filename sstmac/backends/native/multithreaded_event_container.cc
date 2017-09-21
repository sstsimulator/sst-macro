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
#include <limits>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/thread_safe.h>

RegisterDebugSlot(multithread_event_manager);
RegisterDebugSlot(cpu_affinity);

RegisterKeywords(
  "cpu_affinity",
);

static int busy_loop_count = 1000000;

static inline void busy_loop(){
  //500 seems to be just about right
  //checking back often enough - but without thrashing the variable
  for (int i=0; i < busy_loop_count; ++i){
    __asm__ __volatile__("");
  }
}

int num_busy_spins = 0;

static int32_t terminate_sentinel = std::numeric_limits<int32_t>::max();

namespace sstmac {
namespace native {

#define atomic_is_zero(x) \
  (add_int32_atomic(int32_t(0), x) == 0)
  //bool done = cas_int32(0, 0, x);

static inline void wait_on_child_completion(thread_queue* q, timestamp& min_time)
{
  bool done = atomic_is_zero(q->delta_t);
  while (!done){
    busy_loop(); //don't slam the variable too hard
    done = atomic_is_zero(q->delta_t);
  }
  min_time = std::min(min_time, q->min_time);
}

static void
pthread_run_worker_thread(void* args)
{
  thread_queue* q = (thread_queue*) args;
  timestamp horizon;
  uint64_t epoch = 0;
  while(1){
    bool stillZero = atomic_is_zero(q->delta_t);
    if (!stillZero){
      int32_t delta_t = *q->delta_t;
      if (q->child1) add_int32_atomic(delta_t, q->child1->delta_t);
      if (q->child2) add_int32_atomic(delta_t, q->child2->delta_t);
      if (delta_t == terminate_sentinel){
        return;
      } else if (delta_t != 0) {
        horizon += timestamp(delta_t, timestamp::exact);
        timestamp new_min_time = q->mgr->run_events(horizon);
        q->min_time = new_min_time;
        ++epoch;
      }
      if (q->child1) wait_on_child_completion(q->child1, q->min_time);
      if (q->child2) wait_on_child_completion(q->child2, q->min_time);
      add_int32_atomic(-delta_t, q->delta_t);
    } else {
      busy_loop(); //don't slam the variable too hard
    }
  }
  return;
}

static void*
spin_up_pthread_work(void* args){
  thread_queue* q = (thread_queue*) args;
  q->mgr->spin_up(pthread_run_worker_thread, q);
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

  me_ = rt_->me();
  nproc_ = rt_->nproc();
  if (params->has_param("cpu_affinity")) {
    params->get_vector_param("cpu_affinity", cpu_affinity_);
    //it would be nice to check that size of cpu_offsets matches task per node
  }

  busy_loop_count = params->get_optional_int_param("busy_loop_count", busy_loop_count);

  num_subthreads_ = rt->nworker_thread();
  master_thread_ = rt->has_master_thread();
  if (!master_thread_){
    --num_subthreads_;
  }

  queues_.resize(num_subthreads_);
  pthreads_.resize(num_subthreads_);
  pthread_attrs_.resize(num_subthreads_);
  thread_managers_.resize(num_subthreads_);

  for (int i=0; i < num_subthreads_; ++i){
    thread_managers_[i] = new event_manager(params, rt);
    thread_managers_[i]->set_thread(i);
  }
  set_thread(num_subthreads_);

  for (int i=0; i < queues_.size(); ++i){
    queues_[i].mgr = thread_managers_[i];
  }

  for (int i=0; i < num_subthreads_; ++i){
    int status = pthread_attr_init(&pthread_attrs_[i]);
    if (status != 0){
      sprockit::abort("multithreaded_event_container::run: failed creating pthread attributes");
    }
  }
}

static inline uint64_t rdstc(void)
{
  uint32_t hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return uint64_t( (uint64_t)lo | (uint64_t)hi<<32);
}


void
multithreaded_event_container::run_work()
{
  //make the binary spanning tree for the thread barrier
  thread_queue* child1 = nullptr;
  thread_queue* child2 = nullptr;
  if (num_subthreads_ >= 1){
    child1 = &queues_[0]; 
  }
  if (num_subthreads_ >= 2){
    child2 = &queues_[1];
  }

  int last_level_offset = 0;
  int level_offset = 2;
  int level_size = 4;
  while (level_offset < num_subthreads_){
    int maxChild = std::min(level_offset+level_size,num_subthreads_);
    for (int c=level_offset; c < maxChild; ++c){
      int child_offset = c - level_offset;
      int parent_offset = child_offset / 2;
      int child_number = child_offset % 2;
      int parent = last_level_offset + parent_offset;
      thread_queue& parentQ = queues_[parent];
      if (child_number == 0){
        parentQ.child1 = &queues_[c];
      } else {
        parentQ.child2 = &queues_[c];
      }
    }
    last_level_offset = level_offset;
    level_offset += level_size;
    level_size *= 2;
  }
  
  timestamp last_horizon;
  timestamp lower_bound;
  uint64_t epoch = 0;
  int num_loops_left = num_profile_loops_;
  if (num_loops_left && rt_->me() == 0){
    printf("Running %d profile loops\n", num_loops_left); 
    fflush(stdout);
  }
  if (lookahead_.ticks() == 0){
    sprockit::abort("Zero-latency link - no lookahaed, cannot run in parallel");
  }
  while (lower_bound != no_events_left_time || num_loops_left > 0){
    timestamp horizon = lower_bound + lookahead_;
    uint64_t delta_t = horizon.ticks() - last_horizon.ticks();
    if (num_loops_left == 0 && delta_t > std::numeric_limits<int32_t>::max()){
      spkt_abort_printf("delta_t %lu too large: lower timestamp resolution", delta_t);
    }
    int32_t delta_t_32 = delta_t;
    if (child1) add_int32_atomic(delta_t_32, child1->delta_t);
    if (child2) add_int32_atomic(delta_t_32, child2->delta_t);

    timestamp min_time = no_events_left_time;
    if (!master_thread_){
      min_time = run_events(horizon);
    }

    auto t_start = rdstc();

    if (child1) wait_on_child_completion(child1, min_time);
    if (child2) wait_on_child_completion(child2, min_time);
    ++epoch;

    auto t_stop = rdstc();
    printf("Barrier took %llu\n", t_stop - t_start);

    lower_bound = receive_incoming_events(min_time);
    last_horizon = horizon;
    if (num_loops_left) --num_loops_left;
  }

  if (child1) add_int32_atomic(terminate_sentinel, child1->delta_t);
  if (child2) add_int32_atomic(terminate_sentinel, child2->delta_t);

  if (rt_->me() == 0) printf("Ran %lu epochs in multithreading run\n", epoch);

}

void
multithreaded_event_container::run()
{
  interconn_->setup();

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
  debug_printf(sprockit::dbg::parallel, "spawning %d subthreads",
               num_subthreads_);
  for (int i=0; i < num_subthreads_; ++i){
#if SSTMAC_USE_CPU_AFFINITY
    //pin the pthread to core base+i
    thread_affinity = task_affinity + i + 1;

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
        spin_up_pthread_work, &queues_[i]);
    if (status != 0){
        spkt_abort_printf("multithreaded_event_container::run: failed creating pthread=%d:\n%s",
                        errno, ::strerror(errno));
    }
  }

  run_work();

  timestamp final_time = master_thread_ ? timestamp() : now_;

  for (int i=0; i < num_subthreads_; ++i){
    void* ignore;
    int status = pthread_join(pthreads_[i], &ignore);
    if (status != 0){
        sprockit::abort("multithreaded_event_container::run: failed joining pthread");
    }
    final_time = std::max(final_time, thread_managers_[i]->now());
  }

  compute_final_time(final_time);
}


}
}

#endif // !SSTMAC_INTEGRATED_SST_CORE
