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

#include <sstmac/backends/native/clock_cycle_parallel/multithreaded_event_container.h>
#include <unistd.h>
#include <execinfo.h>
#include <dlfcn.h>
#include <signal.h>
#include <iostream>
#include <sprockit/keyword_registration.h>

RegisterDebugSlot(multithread_event_manager);
RegisterDebugSlot(cpu_affinity);

RegisterKeywords(
  "cpu_affinity",
);

namespace sstmac {
namespace native {

void*
pthread_run_subthread(void* args)
{
  multithreaded_subcontainer* thr = (multithreaded_subcontainer*) args;

  try {
    thr->run();
  } catch (std::exception& e) {
    std::cerr << e.what() << std::endl;
    abort();
  }

  return 0;
}

void
thread_event_schedule_map::init(int nthread)
{
  nthread_ = nthread;
  int total_slots = array_index(nthread, nthread);
  events_.resize(total_slots);
}

std::list<event_queue_entry*>&
thread_event_schedule_map::pending_events(int srcthread, int dstthread)
{
  int idx = array_index(srcthread, dstthread);
  return events_[idx];
}

int
thread_event_schedule_map::array_index(int srcthread, int dstthread)
{
  return srcthread*nthread_ + dstthread;
}

event_manager*
multithreaded_event_container::ev_man_for_thread(int thread_id) const
{
  if (thread_id == 0){
    return event_manager::ev_man_for_thread(thread_id);
  }
  else {
    return subthreads_[thread_id];
  }
}

void
multithreaded_event_container::set_interconnect(hw::interconnect* interconn)
{
  int nthread_ = nthread();
  for (int i=1; i < nthread_; ++i){
    subthreads_[i]->set_interconnect(interconn);
  }
  clock_cycle_event_map::set_interconnect(interconn);
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


  send_recv_functor_.parent = this;
  vote_functor_.parent = this;

  int nthread_ = nthread();
  me_ = rt_->me();
  nproc_ = rt_->nproc();
  if (params->has_param("cpu_affinity")) {
    params->get_vector_param("cpu_affinity", cpu_affinity_);
    //it would be nice to check that size of cpu_offsets matches task per node
  }

  send_recv_barrier_.init(nthread_);
  vote_barrier_.init(nthread_);
  final_time_barrier_.init(nthread_);

  subthreads_.resize(nthread_);
  for (int i=1; i < nthread_; ++i){
    multithreaded_subcontainer* ev_man = new multithreaded_subcontainer(params, rt_, i, this);
    subthreads_[i] = ev_man;
  }

  pending_event_map_.init(nthread_);

  pthreads_.resize(nthread_);
  pthread_attrs_.resize(nthread_);

  for (int i=1; i < nthread_; ++i){
    int status = pthread_attr_init(&pthread_attrs_[i]);
    if (status != 0){
      spkt_throw(sprockit::value_error,              
        "multithreaded_event_container::run: failed creating pthread attributes");
    }
  }

}

void
multithreaded_event_container::schedule_stop(timestamp until)
{
  for (int i=1; i < nthread_; ++i){
    subthreads_[i]->schedule_stop(until);
  }
  event_manager::schedule_stop(until);
}

void
multithreaded_event_container::finish_stats(stat_collector *main, const std::string &name, timestamp end)
{
  event_manager::finish_stats(main, name, end); //finish my stats
  for (int i=1; i < nthread_; ++i){
    subthreads_[i]->finish_stats(main, name, end);
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
    spkt_throw(sprockit::value_error,
              "cpu_affinity array is required with cpu affinity enabled");
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

  //launch all the subthreads
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
        spkt_throw(sprockit::value_error,
        "multithreaded_event_container::run: failed setting pthread affinity");
    }
#endif
    status = pthread_create(&pthreads_[i], &pthread_attrs_[i],
        pthread_run_subthread, subthreads_[i]);
    if (status != 0){
        spkt_throw(sprockit::value_error,
            "multithreaded_event_container::run: failed creating pthread");
    }

  }

  clock_cycle_event_map::run();

  debug_printf(sprockit::dbg::event_manager,
    "joining %d event manager threads",
    nthread_);

  for (int i=1; i < nthread_; ++i){
    void* ignore;
    int status = pthread_join(pthreads_[i], &ignore);
    if (status != 0){
        spkt_throw(sprockit::value_error,
            "multithreaded_event_container::run: failed joining pthread");
    }
  }
}

timestamp
multithreaded_event_container::time_vote_barrier(int thread_id, timestamp time, vote_type_t ty)
{
  int64_t ticks = time.ticks_int64();
  int64_t final_vote;
  if (ty == vote_type_t::max){
    final_vote = final_time_barrier_.vote(thread_id, ticks, ty, nullptr);
  } else {
    final_vote = vote_barrier_.vote(thread_id, ticks, ty, &vote_functor_);
  }
  timestamp newtime = timestamp(final_vote, timestamp::exact);
  return newtime;
}

void
multithreaded_event_container::send_recv_barrier(int thread_id)
{
  send_recv_barrier_.start(thread_id, &send_recv_functor_);
}

void
multithreaded_event_container::receive_incoming_events()
{
  send_recv_barrier(thread_id_); //this will invoke clock_cycle_event_map::receive_incoming_events()
  schedule_incoming(thread_id(), this);
}

void
multithreaded_event_container::schedule_incoming(int thread_id, clock_cycle_event_map* mgr)
{
  //receive all the mpi messages
  debug_printf(sprockit::dbg::event_manager, 
    "thread barrier to scheduling incoming on thread %d, epoch %d",
    thread_id, epoch_);
  thread_barrier();

  std::vector<void*>& mpi_buffers = thread_incoming_[thread_id];
  mgr->schedule_incoming(mpi_buffers);
  mpi_buffers.clear();

  int nthread_ = nthread();
  for (int i=0; i < nthread_; ++i){
    std::list<event_queue_entry*>& events = pending_events(i, thread_id);
    std::list<event_queue_entry*>::iterator it, end = events.end();
    debug_printf(sprockit::dbg::event_manager,
      "scheduling %d events on thread %d from thread %d on epoch %d",
      events.size(), thread_id_, i, epoch_);
    for (it=events.begin(); it != end; ++it){
      mgr->add_event(*it);
    }
    events.clear();
  }
}

timestamp
multithreaded_event_container::vote_next_round(timestamp my_time, vote_type_t ty)
{
  debug_printf(sprockit::dbg::event_manager | sprockit::dbg::event_manager_time_vote | sprockit::dbg::parallel,
    "Rank %d thread barrier to start vote on thread %d, epoch %d",
    rt_->me(), thread_id(), epoch_);

  return time_vote_barrier(thread_id_, my_time, ty);
}

void
multithreaded_event_container::multithread_schedule(
    int srcthread,
    int dstthread,
    uint32_t seqnum,
    event_queue_entry* ev)
{
  ev->set_seqnum(seqnum);
  std::list<event_queue_entry*>& pending = pending_events(srcthread, dstthread);
  pending.push_back(ev);
}

}
}

#endif // !SSTMAC_INTEGRATED_SST_CORE