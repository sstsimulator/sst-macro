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

#ifndef MULTITHREADED_EVENT_CONTAINER_H
#define MULTITHREADED_EVENT_CONTAINER_H

#include <sstmac/common/sstmac_config.h>
#if !SSTMAC_INTEGRATED_SST_CORE

#include <sstmac/backends/native/clock_cycle_parallel/clock_cycle_event_container.h>
#include <sstmac/backends/native/clock_cycle_parallel/multithreaded_subcontainer.h>
#include <sstmac/backends/native/clock_cycle_parallel/thread_barrier.h>
#include <pthread.h>


DeclareDebugSlot(multithread_event_manager);
DeclareDebugSlot(cpu_affinity);

namespace sstmac {
namespace native {

class thread_event_schedule_map
{
 public:
  std::list<event_queue_entry*>&
  pending_events(int srcthread, int dstthread);

  void init(int nthread);

 protected:
  int array_index(int srcthread, int dstthread);

 protected:
  int nthread_;

  std::vector<std::list<event_queue_entry*> > events_;

};


class multithreaded_event_container :
  public clock_cycle_event_map
{
  FactoryRegister("multithread | multithreaded", event_manager, multithreaded_event_container,
    "Implements a parallel event queue with support for SMP-aware multithreading")
 public:
  multithreaded_event_container(sprockit::sim_parameters* params, parallel_runtime* rt);

  ~multithreaded_event_container() throw () {}

  virtual void run() override;

  virtual void schedule_stop(timestamp until) override;

  void multithread_schedule(
    int srcthread,
    int dstthread,
    uint32_t seqnum,
    event_queue_entry* ev) override;

  std::list<event_queue_entry*>&
  pending_events(int srcthread, int dstthread) {
    return pending_event_map_.pending_events(srcthread, dstthread);
  }

  virtual void set_interconnect(hw::interconnect* interconn) override;

  virtual void receive_incoming_events() override;

  void schedule_incoming(int thread_id, clock_cycle_event_map* mgr);

  void send_recv_barrier(int thread_id);

  timestamp time_vote_barrier(int thread_id, timestamp min_time, vote_type_t ty);

  timestamp vote_next_round(timestamp my_time, vote_type_t ty) override;

  event_manager* ev_man_for_thread(int thread_id) const override;

  virtual void finish_stats(stat_collector *main, const std::string &name, timestamp end) override;

 protected:
  struct vote_thread_functor : public thread_barrier_functor {
    virtual int64_t
    execute(int64_t min_time){
      return parent->do_vote(min_time);
    }
    multithreaded_event_container* parent;
  };
  vote_thread_functor vote_functor_;

  struct send_recv_thread_functor : public thread_barrier_functor {
    virtual int64_t
    execute(int64_t){
      parent->clock_cycle_event_map::receive_incoming_events();
      return 0;
    }
    multithreaded_event_container* parent;
  };
  send_recv_thread_functor send_recv_functor_;

  std::vector<multithreaded_subcontainer*> subthreads_;

  thread_barrier send_recv_barrier_;
  thread_barrier vote_barrier_;
  thread_barrier final_time_barrier_;

  thread_event_schedule_map pending_event_map_;

  std::vector<int> cpu_affinity_;
  int me_;
  int nproc_;
  std::vector<pthread_t> pthreads_;
  std::vector<pthread_attr_t> pthread_attrs_;

};

}
}


#endif // !SSTMAC_INTEGRATED_SST_CORE

#endif // MULTITHREADED_EVENT_CONTAINER_H