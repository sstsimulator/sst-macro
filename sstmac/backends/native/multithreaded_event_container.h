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

#include <sstmac/backends/native/clock_cycle_event_container.h>
#include <pthread.h>


DeclareDebugSlot(multithread_event_manager);
DeclareDebugSlot(cpu_affinity);

namespace sstmac {
namespace native {

class multithreaded_event_container;

struct thread_queue
{
  thread_queue() :
    numTasks(0),
    mgr(nullptr),
    threadId(0),
    //for these two, 1 means active, 0 means done
    signalFromParentAllDone(1),
    signalToParentWaveDone(1)
  {
  }

  std::vector<event_scheduler*> to_run;
  int32_t numTasks;
  int32_t signalToParentWaveDone;
  int32_t signalFromParentAllDone;
  timestamp minTime;
  timestamp timeHorizon;
  multithreaded_event_container* mgr;
  int threadId;

  void append(event_scheduler* es){
    to_run.push_back(es);
  }

  void clear(){
    numTasks = 0;
    to_run.clear();
  }

  event_scheduler* get(int i){
    return to_run[i];
  }

  int32_t query() {
    uint32_t zero = 0;
    int32_t taskQueury = OSAtomicXor32(zero, (uint32_t*)&numTasks);
    return taskQueury;
  }

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

  void multithread_schedule(int thread, event_queue_entry* ev, event_scheduler* dst) override;

 private:
  void run_loop();

  // Dim-1: Num threads
  // Dim-2: List of events
  // Pair: event to schedule, scheduler to deliver to
  std::vector<std::vector<std::pair<event_queue_entry*,event_scheduler*>>> pending_events_;
  std::vector<thread_queue> queues_;
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
