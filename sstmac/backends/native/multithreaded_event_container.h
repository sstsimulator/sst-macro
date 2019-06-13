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

#ifndef MULTITHREADED_EVENT_CONTAINER_H
#define MULTITHREADED_EVENT_CONTAINER_H

#include <sstmac/common/sstmac_config.h>
#if !SSTMAC_INTEGRATED_SST_CORE

#include <sstmac/backends/native/clock_cycle_event_container.h>
#include <pthread.h>
#include <stdlib.h>

DeclareDebugSlot(multithread_EventManager);
DeclareDebugSlot(cpu_affinity);

namespace sstmac {
namespace native {

class MultithreadedEventContainer;

struct threadQueue
{
  threadQueue() :
    mgr(nullptr),
    child1(nullptr),
    child2(nullptr)
  {
    void* ptr = &delta_t;
    int rc = posix_memalign((void**)ptr, sizeof(void*), sizeof(int64_t));
    if (rc != 0){
      spkt_abort_printf("Failed to allocated aligned memory: %d\n%s",
                        rc, ::strerror(rc));
    }
    *delta_t = 0;
  }

  volatile int64_t* delta_t;
  Timestamp min_time;
  EventManager* mgr;
  threadQueue* child1;
  threadQueue* child2;

};


class MultithreadedEventContainer :
  public ClockCycleEventMap
{
 public:
  SST_ELI_REGISTER_DERIVED(
    EventManager,
    MultithreadedEventContainer,
    "macro",
    "multithread",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "Implements a parallel event queue with support for SMP-aware multithreading")

  MultithreadedEventContainer(SST::Params& params, ParallelRuntime* rt);

  ~MultithreadedEventContainer() throw () {}

  virtual void run() override;

  void scheduleStop(Timestamp until) override;

  EventManager* threadManager(int thr) const override {
    if (thr == num_subthreads_) {
      return const_cast<MultithreadedEventContainer*>(this);
    } else {
      return thread_managers_[thr];
    }
  }

 private:
  int num_subthreads_;

  std::vector<EventManager*> thread_managers_;

  void runWork();

  std::vector<threadQueue> queues_;
  std::vector<int> cpu_affinity_;
  std::vector<pthread_t> pthreads_;
  std::vector<pthread_attr_t> pthread_attrs_;

};

}
}


#endif // !SSTMAC_INTEGRATED_SST_CORE

#endif // MULTITHREADED_EVENT_CONTAINER_H
