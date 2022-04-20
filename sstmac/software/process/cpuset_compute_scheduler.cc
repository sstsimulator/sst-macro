/**
Copyright 2009-2022 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2022, NTESS

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

#include <sstmac/software/process/cpuset_compute_scheduler.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/operating_system.h>

namespace sstmac {
namespace sw {

bool
CpusetComputeScheduler::allocateCores(int ncores_needed, Thread* thr)
{
  int ncores_found = 0;
  uint64_t valid_cores = thr->cpumask() & available_cores_;
  for (int i=0; i < ncores_ && ncores_found < ncores_needed; ++i){
    uint64_t mask = valid_cores & (1<<i);
    if (mask != 0){
      ++ncores_found;
      thr->addActiveCore(i);
      debug_printf(sprockit::dbg::compute_scheduler,
          "Core %d matches from available set %X intersecting thread cpumask %X for thread %ld",
          i, available_cores_, thr->cpumask(), thr->threadId());
    }
  }

  if (ncores_found < ncores_needed){
    for (int i=0; i < ncores_found; ++i){
      thr->popActiveCore();
    }
    return false;
  } else {
    uint64_t mask = thr->activeCoreMask();
    available_cores_ = available_cores_ & ~mask;
    debug_printf(sprockit::dbg::compute_scheduler,
        "Available mask is %X after subtracting %X",
        available_cores_, thr->activeCoreMask());
    return true;
  }
}

void
CpusetComputeScheduler::reserveCores(int ncores, Thread *thr)
{
  bool succ = allocateCores(ncores, thr);
  if (succ){
    //pass - do nothing
  } else {
    //this is guaranteed not to unblock until valid core received
    debug_printf(sprockit::dbg::compute_scheduler,
        "Failed to find %d cores for thread %u matching cpuset %X against available %X",
        ncores, thr->threadId(), thr->cpumask(), available_cores_);
    pending_threads_.emplace_back(ncores, thr);
    os_->block();
  }
}

void
CpusetComputeScheduler::releaseCores(int ncores, Thread *thr)
{  
  for (int i=0; i < ncores; ++i){
    int core = thr->popActiveCore();
    Thread::addCore(core, available_cores_);
    debug_printf(sprockit::dbg::compute_scheduler,
        "Releasing core %d for thread %u yields cpuset %X",
        core, thr->threadId(), available_cores_);
  }



  auto end = pending_threads_.end();
  for (auto it = pending_threads_.begin(); it != end; ){
    auto tmp = it++;
    auto pair = *tmp;
    bool succ = allocateCores(pair.first, pair.second);
    if (succ){
      pending_threads_.erase(tmp);
      os_->unblock(pair.second);
    } else {
      debug_printf(sprockit::dbg::compute_scheduler,
          "Failed to find %d cores for thread %u matching cpuset %X against available %X",
          ncores, pair.second->threadId(), pair.second->cpumask(), available_cores_);
    }
  }
}

}
}
