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

#include <sstmac/software/process/cpuset_compute_scheduler.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/operating_system.h>

namespace sstmac {
namespace sw {

void
cpuset_compute_scheduler::configure(int ncore, int nsocket)
{
  compute_scheduler::configure(ncore, nsocket);
  //all cores greater than ncore should be removed from bitmask
  for (int i=0; i < ncore; ++i){
    available_cores_ = available_cores_ | (1<<i);
  }
}

void
cpuset_compute_scheduler::reserve_core(thread *thr)
{
  uint64_t valid_cores = available_cores_ & thr->cpumask();  
  if (valid_cores == 0){
    debug_printf(sprockit::dbg::compute_scheduler,
        "No available cores from set %X intersect cpumask %X for thread %ld",
        available_cores_, thr->cpumask(), thr->thread_id());
    //no available cores, hold up
    pending_threads_.push_back(thr);
    os_->block();
    //this is guaranteed not to unblock until valid core received
  } else {
    allocate_core_to_thread(valid_cores, thr);
  }

}

void
cpuset_compute_scheduler::allocate_core_to_thread(uint64_t valid_cores, thread* thr)
{
  for (int i=0; i < ncores_; ++i){
    uint64_t mask = valid_cores & (1<<i);
    if (mask != 0){
      allocate_core(i);
      thr->set_active_core(i);
      debug_printf(sprockit::dbg::compute_scheduler,
          "Core %d matches from set %X intersecting cpumask %X for thread %ld",
          i, valid_cores, thr->cpumask(), thr->thread_id());
      return;
    }
  }

  spkt_abort_printf("No cores matches between %X and %X but still trying to allocate",
                    thr->cpumask(), valid_cores);

}

void
cpuset_compute_scheduler::release_core(thread *thr)
{  
  deallocate_core(thr->active_core());
  debug_printf(sprockit::dbg::compute_scheduler,
      "Releasing core %d for thread %ld yields cpuset %X",
      thr->active_core(), thr->thread_id(), available_cores_);
  if (pending_threads_.empty())
    return;
  
  std::list<thread*>::iterator it, tmp, end = pending_threads_.end();
  it = pending_threads_.begin();
  while (it != end){
    tmp = it++;
    thread* thr = *tmp;
    uint64_t valid_cores = available_cores_ & thr->cpumask();    
    if (valid_cores != 0){
      allocate_core_to_thread(valid_cores, thr);
      //the newly freed core allows another thread to continue
      pending_threads_.erase(tmp);
      os_->unblock(thr);
      break;
    }
  }
  
}

}
}
