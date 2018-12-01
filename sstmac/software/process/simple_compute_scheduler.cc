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

#include <sstmac/software/process/simple_compute_scheduler.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/common/sstmac_config.h>

namespace sstmac {
namespace sw {

void
simple_compute_scheduler::reserve_cores(int ncores, thread* thr)
{
#if SSTMAC_SANITY_CHECK
  if (ncore_active_ > ncores_){
    spkt_throw_printf(
      sprockit::value_error,
      "simple_compute_scheduler::reserve_core: %d cores active, only %d cores total",
      ncore_active_, ncores_);
  }
#endif
  int total_cores_needed = ncores + ncore_active_;
  if (total_cores_needed > ncores_){
    pending_threads_.emplace_back(ncores, thr);
    os_->block();
  }
  //no worrying about masks
  for (int i=ncore_active_; i < ncore_active_ + ncores; ++i){
    thr->add_active_core(i);
  }
  ncore_active_ += ncores;
}

void
simple_compute_scheduler::release_cores(int ncores, thread* thr)
{
  ncore_active_ -= ncores;
  for (int i=0; i < ncores; ++i){
    thr->pop_active_core();
  }

  while (!pending_threads_.empty()){
    auto pair = pending_threads_.front();
    int ncores_needed = pair.first;
    int ncores_free = ncores_ - ncore_active_;
    if (ncores_free >= ncores_needed){
      pending_threads_.pop_front();
      os_->unblock(pair.second);
    }
  }
}

}
}
