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

#ifndef CPUSET_COMPUTE_scheduleR_H
#define CPUSET_COMPUTE_scheduleR_H

#include <sstmac/software/process/compute_scheduler.h>

namespace sstmac {
namespace sw {

class cpuset_compute_scheduler : public compute_scheduler
{
  FactoryRegister("cpuset", compute_scheduler, cpuset_compute_scheduler,
              "Compute scheduler that assigns threads to specific cores based on CPU_SET")
 public:  
  cpuset_compute_scheduler(sprockit::sim_parameters* params,
                           operating_system* os) :
    available_cores_(0),
    compute_scheduler(params, os)
  {
  }

  void configure(int ncore, int nsocket);
  
  void reserve_core(thread *thr);
  
  void release_core(thread *thr);
  
 private:
  static int available_core(int ncore, uint64_t cpumask);
  
  void allocate_core(int core){
    available_cores_ = available_cores_ & ~(1<<core);
  }
  
  void deallocate_core(int core){
    available_cores_ = available_cores_ | (1<<core);
  }
  
 private:
  uint64_t available_cores_;
  std::list<thread*> pending_threads_;


};

}
}

#endif // CPUSET_COMPUTE_scheduleR_H