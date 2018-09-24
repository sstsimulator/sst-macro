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

#ifndef CLOCK_CYCLE_EVENT_CONTAINER_H
#define CLOCK_CYCLE_EVENT_CONTAINER_H

#include <sstmac/common/sstmac_config.h>
#if !SSTMAC_INTEGRATED_SST_CORE

#include <sstmac/common/event_manager.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/backends/common/parallel_runtime.h>

DeclareDebugSlot(event_manager_time_vote);

namespace sstmac {
namespace native {

enum class vote_type_t {
  max,
  min
};

static inline uint64_t rdtsc(void)
{
  uint32_t hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return uint64_t( (uint64_t)lo | (uint64_t)hi<<32);
}

class clock_cycle_event_map :
  public event_manager
{
  FactoryRegister("clock_cycle_parallel", event_manager, clock_cycle_event_map,
      "Implements a parallel event queue with synchronization on regular clock cycles")
 public:
  clock_cycle_event_map(sprockit::sim_parameters* params, parallel_runtime* rt);

  virtual ~clock_cycle_event_map() throw() {}

  void renew_scheduler(int thread, timestamp t, event_scheduler* es);

 protected:
  /**
   * @brief receive_incoming_events
   * @param vote The minimum event time I have
   * @return The minimum event time across all LPs
   */
  timestamp receive_incoming_events(timestamp vote) override;

  void compute_final_time(timestamp vote);

  int num_profile_loops_;

 private:
  void run() override;

  int handle_incoming(char* buf);

 private:
  int epoch_;

  

};

}
}

#endif // !SSTMAC_INTEGRATED_SST_CORE

#endif // CLOCK_CYCLE_EVENT_CONTAINER_H
