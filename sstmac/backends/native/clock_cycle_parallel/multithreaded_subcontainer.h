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

#ifndef MULTITHREADED_SUBCONTAINER_H
#define MULTITHREADED_SUBCONTAINER_H

#include <sstmac/common/sstmac_config.h>
#if !SSTMAC_INTEGRATED_SST_CORE

#include <sstmac/backends/native/clock_cycle_parallel/clock_cycle_event_container.h>
#include <sstmac/backends/native/clock_cycle_parallel/multithreaded_event_container_fwd.h>

namespace sstmac {
namespace native {

class multithreaded_subcontainer :
  public clock_cycle_event_map
{
 public:
  ~multithreaded_subcontainer() throw () {}

  void
  multithread_schedule(
    int srcthread,
    int dstthread,
    uint32_t seqnum,
    event_queue_entry* ev) override;

  void
  receive_incoming_events() override;

  timestamp
  vote_next_round(timestamp my_time, vote_type_t ty) override;

  void run() override;

  multithreaded_subcontainer(
    sprockit::sim_parameters* params,
    parallel_runtime* rt,
    int thread_id,
    multithreaded_event_container* parent);

 protected:
  multithreaded_event_container* parent_;

};

}
}

#endif // !SSTMAC_INTEGRATED_SST_CORE

#endif // MULTITHREADED_SUBCONTAINER_H