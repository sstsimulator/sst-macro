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

#ifndef sstmac_software_libraries_API_H
#define sstmac_software_libraries_API_H

#include <sprockit/factory.h>
#include <sstmac/software/process/software_id.h>
#include <sstmac/software/process/host_timer.h>
#include <sstmac/software/process/app_fwd.h>
#include <sstmac/software/process/thread_fwd.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/node_address.h>
#include <sstmac/common/event_scheduler_fwd.h>
#include <sstmac/sst_core/integrated_component.h>
#include <sprockit/keyword_registration.h>
#include <sys/time.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/subcomponent.h>
#endif

namespace sstmac {
namespace sw {

class API
#if SSTMAC_INTEGRATED_SST_CORE
 : public SST::SubComponent
#endif
{
 public:
  SST_ELI_REGISTER_BASE_DEFAULT(API)
  SST_ELI_REGISTER_CTOR(SST::Params&,App*,SST::Component*)

  virtual ~API();

  SoftwareId sid() const;

  NodeId addr() const;

  App* parent() const {
    return parent_;
  }

  Thread* activeThread();

  virtual void init(){}

  virtual void finish(){}

  GlobalTimestamp now() const;

  void schedule(GlobalTimestamp t, ExecutionEvent* ev);

  void scheduleDelay(Timestamp t, ExecutionEvent* ev);

  /**
   * @brief start_api_call
   * Enter a call such as MPI_Send. Any perf counters or time counters
   * collected since the last API call can then advance time or
   * increment statistics.
   */
  void startAPICall();

  /**
   * @brief end_api_call
   * Exit a call such as MPI_Send. Perf counters or time counters
   * collected since the last API call can then clear counters for
   * the next time window.
   */
  void endAPICall();

 protected:
  API(SST::Params& params, App* parent, SST::Component* comp);

  std::unique_ptr<HostTimer> host_timer_;
  App* parent_;

};

void apiLock();
void apiUnlock();

}
}

#if !SSTMAC_INTEGRATED_SST_CORE
namespace SST {
using SubComponent = sstmac::sw::API;
}
#endif

#endif // API_H
