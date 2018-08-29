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

#include <sstmac/skeletons/otf2_trace_replay/callqueue.h>
#include <sstmac/skeletons/otf2_trace_replay/otf2_trace_replay.h>

using namespace std;

#if 1
    #define TRIGGER_PRINT(...) cerr << "TRIGGERED CALL (#" << app->rank() << "): " << __VA_ARGS__ << endl;
#else
    #define TRIGGER_PRINT(...)
#endif

/******************************************************************************
 *  CallQueue functions
 */

int
CallQueue::CallReady(MpiCall* call) {
  int triggered = 0;
  call->isready = true;

  // when a call at the front of the queue is ready, there may be a
  // cascade of other ready calls behind it.
  while (call_queue.size() > 0 && call_queue.front().IsReady()) {
    auto& front = call_queue.front();
    front.Trigger();

    if (app->PrintMpiCalls()) {
       TRIGGER_PRINT(front.ToString());
    }
    call_queue.pop();
    triggered++;
  }

  return triggered;
}

MpiCall*
CallQueue::FindRequest(MPI_Request req) const
{
  auto iter = request_map.find(req);
  if (iter == request_map.end()){
    spkt_abort_printf("Rank %d cannot find request %ld\n",
                      app->GetMpi()->rank(), req);
  }
  return iter->second;
}


void
MpiCall::Trigger() {
  app->StartMpi(GetStart());
  if (on_trigger){
    on_trigger();
  }
  app->EndMpi(GetEnd());
}

sstmac::timestamp
MpiCall::convert_time(const OTF2_TimeStamp ts) const
{
  const auto start_offset = app->otf2_clock_properties.globalOffset;
  const auto ticks_per_second = app->otf2_clock_properties.timerResolution;
  return sstmac::timestamp(((double(ts) - start_offset)/ticks_per_second));
}
