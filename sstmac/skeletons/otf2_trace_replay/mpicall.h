/**
Copyright 2009-2021 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2021, NTESS

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

#ifndef SSTMAC_SKELETONS_OTF2_TRACE_REPLAY_MpiCall_H_
#define SSTMAC_SKELETONS_OTF2_TRACE_REPLAY_MpiCall_H_

#include <sstmac/software/process/operating_system.h>
#include <sumi-mpi/mpi_api.h>
#include <otf2/otf2.h>
#include <string>
#include <functional>

#include <sstmac/skeletons/otf2_trace_replay/callid.h>

// forward declare
class OTF2TraceReplayApp;

class MpiCall {
 public:
  MpiCall(OTF2_TimeStamp start, OTF2TraceReplayApp* _app,
          MPI_CALL_ID _id) :
    isready(false), app(_app),
    start_time(start),
    end_time(0),
    id(_id)
  {
  }

  MpiCall(OTF2_TimeStamp start, OTF2TraceReplayApp* _app,
          MPI_CALL_ID _id, std::function<void()> trigger) :
    isready(false), app(_app),
    start_time(start),
    end_time(0),
    id(_id),
    on_trigger(trigger)
  {
  }

  static const char* name(MPI_CALL_ID id);

  MpiCall(const MpiCall&) = delete; //to avoid accidental copies


  ~MpiCall() {}

  // Methods
  sstmac::TimeDelta getStart() const {
    if (start_time == 0){
      std::cerr << "Warning: start timestamp is not initialized for " << toString() << std::endl;
    }
    return convertTime(start_time);
  }

  sstmac::TimeDelta getEnd() const {
    if (end_time == 0){
      std::cerr << "Warning: end timestamp is not initialized for " << toString() << std::endl;
    }
    return convertTime(end_time);
  }

  void setTrigger(std::function<void()> trigger){
    on_trigger = trigger;
  }


  bool isReady() const {
    return isready;
  }

  void trigger();

  const char* toString() const {
    return name(id);
  }

  // Members
  OTF2_TimeStamp start_time, end_time;
  std::function<void()> on_trigger;
  OTF2TraceReplayApp* app;
  bool isready;
  MPI_CALL_ID id;

  static void assertCall(MpiCall* cb, std::string msg){
    if (cb == nullptr) {
      spkt_abort_printf("ASSERT FAILED: %s", msg.c_str());
    }
  }

 private:
  sstmac::TimeDelta convertTime(const OTF2_TimeStamp) const;

};




#endif /* SSTMAC_SKELETONS_OTF2_TRACE_REPLAY_MpiCall_H_ */
