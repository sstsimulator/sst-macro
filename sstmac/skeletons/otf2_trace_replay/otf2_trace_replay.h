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

#ifndef sstmac_skeletons_otf2_OTF2_TRACE_REPLAY_H_
#define sstmac_skeletons_otf2_OTF2_TRACE_REPLAY_H_

#include <otf2/otf2.h>
#include <string>

#include <sstmac/software/process/app.h>
#include <sumi-mpi/mpi_api.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

#include <sstmac/skeletons/otf2_trace_replay/callqueue.h>
#include <sstmac/skeletons/otf2_trace_replay/structures.h>

RegisterKeywords(
  "otf2_timescale",
  "otf2_terminate_percent",
  "otf2_print_progress",
  "otf2_metafile",
  "otf2_print_mpi_calls",
  "otf2_print_trace_events",
  "otf2_print_time_deltas"
  "otf2_print_unknown_callback"
);


class OTF2TraceReplayApp : public sstmac::sw::app {
  FactoryRegister("otf2_trace_replay_app | parseotf2 | otf2",
               sstmac::sw::app, OTF2TraceReplayApp,
               "application for parsing and simulating OTF2 traces")
 public:
  OTF2TraceReplayApp(sprockit::sim_parameters* params,
                     sstmac::sw::software_id sid,
                     sstmac::sw::operating_system* os);

  sumi::mpi_api* GetMpi();
  CallQueue& GetCallQueue();
  bool PrintTraceEvents();
  bool PrintMpiCalls();
  bool PrintTimeDeltas();
  bool PrintUnknownCallback();

  virtual void skeleton_main();

  void StartMpi(const sstmac::timestamp);
  void EndMpi(const sstmac::timestamp);

  int rank = -1;
  long total_events = 0;

  OTF2_ClockProperties otf2_clock_properties;
  std::vector<std::string> otf2_string_table;
  //std::unordered_map<OTF2_RegionRef, OTF2_Region> otf2_regions;
  std::vector<OTF2_Region> otf2_regions;
  std::vector<OTF2_Callpath> otf2_callpaths;
  std::vector<OTF2_Group> otf2_groups;
  std::vector<OTF2_Comm> otf2_comms;
  std::unordered_map<OTF2_StringRef, MPI_CALL_ID> otf2_mpi_call_map;

  ~OTF2TraceReplayApp() throw()	{ }

 private:
  OTF2_Reader* initialize_event_reader();
  void initiate_trace_replay(OTF2_Reader*);
  void verify_replay_success();

 private:
  CallQueue call_queue_;

  sstmac::timestamp compute_time;

  bool initialized_ = false;
  sumi::mpi_api* mpi_;

  double timescale_;
  double terminate_percent_;
  bool print_progress_;
  bool print_mpi_calls_;
  bool print_trace_events_;
  bool print_time_deltas_;
  bool print_unknown_callback_;
  std::string metafile_;
};

#endif /* OTF2_TRACE_REPLAY_H_ */