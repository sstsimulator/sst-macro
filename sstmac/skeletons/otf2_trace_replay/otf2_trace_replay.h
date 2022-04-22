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

#ifndef sstmac_skeletons_otf2_OTF2_TRACE_REPLAY_H_
#define sstmac_skeletons_otf2_OTF2_TRACE_REPLAY_H_

#include <otf2/otf2.h>
#include <string>
#include <map>

#include <sstmac/software/process/app.h>
#include <sumi-mpi/mpi_api.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

#include <sstmac/skeletons/otf2_trace_replay/callqueue.h>
#include <sstmac/skeletons/otf2_trace_replay/structures.h>

class OTF2TraceReplayApp : public sstmac::sw::App {
 public:
  SST_ELI_REGISTER_DERIVED(
    sstmac::sw::App,
    OTF2TraceReplayApp,
    "macro",
    "parseotf2",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "application for parsing and simulating OTF2 traces")

  OTF2TraceReplayApp(SST::Params& params,
                     sstmac::sw::SoftwareId sid,
                     sstmac::sw::OperatingSystem* os);

  sumi::MpiApi* getMpi(){
    return mpi_;
  }

  CallQueue& getCallQueue(){
    return call_queue_;
  }

  bool printTraceEvents() const {
    return print_trace_events_;
  }

  bool printMpiCalls() const {
    return print_mpi_calls_;
  }

  bool printTimeDeltas() const {
    return print_time_deltas_;
  }

  bool printUnknownCallback() const {
    return print_unknown_callback_;
  }

  int rank() const {
    return rank_;
  }

  void addEvents(int events){
    total_events_ += events;
  }

  void localToGlobalComm(MPI_Comm local, MPI_Comm global){
    global_to_local_comm_[global] = local;
  }

  MPI_Comm convertGlobalToLocalComm(MPI_Comm global){
    auto iter = global_to_local_comm_.find(global);
    if (iter == global_to_local_comm_.end()){
      return global;
    } else {
      return iter->second;
    }
  }

  int skeletonMain() override;

  void startMpi(sstmac::TimeDelta);

  void endMpi(sstmac::TimeDelta);

  OTF2_ClockProperties otf2_clock_properties;
  std::map<OTF2_StringRef, std::string> otf2_string_table;
  std::map<OTF2_RegionRef, MPI_CALL_ID> otf2_regions;
  std::vector<OTF2_Callpath> otf2_callpaths;
  std::map<OTF2_GroupRef,bool> otf2_groups;

  ~OTF2TraceReplayApp() throw()	{ }

 private:
  OTF2_Reader* initializeEventReader();
  void initiateTraceReplay(OTF2_Reader*);
  void verifyReplaySuccess();


 private:
  CallQueue call_queue_;

  sstmac::TimeDelta compute_time;

  std::map<MPI_Comm, MPI_Comm> global_to_local_comm_;

  sumi::MpiApi* mpi_;

  double timescale_;
  double terminate_percent_;
  bool print_progress_;
  bool print_mpi_calls_;
  bool print_trace_events_;
  bool print_time_deltas_;
  bool print_unknown_callback_;
  std::string metafile_;
  int rank_;
  long total_events_;
};

#endif /* OTF2_TRACE_REPLAY_H_ */
