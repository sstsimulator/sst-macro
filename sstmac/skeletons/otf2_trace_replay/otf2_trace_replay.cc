/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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

#include "otf2_trace_replay.h"
#include "callbacks.h"
#include <algorithm>
#include <iomanip>
#include <string>
#include <algorithm>
#include <string>

using std::cout;
using std::cerr;
using std::endl;
using std::min;
using std::setw;

RegisterKeywords(
  { "otf2_timescale", "Compute time multiplier" },
  { "otf2_terminate_percent", "(Not implemented) Stop trace replay early" },
  { "otf2_print_progress", "(Not implemented) Prints percentage of trace completion" },
  { "otf2_metafile", "Path to an OTF2 trace metafile" },
  { "otf2_print_mpi_calls", "Print MPI calls as they are parsed" },
  { "otf2_print_trace_events", "Print trace callbacks as they are called" },
  { "otf2_print_time_deltas", "Print compute times between MPI events" },
  { "otf2_print_unknown_callback", "Print when an unknown callback is discovered" },
);


OTF2_GlobalDefReaderCallbacks* create_global_def_callbacks() {
  OTF2_GlobalDefReaderCallbacks* callbacks = OTF2_GlobalDefReaderCallbacks_New();
  OTF2_GlobalDefReaderCallbacks_SetClockPropertiesCallback(callbacks, def_clock_properties);
  OTF2_GlobalDefReaderCallbacks_SetStringCallback(callbacks, def_string);
  OTF2_GlobalDefReaderCallbacks_SetLocationGroupCallback( callbacks, def_location_group );
  OTF2_GlobalDefReaderCallbacks_SetLocationCallback(callbacks, def_location);
  OTF2_GlobalDefReaderCallbacks_SetRegionCallback(callbacks, def_region);
  OTF2_GlobalDefReaderCallbacks_SetCallpathCallback(callbacks, def_callpath);
  OTF2_GlobalDefReaderCallbacks_SetGroupCallback(callbacks, def_group);
  OTF2_GlobalDefReaderCallbacks_SetCommCallback(callbacks, def_comm);
  OTF2_GlobalDefReaderCallbacks_SetLocationGroupPropertyCallback( callbacks, def_location_group_property );
  OTF2_GlobalDefReaderCallbacks_SetLocationPropertyCallback( callbacks, def_location_property );

  return callbacks;
}

OTF2_DefReaderCallbacks* create_def_mapping_callback() {
  OTF2_DefReaderCallbacks* callbacks = OTF2_DefReaderCallbacks_New();
  OTF2_DefReaderCallbacks_SetMappingTableCallback(callbacks, def_mapping_table);

  return callbacks;
}

OTF2_EvtReaderCallbacks* create_evt_callbacks() {
    // TODO: check success
    OTF2_EvtReaderCallbacks* callbacks = OTF2_EvtReaderCallbacks_New();
    OTF2_EvtReaderCallbacks_SetEnterCallback(callbacks, event_enter);
    OTF2_EvtReaderCallbacks_SetLeaveCallback(callbacks, event_leave);
    OTF2_EvtReaderCallbacks_SetMpiSendCallback(callbacks, event_mpi_send);
    OTF2_EvtReaderCallbacks_SetMpiIsendCallback(callbacks, event_mpi_isend);
    OTF2_EvtReaderCallbacks_SetMpiIsendCompleteCallback(callbacks, event_mpi_isend_complete);
    OTF2_EvtReaderCallbacks_SetMpiIrecvRequestCallback(callbacks, event_mpi_irecv_request);
    OTF2_EvtReaderCallbacks_SetMpiRecvCallback(callbacks, event_mpi_recv);
    OTF2_EvtReaderCallbacks_SetMpiIrecvCallback(callbacks, event_mpi_irecv);
    OTF2_EvtReaderCallbacks_SetMpiRequestTestCallback(callbacks, event_mpi_request_test);
    OTF2_EvtReaderCallbacks_SetMpiRequestCancelledCallback(callbacks, event_mpi_request_cancelled);
    OTF2_EvtReaderCallbacks_SetMpiCollectiveEndCallback(callbacks, event_mpi_collective_end);
    OTF2_EvtReaderCallbacks_SetParameterStringCallback(callbacks, event_parameter_string);
    return callbacks;
}

static void check_status(OTF2_ErrorCode status, const std::string& description)
{
  if (status != OTF2_SUCCESS){
    spkt_abort_printf("OTF2 Error: %s  %s",
                      OTF2_Error_GetName(status),
                      description.c_str());
  }
}

OTF2TraceReplayApp::OTF2TraceReplayApp(SST::Params& params,
        sumi::SoftwareId sid, sstmac::sw::OperatingSystem* os) :
  App(params, sid, os), mpi_(nullptr), rank_(sid.task_), call_queue_(this), total_events_(0) {
  timescale_ = params.find<double>("otf2_timescale", 1.0);
  terminate_percent_ = params.find<double>("otf2_terminate_percent", 1);
  print_progress_ = params.find<bool>("otf2_print_progress", true);
  metafile_ = params.find<std::string>("otf2_metafile");

  print_mpi_calls_ = params.find<bool>("otf2_print_mpi_calls", false);
  print_trace_events_ = params.find<bool>("otf2_print_trace_events", false);
  print_time_deltas_ = params.find<bool>("otf2_print_time_deltas", false);
  print_unknown_callback_ = params.find<bool>("otf2_print_unknown_callback", false);
}

int
OTF2TraceReplayApp::skeletonMain() {
  if (rank_ == 0){
    std::cout << "Running OTF2 replay on "
        << metafile_ << std::endl;
  }

  mpi_ = getApi<sumi::MpiApi>("mpi");
  mpi_->setGenerateIds(false);
  mpi_->init(nullptr,nullptr); //force init here

  auto event_reader = initializeEventReader();

  initiateTraceReplay(event_reader);
  verifyReplaySuccess();

  OTF2_Reader_Close(event_reader);

  return 0;
}

// Indicate that we are starting an MPI call.
void
OTF2TraceReplayApp::startMpi(const sstmac::TimeDelta wall) {
  // Time not initialized
  if (compute_time.ticks() == sstmac::TimeDelta::zero) return;

  if (printTimeDeltas()) {
    cout << "\u0394T " << (wall-compute_time).sec() << " seconds"<< endl;
  }

  compute((timescale_ * (wall - compute_time)));
}

void
OTF2TraceReplayApp::endMpi(const sstmac::TimeDelta wall) {
  compute_time = wall;
}

struct c_vector {
  size_t capacity;
  size_t size;
  uint64_t members[];
};

// What did this function do. It caused a memory error, disabling it doesn't seem to affect trace replay.
static OTF2_CallbackCode
GlobDefLocation_Register(void* userData,
  OTF2_LocationRef location, OTF2_StringRef name,
  OTF2_LocationType locationType, uint64_t numberOfEvents,
  OTF2_LocationGroupRef locationGroup)
{
  auto app = (OTF2TraceReplayApp*)userData;
  app->addEvents(numberOfEvents);

  return OTF2_CALLBACK_SUCCESS;
}

OTF2_Reader*
OTF2TraceReplayApp::initializeEventReader() {
	// OTF2 has an excellent API
	uint64_t number_of_locations;
	//uint64_t trace_length = 0;
  auto reader = OTF2_Reader_Open(metafile_.c_str());
  OTF2_Reader_SetSerialCollectiveCallbacks(reader);
  check_status(OTF2_Reader_GetNumberOfLocations(reader, &number_of_locations), "OTF2_Reader_GetNumberOfLocations\n");

  if (number_of_locations <= rank_) {
    cerr << "ERROR: Rank " << rank_ << " cannot participate in a trace replay with "
         << number_of_locations << " ranks" << endl;
    spkt_abort_printf("ASSERT FAILED: Number of MPI ranks must match the number of trace files");
  }

  bool successful_open_def_files = OTF2_Reader_OpenDefFiles(reader) == OTF2_SUCCESS;
  check_status(OTF2_Reader_OpenEvtFiles(reader), "OTF2_Reader_OpenEvtFiles\n");

  OTF2_DefReader* def_reader = OTF2_Reader_GetDefReader(reader, this->tid());
  OTF2_DefReaderCallbacks* def_callbacks = create_def_mapping_callback();

  check_status( OTF2_Reader_RegisterDefCallbacks(reader, def_reader, def_callbacks, (void*)this),
                   "OTF2_Reader_RegisterGlobalDefCallbacks\n");
  OTF2_DefReaderCallbacks_Delete(def_callbacks);

  uint64_t definitions_read = 0;
  check_status(OTF2_Reader_ReadAllLocalDefinitions(reader, def_reader, &definitions_read),
                 "OTF2_Reader_ReadAllDefinitions\n");

  if (successful_open_def_files) {
    check_status(OTF2_Reader_CloseDefFiles(reader),
                 "OTF2_Reader_CloseDefFiles\n");
  }

  //struct c_vector* locations = (c_vector*) malloc(sizeof(*locations) + number_of_locations * sizeof(*locations->members));
  //locations->capacity = number_of_locations;
  //locations->size = number_of_locations;
  OTF2_GlobalDefReader* global_def_reader = OTF2_Reader_GetGlobalDefReader(reader);
  OTF2_GlobalDefReaderCallbacks* global_def_callbacks;
  global_def_callbacks = create_global_def_callbacks();
  OTF2_GlobalDefReaderCallbacks_SetLocationCallback(global_def_callbacks, &GlobDefLocation_Register);
  check_status( OTF2_Reader_RegisterGlobalDefCallbacks(reader, global_def_reader, global_def_callbacks, (void*)this),
                "OTF2_Reader_RegisterGlobalDefCallbacks\n");
  OTF2_GlobalDefReaderCallbacks_Delete(global_def_callbacks);
  definitions_read = 0;
  check_status(OTF2_Reader_ReadAllGlobalDefinitions(reader, global_def_reader, &definitions_read),
               "OTF2_Reader_ReadAllGlobalDefinitions\n");
  OTF2_Reader_CloseGlobalDefReader(reader, global_def_reader);

  OTF2_EvtReader* evt_reader = OTF2_Reader_GetEvtReader(reader, rank_);
  OTF2_EvtReaderCallbacks* event_callbacks = create_evt_callbacks();
  check_status(OTF2_Reader_RegisterEvtCallbacks(reader, evt_reader,
                                       event_callbacks, (void*)this),
      "OTF2_Reader_RegisterEvtCallbacks\n");
  OTF2_EvtReaderCallbacks_Delete(event_callbacks);

  //free(locations);

  return reader;
}

inline uint64_t
handle_events(OTF2_Reader* reader, OTF2_EvtReader* event_reader) {
  uint64_t events_read = 0;
  uint64_t read_all_events = OTF2_UNDEFINED_UINT64;

  check_status(OTF2_Reader_ReadLocalEvents(reader, event_reader, read_all_events, &events_read),
               "Trace replay failure");

	return events_read;
}

void OTF2TraceReplayApp::initiateTraceReplay(OTF2_Reader* reader) {
  // get the trace reader corresponding to the rank
  uint64_t locs = 0;
  OTF2_Reader_GetNumberOfLocations(reader, &locs);

  OTF2_EvtReader* event_reader = OTF2_Reader_GetEvtReader(reader, rank_);

  uint64_t events_read = handle_events(reader, event_reader);

  if (rank_ == 0) std::cout << "OTF2 Trace replay complete" << endl;

  // cleanup
  check_status(OTF2_Reader_CloseEvtReader(reader, event_reader), "OTF2_Reader_CloseEvtReader");
  check_status(OTF2_Reader_CloseEvtFiles(reader), "OTF2_Reader_CloseEvtFiles\n");
}

void
OTF2TraceReplayApp::verifyReplaySuccess()
{
  int incomplete_calls = call_queue_.getDepth();

  if(incomplete_calls > 0) { // Something stalled the queue...
    cerr << "ERROR: rank " << rank_ << " has " << incomplete_calls << " incomplete calls!" << endl;
    cerr << "This is likely caused by dangling MPI_Isend/MPI_Irecv(s). The OTF2 trace will be incomplete." << endl;

    int calls_to_print = min(incomplete_calls,25);
    cerr << "Printing " << calls_to_print << " calls" << endl;

    for (int i = 0; i < calls_to_print; i++) {
      auto& call = call_queue_.peekFront();
      call_queue_.call_queue.pop();
      cerr << "  ==> " << setw(15) << call.toString() << (call.isready?"\tREADY":"\tNOT READY")<< endl;
    }

    for (auto iter = call_queue_.requestBegin(); iter != call_queue_.requestEnd(); ++iter){
      cerr << "Stalled on request " << iter->first
           << " on " << iter->second->toString()
           << std::endl;
    }
  }
}

