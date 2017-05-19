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

#include "otf2_trace_replay.h"
#include "callbacks.h"
#include <algorithm>
#include <iomanip>

using std::cout;
using std::cerr;
using std::endl;
using std::min;
using std::setw;


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

void check_status(OTF2_ErrorCode status, const std::string& description)
{
  if (status != OTF2_SUCCESS){
    spkt_abort_printf("OTF2 Error: %s  %s",
                      OTF2_Error_GetName(status),
                      description.c_str());
  }
}

OTF2TraceReplayApp::OTF2TraceReplayApp(sprockit::sim_parameters* params,
        sumi::software_id sid, sstmac::sw::operating_system* os) :
  app(params, sid, os), mpi_(nullptr), call_queue_(this) {
  timescale_ = params->get_optional_double_param("otf2_timescale", 1.0);
  terminate_percent_ = params->get_optional_double_param("otf2_terminate_percent", 1);
  print_progress_ = params->get_optional_bool_param("otf2_print_progress", true);
  metafile_ = params->get_param("otf2_metafile");

  print_mpi_calls_ = params->get_optional_bool_param("otf2_print_mpi_calls", false);
  print_trace_events_ = params->get_optional_bool_param("otf2_print_trace_events", false);
  print_time_deltas_ = params->get_optional_bool_param("otf2_print_time_deltas", false);
  print_unknown_callback_ = params->get_optional_bool_param("otf2_print_unknown_callback", false);
}

void
OTF2TraceReplayApp::skeleton_main() {
  rank = this->tid();

  auto event_reader = initialize_event_reader();

  initiate_trace_replay(event_reader);
  verify_replay_success();

  OTF2_Reader_Close(event_reader);
}

sumi::mpi_api*
OTF2TraceReplayApp::GetMpi() {
  if (mpi_) return mpi_;

  mpi_ = get_api<sumi::mpi_api>();
  mpi_->set_generate_ids(false); // We use requests, comms, etc from OTF2 traces
  return mpi_;
}

CallQueue& OTF2TraceReplayApp::GetCallQueue() {
    return call_queue_;
}

// Indicate that we are starting an MPI call.
void OTF2TraceReplayApp::StartMpi(const sstmac::timestamp wall) {
  // Time not initialized
  if (compute_time == sstmac::timestamp::zero) return;

  if (PrintTimeDeltas()) {
    cout << "\u0394T " << (wall-compute_time).sec() << " seconds"<< endl;
  }

  compute((timescale_ * (wall - compute_time)));
}

void OTF2TraceReplayApp::EndMpi(const sstmac::timestamp wall) {
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
  app->total_events += numberOfEvents;

  return OTF2_CALLBACK_SUCCESS;
}

OTF2_Reader*
OTF2TraceReplayApp::initialize_event_reader() {
	// OTF2 has an excellent API
	uint64_t number_of_locations;
	//uint64_t trace_length = 0;
  auto reader = OTF2_Reader_Open(metafile_.c_str());
  OTF2_Reader_SetSerialCollectiveCallbacks(reader);
  check_status(OTF2_Reader_GetNumberOfLocations(reader, &number_of_locations), "OTF2_Reader_GetNumberOfLocations\n");

  if (number_of_locations <= rank) {
    cerr << "ERROR: Rank " << rank << " cannot participate in a trace replay with " << number_of_locations << " ranks" << endl;
    spkt_throw(sprockit::io_error, "ASSERT FAILED:", " Number of MPI ranks must match the number of trace files.");
  }

  struct c_vector* locations = (c_vector*) malloc(sizeof(*locations) + number_of_locations * sizeof(*locations->members));
  locations->capacity = number_of_locations;
  locations->size = 0;
  OTF2_GlobalDefReader* global_def_reader = OTF2_Reader_GetGlobalDefReader(reader);
  OTF2_GlobalDefReaderCallbacks* global_def_callbacks;
  global_def_callbacks = create_global_def_callbacks();
  OTF2_GlobalDefReaderCallbacks_SetLocationCallback(global_def_callbacks, &GlobDefLocation_Register);
  check_status( OTF2_Reader_RegisterGlobalDefCallbacks(reader, global_def_reader, global_def_callbacks, (void*)this),
                "OTF2_Reader_RegisterGlobalDefCallbacks\n");
  OTF2_GlobalDefReaderCallbacks_Delete(global_def_callbacks);
  uint64_t definitions_read = 0;
  check_status(OTF2_Reader_ReadAllGlobalDefinitions(reader, global_def_reader, &definitions_read),
               "OTF2_Reader_ReadAllGlobalDefinitions\n");

  for (size_t i = 0; i < locations->size; i++) {
    cout << "registering def reader" << endl;
      check_status(OTF2_Reader_SelectLocation(reader, locations->members[i]),
                   "OTF2_Reader_ReadAllGlobalDefinitions\n");
  }

  bool successful_open_def_files = OTF2_Reader_OpenDefFiles(reader)
                                   == OTF2_SUCCESS;
  check_status(OTF2_Reader_OpenEvtFiles(reader),
               "OTF2_Reader_OpenEvtFiles\n");

  for (size_t i = 0; i < locations->size; i++) {
    if (successful_open_def_files) {
      OTF2_DefReader* def_reader = OTF2_Reader_GetDefReader(reader,
                                   locations->members[i]);

      if (def_reader) {
          uint64_t def_reads = 0;
          check_status(
              OTF2_Reader_ReadAllLocalDefinitions(reader, def_reader,
                                                  &def_reads),
              "OTF2_Reader_ReadAllLocalDefinitions\n");
          check_status(OTF2_Reader_CloseDefReader(reader, def_reader),
                       "OTF2_Reader_CloseDefReader\n");
      }
    }
    OTF2_Reader_GetEvtReader(reader, locations->members[i]);
  }

  if (successful_open_def_files) {
    check_status(OTF2_Reader_CloseDefFiles(reader),
                 "OTF2_Reader_CloseDefFiles\n");
  }

  OTF2_Reader_CloseGlobalDefReader(reader, global_def_reader);
  OTF2_EvtReader* evt_reader = OTF2_Reader_GetEvtReader(reader, rank);
  OTF2_EvtReaderCallbacks* event_callbacks = create_evt_callbacks();
  check_status(OTF2_Reader_RegisterEvtCallbacks(reader, evt_reader,
                                       event_callbacks, (void*)this),
      "OTF2_Reader_RegisterEvtCallbacks\n");
  OTF2_EvtReaderCallbacks_Delete(event_callbacks);

  free(locations);

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

void OTF2TraceReplayApp::initiate_trace_replay(OTF2_Reader* reader) {
  // get the trace reader corresponding to the rank
  uint64_t locs = 0;
  OTF2_Reader_GetNumberOfLocations(reader, &locs);
  //cout << "detected " << locs << endl;

  OTF2_EvtReader* event_reader = OTF2_Reader_GetEvtReader(reader, rank);

  handle_events(reader, event_reader);

  if (rank == 0) std::cout << "OTF2 Trace replay complete" << endl;

  // cleanup
  check_status(OTF2_Reader_CloseEvtReader(reader, event_reader), "OTF2_Reader_CloseEvtReader");
  check_status(OTF2_Reader_CloseEvtFiles(reader), "OTF2_Reader_CloseEvtFiles\n");
}

void OTF2TraceReplayApp::verify_replay_success() {
  int incomplete_calls = call_queue_.GetDepth();

  if(incomplete_calls > 0) { // Something stalled the queue...
    cout << "ERROR: rank " << rank << " has " << incomplete_calls << " incomplete calls!" << endl;

    int calls_to_print = min(incomplete_calls,25);
    cout << "Printing " << calls_to_print << " calls" << endl;

    for (int i = 0; i < calls_to_print; i++) {
      auto call = call_queue_.Peek();
      call_queue_.call_queue.pop();
      cout << "  ==> " << setw(15) << call->ToString() << (call->isready?"\tREADY":"\tNOT READY")<< endl;
    }
  }
}

bool OTF2TraceReplayApp::PrintTraceEvents() {
	return print_trace_events_;
}

bool OTF2TraceReplayApp::PrintMpiCalls() {
	return print_mpi_calls_;
}
bool OTF2TraceReplayApp::PrintTimeDeltas() {
	return print_time_deltas_;
}
bool OTF2TraceReplayApp::PrintUnknownCallback() {
	return print_unknown_callback_;
}