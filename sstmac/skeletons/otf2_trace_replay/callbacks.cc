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

#include <sprockit/errors.h>
#include <functional>
#include <iomanip>

#include <sstmac/skeletons/otf2_trace_replay/structures.h>
#include <sstmac/skeletons/otf2_trace_replay/callbacks.h>

#include <sstmac/skeletons/otf2_trace_replay/mpicall.h>
#include <sstmac/skeletons/otf2_trace_replay/callid.h>
#include <sstmac/skeletons/otf2_trace_replay/callqueue.h>
#include <sstmac/skeletons/otf2_trace_replay/otf2_trace_replay.h>

using std::cout;
using std::cerr;
using std::endl;
using std::min;
using std::setw;

#if 0
#define DEF_PRINT(...) printf("DEF: " __VA_ARGS__)
#else
#define DEF_PRINT(...) ;
#endif

#if 1
#define EVENT_PRINT(...){ \
  cerr << "EVT (#" << setw(2) << ((OTF2TraceReplayApp*)userData)->rank() << "): " __VA_ARGS__ << endl; \
}
#else
#define EVENT_PRINT(...) ;
#endif

/******************************************************************************
 * Definition callbacks
 *
 * OTF2 definition reader will use these as callbacks when streaming through a
 * trace definition file. Definitions are necessary for converting callback
 * data (integer hashes) into meaningful data about MPI calls
 */

OTF2_CallbackCode def_clock_properties(
  void*    userData,
  uint64_t timerResolution,
  uint64_t globalOffset,
  uint64_t traceLength )
{
  auto app = (OTF2TraceReplayApp*)userData;
  app->otf2_clock_properties =  {
    timerResolution, globalOffset, traceLength
  };

  return OTF2_CALLBACK_SUCCESS;
}

OTF2_CallbackCode def_string(
  void*          userData,
  OTF2_StringRef self,
  const char*    str)
{
  auto app = (OTF2TraceReplayApp*)userData;
  app->otf2_string_table[self] = str;
  DEF_PRINT("STRING\n");
  return OTF2_CALLBACK_SUCCESS;
}

// probably not needed
OTF2_CallbackCode def_location(
  void*                 userData,
  OTF2_LocationRef      self,
  OTF2_StringRef        name,
  OTF2_LocationType     locationType,
  uint64_t              numberOfEvents,
  OTF2_LocationGroupRef locationGroup )
{
  return OTF2_CALLBACK_SUCCESS;
}

OTF2_CallbackCode def_location_group(
  void*                  userData,
  OTF2_LocationGroupRef  self,
  OTF2_StringRef         name,
  OTF2_LocationGroupType locationGroupType,
  OTF2_SystemTreeNodeRef systemTreeParent )
{
  return OTF2_CALLBACK_SUCCESS;
}

OTF2_CallbackCode def_region(
  void*           userData,
  OTF2_RegionRef  self,
  OTF2_StringRef  name,
  OTF2_StringRef  canonicalName,
  OTF2_StringRef  description,
  OTF2_RegionRole regionRole,
  OTF2_Paradigm   paradigm,
  OTF2_RegionFlag regionFlags,
  OTF2_StringRef  sourceFile,
  uint32_t        beginLineNumber,
  uint32_t        endLineNumber )
{
  auto app = (OTF2TraceReplayApp*)userData;

  auto& str = app->otf2_string_table[name];
  MPI_CALL_ID id = MPI_call_to_id.get(str);
  if (id != ID_NULL){
    app->otf2_regions[self] = id;
  }

  DEF_PRINT("REGION\n");
  return OTF2_CALLBACK_SUCCESS;
}

OTF2_CallbackCode def_callpath(
  void*            userData,
  OTF2_CallpathRef self,
  OTF2_CallpathRef parent,
  OTF2_RegionRef   region )
{
  auto app = (OTF2TraceReplayApp*)userData;
  app->otf2_callpaths.push_back({parent, region});

  DEF_PRINT("CALLPATH\n");
  return OTF2_CALLBACK_SUCCESS;
}

// May be useful for ID'ing threads
OTF2_CallbackCode def_group(
  void*           userData,
  OTF2_GroupRef   id,
  OTF2_StringRef  name,
  OTF2_GroupType  groupType,
  OTF2_Paradigm   paradigm,
  OTF2_GroupFlag  groupFlags,
  uint32_t        numberOfMembers,
  const uint64_t* members )
{
  auto app = (OTF2TraceReplayApp*)userData;

  if (groupType == OTF2_GROUP_TYPE_COMM_SELF){
    int me = app->rank();
    app->GetMpi()->group_create_with_id(id, 1, &me);
    app->otf2_groups[id] = true; //yes, needed
  } else {
    std::vector<int> member_vect(numberOfMembers);
    for (int i = 0; i < numberOfMembers; i++){
      member_vect[i] = members[i];
    }

    bool included = app->GetMpi()->group_create_with_id(id, numberOfMembers, member_vect.data());
    app->otf2_groups[id] = included;
  }

  DEF_PRINT("GROUP\n");
  return OTF2_CALLBACK_SUCCESS;
}

OTF2_CallbackCode def_comm(
  void*          userData,
  OTF2_CommRef   id,
  OTF2_StringRef name,
  OTF2_GroupRef  group,
  OTF2_CommRef   parent )
{
  auto app = (OTF2TraceReplayApp*)userData;
  app->GetMpi()->set_generate_ids(false);
  bool need_comm = app->otf2_groups[group];
  if (need_comm){
    app->GetMpi()->comm_create_with_id(MPI_COMM_WORLD, group, id);
  } else {
    auto& str = app->otf2_string_table[name];
    if (str == "MPI_COMM_WORLD"){
      MPI_Comm output = id;
      app->GetMpi()->comm_dup(MPI_COMM_WORLD, &output);
    } else if (str == "MPI_COMM_SELF"){
      MPI_Comm output = id;
      app->GetMpi()->comm_dup(MPI_COMM_SELF, &output);
    } else {
    }
  }
  DEF_PRINT("COMMUNICATOR\n");
  return OTF2_CALLBACK_SUCCESS;
}

// probably not needed
OTF2_CallbackCode
def_location_group_property(
	void*                 userData,
	OTF2_LocationGroupRef locationGroup,
	OTF2_StringRef        name,
	OTF2_Type             type,
  OTF2_AttributeValue   value )
{
  DEF_PRINT("LOCATION_GROUP_PROPERTY\n");
  return OTF2_CALLBACK_SUCCESS;
}

// probably not needed
OTF2_CallbackCode
def_location_property(
	void*               userData,
	OTF2_LocationRef    location,
	OTF2_StringRef      name,
	OTF2_Type           type,
  OTF2_AttributeValue value)
{
  DEF_PRINT("LOCATION_PROPERTY\n");
  return OTF2_CALLBACK_SUCCESS;
}

OTF2_CallbackCode
def_mapping_table(
    void*             userData,
    OTF2_MappingType  mappingType,
    const OTF2_IdMap* idMap )
{
  return OTF2_CALLBACK_SUCCESS;
}

/******************************************************************************
 * Event callbacks
 *
 * OTF2 event reader will use these as callbacks when streaming through a trace.
 */

// puts call into wait when wait does not have one, otherwise it will make a new wait
// Fundamental assumption: requests are always resolved inside MPI_Waits. This means
// an event_enter must have put one on the stack before this function is called.
void add_wait(OTF2TraceReplayApp* app, CallQueue& queue, MPI_Request requestID)
{
	auto wait_event = [=]() {
		MPI_Request req = requestID;
		app->GetMpi()->wait(&req, MPI_STATUS_IGNORE);
	};

  MpiCall& wait_call = queue.PeekBack();

  if (wait_call.on_trigger == nullptr) {
    // this is the first wait request
    // this could be part of a waitall, waitany, wait, or waitsome
    wait_call.on_trigger = wait_event;
  } else {
    // for trace replay, we have to breakdown MPI_Waitall into
    // a sequence of individual MPI_Wait calls
    // this is a 2nd or later wait request in a waitall (or waitsome)
    queue.emplaceCall(wait_call.start_time, app,
                      ID_MPI_Wait, wait_call.name,
                      wait_event);

    queue.CallReady(wait_call);

    // the previous call should not advance any time when executing
    wait_call.end_time = wait_call.start_time;

	}
}

OTF2_CallbackCode event_mpi_send(
    OTF2_LocationRef    location,
    OTF2_TimeStamp      time,
    uint64_t            eventPosition,
    void*               userData,
    OTF2_AttributeList* attributes,
    uint32_t            receiver,
    OTF2_CommRef        communicator,
    uint32_t            msgTag,
    uint64_t            msgLength ) {

    auto app = (OTF2TraceReplayApp*)userData;
    MpiCall* call = app->GetCallQueue().find_latest(ID_MPI_Send);
    MpiCall::assert_call(call, "Lookup for MPI_Send in 'event_mpi_send' returned NULL");

    call->on_trigger = [=]() {
      call->app->GetMpi()->send(nullptr, msgLength, MPI_BYTE, receiver, msgTag, communicator);
    };

    if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()) EVENT_PRINT("MPI SEND");
    return OTF2_CALLBACK_SUCCESS;
}

OTF2_CallbackCode event_mpi_isend(
    OTF2_LocationRef    location,
    OTF2_TimeStamp      time,
    uint64_t            eventPosition,
    void*               userData,
    OTF2_AttributeList* attributes,
    uint32_t            receiver,
    OTF2_CommRef        communicator,
    uint32_t            msgTag,
    uint64_t            msgLength,
    uint64_t            requestID ) {

    auto app = (OTF2TraceReplayApp*)userData;

    // event_enter has just put an isend on the top of the queue
    MpiCall& call = app->GetCallQueue().PeekBack();

    app->GetCallQueue().AddRequest(requestID, call);
    call.on_trigger = [=]() {
      MPI_Request req = requestID;
      app->GetMpi()->isend(nullptr, msgLength, MPI_BYTE, receiver, msgTag,
                                communicator, &req);
    };

    if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()){
      EVENT_PRINT("ISEND count" << msgLength << " tag " << msgTag << " id "
                  << requestID << " comm " << communicator << " dest " << receiver);
    }
    return OTF2_CALLBACK_SUCCESS;
}

OTF2_CallbackCode event_mpi_isend_complete(
    OTF2_LocationRef    location,
    OTF2_TimeStamp      time,
    uint64_t            eventPosition,
    void*               userData,
    OTF2_AttributeList* attributes,
    uint64_t            requestID )
{

	auto app = (OTF2TraceReplayApp*)userData;
	auto& callqueue = app->GetCallQueue();

	add_wait(app, app->GetCallQueue(), (MPI_Request)requestID);
	callqueue.RemoveRequest((MPI_Request)requestID);

	if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()) EVENT_PRINT("ISEND COMPLETE");
    return OTF2_CALLBACK_SUCCESS;
}

OTF2_CallbackCode event_mpi_irecv_request(
    OTF2_LocationRef    location,
    OTF2_TimeStamp      time,
    uint64_t            eventPosition,
    void*               userData,
    OTF2_AttributeList* attributes,
    uint64_t            requestID ) {

    auto app = (OTF2TraceReplayApp*)userData;
    MpiCall& call = app->GetCallQueue().PeekBack();

    app->GetCallQueue().AddRequest(requestID, call);

    if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()){
      EVENT_PRINT("IRECV REQUEST id: " << requestID );
    }
    return OTF2_CALLBACK_SUCCESS;
}

// This event is triggered inside a wait. Must finish the Irecv event,
// and find the parent wait call to give it this request
OTF2_CallbackCode event_mpi_irecv(
  OTF2_LocationRef    location,
  OTF2_TimeStamp      time,
  uint64_t            eventPosition,
  void*               userData,
  OTF2_AttributeList* attributes,
  uint32_t            sender,
  OTF2_CommRef        communicator,
  uint32_t            msgTag,
  uint64_t            msgLength,
  uint64_t            requestID )
{
  auto app = (OTF2TraceReplayApp*)userData;
  auto& callqueue = app->GetCallQueue();

  // finish off the Irecv call
  MpiCall* call = callqueue.FindRequest((MPI_Request)requestID);

  call->on_trigger = [=]() {
    MPI_Request req = requestID;
    app->GetMpi()->irecv(nullptr, msgLength, MPI_BYTE, sender, msgTag, communicator, &req);
  };

  add_wait(app, app->GetCallQueue(), (MPI_Request)requestID);
  callqueue.RemoveRequest((MPI_Request)requestID);
  callqueue.CallReady(call);

  if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents())
    EVENT_PRINT("IRECV count: " << msgLength << " source: " << sender << " tag: " << msgTag);
  return OTF2_CALLBACK_SUCCESS;
}

OTF2_CallbackCode event_mpi_recv(
    OTF2_LocationRef    location,
    OTF2_TimeStamp      time,
    uint64_t            eventPosition,
    void*               userData,
    OTF2_AttributeList* attributes,
    uint32_t            sender,
    OTF2_CommRef        communicator,
    uint32_t            msgTag,
    uint64_t            msgLength ) {

	auto app = (OTF2TraceReplayApp*)userData;
    MpiCall* call = app->GetCallQueue().find_latest(ID_MPI_Recv);
    MpiCall::assert_call(call, "Lookup for MpiIrecvCall in 'event_mpi_irecv_request' returned NULL");

    call->on_trigger = [=]() {
      app->GetMpi()->recv(nullptr, msgLength, MPI_BYTE,
                          sender, msgTag, communicator, MPI_STATUS_IGNORE);
    };

    if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()){
      EVENT_PRINT("RECV count: " << msgLength << " source: " << sender << " tag: " << msgTag);
    }
    return OTF2_CALLBACK_SUCCESS;
}

// TODO implement
OTF2_CallbackCode event_mpi_request_test(
    OTF2_LocationRef    location,
    OTF2_TimeStamp      time,
    uint64_t            eventPosition,
    void*               userData,
    OTF2_AttributeList* attributes,
    uint64_t            requestID ) {

	if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()) EVENT_PRINT("REQUEST TEST\n");
    return OTF2_CALLBACK_SUCCESS;
}

// TODO implement
OTF2_CallbackCode event_mpi_request_cancelled(
    OTF2_LocationRef    location,
    OTF2_TimeStamp      time,
    uint64_t            eventPosition,
    void*               userData,
    OTF2_AttributeList* attributes,
    uint64_t            requestID ) {

	if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()) EVENT_PRINT("REQUEST CANCELLED\n");
    return OTF2_CALLBACK_SUCCESS;
}

// probably not needed
OTF2_CallbackCode event_mpi_collective_begin(
    OTF2_LocationRef    location,
    OTF2_TimeStamp      time,
    uint64_t            eventPosition,
    void*               userData,
    OTF2_AttributeList* attributes ) {

	if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()) EVENT_PRINT("COLLECTIVE BEGIN\n");
    return OTF2_CALLBACK_SUCCESS;
}

/* OTF2_CollectiveOp_enum
    OTF2_COLLECTIVE_OP_BARRIER                       = 0,
    OTF2_COLLECTIVE_OP_BCAST                         = 1,
    OTF2_COLLECTIVE_OP_GATHER                        = 2,
    OTF2_COLLECTIVE_OP_GATHERV                       = 3,
    OTF2_COLLECTIVE_OP_SCATTER                       = 4,
    OTF2_COLLECTIVE_OP_SCATTERV                      = 5,
    OTF2_COLLECTIVE_OP_ALLGATHER                     = 6,
    OTF2_COLLECTIVE_OP_ALLGATHERV                    = 7,
    OTF2_COLLECTIVE_OP_ALLTOALL                      = 8,
    OTF2_COLLECTIVE_OP_ALLTOALLV                     = 9,
    OTF2_COLLECTIVE_OP_ALLTOALLW                     = 10,
    OTF2_COLLECTIVE_OP_ALLREDUCE                     = 11,
    OTF2_COLLECTIVE_OP_REDUCE                        = 12,
    OTF2_COLLECTIVE_OP_REDUCE_SCATTER                = 13,
    OTF2_COLLECTIVE_OP_SCAN                          = 14,
 */

// otf2 doesn't save mpi ops, so pick one for the replay
#define OTF2_OP MPI_MAX

OTF2_CallbackCode event_mpi_collective_end(
    OTF2_LocationRef    location,
    OTF2_TimeStamp      time,
    uint64_t            eventPosition,
    void*               userData,
    OTF2_AttributeList* attributes,
    OTF2_CollectiveOp   collectiveOp,
    OTF2_CommRef        comm,
    uint32_t            root,
    uint64_t            sizeSent,
    uint64_t            sizeReceived ) {

    auto app = (OTF2TraceReplayApp*)userData;
    auto comm_size = app->GetMpi()->get_comm(comm)->size();
#define HANDLE_CASE(op, ...) case op : { \
            auto& call = app->GetCallQueue().PeekBack(); \
            __VA_ARGS__; \
            } break;

    // Each case is interlaced with output from a Scorep trace on 4 ranks sending 1 int with root 0 (where applicable)
    switch (collectiveOp) {
    HANDLE_CASE(OTF2_COLLECTIVE_OP_BARRIER,   call.on_trigger = [=]() {app->GetMpi()->barrier(comm);})
    //MPI_COLLECTIVE_END                         0     7399555605472008  Operation: BCAST, Communicator: "MPI_COMM_WORLD" <0>, Root: 0 ("Master thread" <0>), Sent: 16, Received: 4
    //MPI_COLLECTIVE_END                         2     7399555605473214  Operation: BCAST, Communicator: "MPI_COMM_WORLD" <0>, Root: 0 ("Master thread" <0>), Sent: 0, Received: 4
    //MPI_COLLECTIVE_END                         1     7399555605475368  Operation: BCAST, Communicator: "MPI_COMM_WORLD" <0>, Root: 0 ("Master thread" <0>), Sent: 0, Received: 4
    //MPI_COLLECTIVE_END                         3     7399555605478615  Operation: BCAST, Communicator: "MPI_COMM_WORLD" <0>, Root: 0 ("Master thread" <0>), Sent: 0, Received: 4
    HANDLE_CASE(OTF2_COLLECTIVE_OP_BCAST,     call.on_trigger = [=]() {app->GetMpi()->bcast(sizeReceived, MPI_BYTE, root, comm);})
    //MPI_COLLECTIVE_END                         1     7556711140931805  Operation: GATHER, Communicator: "MPI_COMM_WORLD" <0>, Root: 0 ("Master thread" <0>), Sent: 4, Received: 0
    //MPI_COLLECTIVE_END                         2     7556711141055857  Operation: GATHER, Communicator: "MPI_COMM_WORLD" <0>, Root: 0 ("Master thread" <0>), Sent: 4, Received: 0
    //MPI_COLLECTIVE_END                         3     7556711141080043  Operation: GATHER, Communicator: "MPI_COMM_WORLD" <0>, Root: 0 ("Master thread" <0>), Sent: 4, Received: 0
    //MPI_COLLECTIVE_END                         0     7556711141128100  Operation: GATHER, Communicator: "MPI_COMM_WORLD" <0>, Root: 0 ("Master thread" <0>), Sent: 4, Received: 16
    HANDLE_CASE(OTF2_COLLECTIVE_OP_GATHER,    call.on_trigger = [=]() {app->GetMpi()->gather(sizeSent, MPI_BYTE, sizeSent, MPI_BYTE, root, comm);})
    HANDLE_CASE(OTF2_COLLECTIVE_OP_GATHERV,   call.on_trigger = [=]() {std::cout << "MPI_Gatherv not implemented by Sumi-MPI" << std::endl;})
    //MPI_COLLECTIVE_END                         0     7557948283284700  Operation: SCATTER, Communicator: "MPI_COMM_WORLD" <0>, Root: 0 ("Master thread" <0>), Sent: 16, Received: 4
    //MPI_COLLECTIVE_END                         1     7557948283284988  Operation: SCATTER, Communicator: "MPI_COMM_WORLD" <0>, Root: 0 ("Master thread" <0>), Sent: 0, Received: 4
    //MPI_COLLECTIVE_END                         2     7557948283285064  Operation: SCATTER, Communicator: "MPI_COMM_WORLD" <0>, Root: 0 ("Master thread" <0>), Sent: 0, Received: 4
    //MPI_COLLECTIVE_END                         3     7557948283301477  Operation: SCATTER, Communicator: "MPI_COMM_WORLD" <0>, Root: 0 ("Master thread" <0>), Sent: 0, Received: 4
    HANDLE_CASE(OTF2_COLLECTIVE_OP_SCATTER,   call.on_trigger = [=]() {app->GetMpi()->scatter(sizeReceived, MPI_BYTE, sizeReceived, MPI_BYTE, root, comm);})
    HANDLE_CASE(OTF2_COLLECTIVE_OP_SCATTERV,  call.on_trigger = [=]() {std::cout << "MPI_Scatterv not implemented by Sumi-MPI" << std::endl;})
    //MPI_COLLECTIVE_END                         2     7558923358856786  Operation: ALLGATHER, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 16, Received: 16
    //MPI_COLLECTIVE_END                         0     7558923358857236  Operation: ALLGATHER, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 16, Received: 16
    //MPI_COLLECTIVE_END                         1     7558923358857724  Operation: ALLGATHER, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 16, Received: 16
    //MPI_COLLECTIVE_END                         3     7558923358859170  Operation: ALLGATHER, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 16, Received: 16
    HANDLE_CASE(OTF2_COLLECTIVE_OP_ALLGATHER, call.on_trigger = [=]() {app->GetMpi()->allgather(sizeSent/comm_size, MPI_BYTE, sizeReceived/comm_size, MPI_BYTE, comm);})
    HANDLE_CASE(OTF2_COLLECTIVE_OP_ALLGATHERV,call.on_trigger = [=]() {std::cout << "MPI_Allgatherv not implemented by Sumi-MPI" << std::endl;})
    //MPI_COLLECTIVE_END                         3     7561894889294896  Operation: ALLTOALL, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 16, Received: 16
    //MPI_COLLECTIVE_END                         1     7561894889299008  Operation: ALLTOALL, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 16, Received: 16
    //MPI_COLLECTIVE_END                         2     7561894889326469  Operation: ALLTOALL, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 16, Received: 16
    //MPI_COLLECTIVE_END                         0     7561894889327660  Operation: ALLTOALL, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 16, Received: 16
    HANDLE_CASE(OTF2_COLLECTIVE_OP_ALLTOALL,  call.on_trigger = [=]() {app->GetMpi()->alltoall(sizeSent/comm_size, MPI_BYTE, sizeReceived/comm_size, MPI_BYTE, comm);})
    HANDLE_CASE(OTF2_COLLECTIVE_OP_ALLTOALLV, call.on_trigger = [=]() {std::cout << "MPI_Alltoallv not implemented by Sumi-MPI" << std::endl;})
    //MPI_COLLECTIVE_END                         2     7577512533313071  Operation: ALLREDUCE, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 16, Received: 16
    //MPI_COLLECTIVE_END                         0     7577512533314445  Operation: ALLREDUCE, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 16, Received: 16
    //MPI_COLLECTIVE_END                         1     7577512533314888  Operation: ALLREDUCE, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 16, Received: 16
    //MPI_COLLECTIVE_END                         3     7577512533317046  Operation: ALLREDUCE, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 16, Received: 16
    // Cannot model accurately without type size information
    HANDLE_CASE(OTF2_COLLECTIVE_OP_ALLREDUCE, call.on_trigger = [=]() {app->GetMpi()->allreduce((sizeReceived/comm_size)/4, MPI_INT, OTF2_OP, comm);})
    //MPI_COLLECTIVE_END                         1     7407606802153808  Operation: REDUCE, Communicator: "MPI_COMM_WORLD" <0>, Root: 2 ("Master thread" <2>), Sent: 4, Received: 0
    //MPI_COLLECTIVE_END                         3     7407606802341911  Operation: REDUCE, Communicator: "MPI_COMM_WORLD" <0>, Root: 2 ("Master thread" <2>), Sent: 4, Received: 0
    //MPI_COLLECTIVE_END                         0     7407606802394234  Operation: REDUCE, Communicator: "MPI_COMM_WORLD" <0>, Root: 2 ("Master thread" <2>), Sent: 4, Received: 0
    //MPI_COLLECTIVE_END                         2     7407606802394949  Operation: REDUCE, Communicator: "MPI_COMM_WORLD" <0>, Root: 2 ("Master thread" <2>), Sent: 4, Received: 16
    // Cannot model accurately without type size information
    HANDLE_CASE(OTF2_COLLECTIVE_OP_REDUCE,    call.on_trigger = [=]() {app->GetMpi()->reduce(sizeSent/4, MPI_INT, OTF2_OP, root, comm);})
    //MPI_COLLECTIVE_END                         2     7565304070746368  Operation: REDUCE_SCATTER, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 4, Received: 0
    //MPI_COLLECTIVE_END                         3     7565304070843972  Operation: REDUCE_SCATTER, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 4, Received: 0
    //MPI_COLLECTIVE_END                         1     7565304070881025  Operation: REDUCE_SCATTER, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 4, Received: 0
    //MPI_COLLECTIVE_END                         0     7565304070881276  Operation: REDUCE_SCATTER, Communicator: "MPI_COMM_WORLD" <0>, Root: UNDEFINED, Sent: 4, Received: 16
    // Cannot model accurately without type size information
    HANDLE_CASE(OTF2_COLLECTIVE_OP_REDUCE_SCATTER, call.on_trigger = [=]() {
      int avg_size = sizeReceived/comm_size;
      std::vector<int> recv_sizes(comm_size);
      for (auto& recv_size : recv_sizes) recv_size = avg_size;
      // Cannot reconstruct reduce_scatter from OTF2, only a rough approximation
      app->GetMpi()->reduce_scatter(recv_sizes.data(), MPI_BYTE, OTF2_OP, comm);
    })
    HANDLE_CASE(OTF2_COLLECTIVE_OP_SCAN,      call.on_trigger = [=]() {app->GetMpi()->scan((sizeSent+sizeReceived)/comm_size, MPI_BYTE, OTF2_OP, comm);})

    default:
        std::cerr << "ERROR: Collective not handled; " << (int)collectiveOp << endl;
    }

#undef HANDLE_CASE
#undef END_CASE
    if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()) EVENT_PRINT("COLLECTIVE END");
    return OTF2_CALLBACK_SUCCESS;
}

// probably not needed
OTF2_CallbackCode event_parameter_string(
    OTF2_LocationRef    location,
    OTF2_TimeStamp      time,
    uint64_t            eventPosition,
    void*               userData,
    OTF2_AttributeList* attributes,
    OTF2_ParameterRef   parameter,
    OTF2_StringRef      string ) {

    if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()) EVENT_PRINT("PARAMETER STRING\n");
    return OTF2_CALLBACK_SUCCESS;
}

// Ignoring MPI calls in event_enter and event_leave will prevent it being
// added to the queue.
#define CASE_IGNORE(call_id) case ID_##call_id : \
	break;

#define CASE_ADD_CALL(call_id) case ID_##call_id: \
    { \
  app->GetCallQueue().emplaceCall(time,app,ID_##call_id,#call_id); \
  if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()) \
    EVENT_PRINT("ENTER " << app->GetCallQueue().PeekBack().ToString() << " time: " << time); \
  } \
  break;

#define ADD_SPECIAL_CASE(call_id) case ID_##call_id:

#define END_SPECIAL_CASE break;

OTF2_CallbackCode event_enter(
    OTF2_LocationRef    location,
    OTF2_TimeStamp      time,
    uint64_t            eventPosition,
    void*               userData,
    OTF2_AttributeList* attributes,
    OTF2_RegionRef      region ) {

    auto app = (OTF2TraceReplayApp*)userData;

    auto iter = app->otf2_regions.find(region);
    if (iter == app->otf2_regions.end()){
      if (app->PrintUnknownCallback()) {
        std::cout << "unknown OTF2 region \""
                  << region
                  << "\"" << std::endl;
      }
      return OTF2_CALLBACK_SUCCESS;
    }
    MPI_CALL_ID id = iter->second;

    switch (id) {
    CASE_ADD_CALL(MPI_Abort)
    CASE_ADD_CALL(MPI_Accumulate)
		CASE_ADD_CALL(MPI_Add_error_class)
		CASE_ADD_CALL(MPI_Add_error_code)
		CASE_ADD_CALL(MPI_Add_error_string)
		CASE_ADD_CALL(MPI_Address)
		CASE_ADD_CALL(MPI_Aint_add)
		CASE_ADD_CALL(MPI_Aint_diff)
		CASE_ADD_CALL(MPI_Allgather)
		CASE_ADD_CALL(MPI_Allgatherv)
		CASE_ADD_CALL(MPI_Alloc_mem)
		CASE_ADD_CALL(MPI_Allreduce)
		CASE_ADD_CALL(MPI_Alltoall)
		CASE_ADD_CALL(MPI_Alltoallv)
		CASE_ADD_CALL(MPI_Alltoallw)
		CASE_ADD_CALL(MPI_Attr_delete)
		CASE_ADD_CALL(MPI_Attr_get)
		CASE_ADD_CALL(MPI_Attr_put)
		CASE_ADD_CALL(MPI_Barrier)
		CASE_ADD_CALL(MPI_Bcast)
		CASE_ADD_CALL(MPI_Bsend)
		CASE_ADD_CALL(MPI_Bsend_init)
		CASE_ADD_CALL(MPI_Buffer_attach)
		CASE_ADD_CALL(MPI_Buffer_detach)
		CASE_ADD_CALL(MPI_Cancel)
		CASE_ADD_CALL(MPI_Cart_coords)
		CASE_ADD_CALL(MPI_Cart_create)
		CASE_ADD_CALL(MPI_Cart_get)
		CASE_ADD_CALL(MPI_Cart_map)
		CASE_ADD_CALL(MPI_Cart_rank)
		CASE_ADD_CALL(MPI_Cart_shift)
		CASE_ADD_CALL(MPI_Cart_sub)
		CASE_ADD_CALL(MPI_Cartdim_get)
		CASE_ADD_CALL(MPI_Close_port)
		CASE_ADD_CALL(MPI_Comm_accept)
		CASE_ADD_CALL(MPI_Comm_call_errhandler)
		CASE_ADD_CALL(MPI_Comm_compare)
		CASE_ADD_CALL(MPI_Comm_connect)
		CASE_ADD_CALL(MPI_Comm_create)
		CASE_ADD_CALL(MPI_Comm_create_errhandler)
		CASE_ADD_CALL(MPI_Comm_create_group)
		CASE_ADD_CALL(MPI_Comm_create_keyval)
		CASE_ADD_CALL(MPI_Comm_delete_attr)
		CASE_ADD_CALL(MPI_Comm_disconnect)
		CASE_ADD_CALL(MPI_Comm_dup)
		CASE_ADD_CALL(MPI_Comm_dup_with_info)
		CASE_ADD_CALL(MPI_Comm_free)
		CASE_ADD_CALL(MPI_Comm_free_keyval)
		CASE_ADD_CALL(MPI_Comm_get_attr)
		CASE_ADD_CALL(MPI_Comm_get_errhandler)
		CASE_ADD_CALL(MPI_Comm_get_info)
		CASE_ADD_CALL(MPI_Comm_get_name)
		CASE_ADD_CALL(MPI_Comm_get_parent)
		CASE_ADD_CALL(MPI_Comm_group)
		CASE_ADD_CALL(MPI_Comm_idup)
		CASE_ADD_CALL(MPI_Comm_join)
		CASE_IGNORE(MPI_Comm_rank)
		CASE_ADD_CALL(MPI_Comm_remote_group)
		CASE_ADD_CALL(MPI_Comm_remote_size)
		CASE_ADD_CALL(MPI_Comm_set_attr)
		CASE_ADD_CALL(MPI_Comm_set_errhandler)
		CASE_ADD_CALL(MPI_Comm_set_info)
		CASE_ADD_CALL(MPI_Comm_set_name)
		CASE_IGNORE(MPI_Comm_size)
		CASE_IGNORE(MPI_Comm_spawn)
		CASE_IGNORE(MPI_Comm_spawn_multiple)
		CASE_IGNORE(MPI_Comm_split)
		CASE_IGNORE(MPI_Comm_split_type)
		CASE_IGNORE(MPI_Comm_test_inter)
		CASE_ADD_CALL(MPI_Compare_and_swap)
		CASE_ADD_CALL(MPI_Dims_create)
		CASE_ADD_CALL(MPI_Dist_graph_create)
		CASE_ADD_CALL(MPI_Dist_graph_create_adjacent)
		CASE_ADD_CALL(MPI_Dist_graph_neighbors)
		CASE_ADD_CALL(MPI_Dist_graph_neighbors_count)
		CASE_ADD_CALL(MPI_Errhandler_create)
		CASE_ADD_CALL(MPI_Errhandler_free)
		CASE_ADD_CALL(MPI_Errhandler_get)
		CASE_ADD_CALL(MPI_Errhandler_set)
		CASE_ADD_CALL(MPI_Error_class)
		CASE_ADD_CALL(MPI_Error_string)
		CASE_ADD_CALL(MPI_Exscan)
		CASE_ADD_CALL(MPI_Fetch_and_op)
		CASE_ADD_CALL(MPI_File_c2f)
		CASE_ADD_CALL(MPI_File_call_errhandler)
		CASE_ADD_CALL(MPI_File_close)
		CASE_ADD_CALL(MPI_File_create_errhandler)
		CASE_ADD_CALL(MPI_File_delete)
		CASE_ADD_CALL(MPI_File_f2c)
		CASE_ADD_CALL(MPI_File_get_amode)
		CASE_ADD_CALL(MPI_File_get_atomicity)
		CASE_ADD_CALL(MPI_File_get_byte_offset)
		CASE_ADD_CALL(MPI_File_get_errhandler)
		CASE_ADD_CALL(MPI_File_get_group)
		CASE_ADD_CALL(MPI_File_get_info)
		CASE_ADD_CALL(MPI_File_get_position)
		CASE_ADD_CALL(MPI_File_get_position_shared)
		CASE_ADD_CALL(MPI_File_get_size)
		CASE_ADD_CALL(MPI_File_get_type_extent)
		CASE_ADD_CALL(MPI_File_get_view)
		CASE_ADD_CALL(MPI_File_iread)
		CASE_ADD_CALL(MPI_File_iread_all)
		CASE_ADD_CALL(MPI_File_iread_at)
		CASE_ADD_CALL(MPI_File_iread_at_all)
		CASE_ADD_CALL(MPI_File_iread_shared)
		CASE_ADD_CALL(MPI_File_iwrite)
		CASE_ADD_CALL(MPI_File_iwrite_all)
		CASE_ADD_CALL(MPI_File_iwrite_at)
		CASE_ADD_CALL(MPI_File_iwrite_at_all)
		CASE_ADD_CALL(MPI_File_iwrite_shared)
		CASE_ADD_CALL(MPI_File_open)
		CASE_ADD_CALL(MPI_File_preallocate)
		CASE_ADD_CALL(MPI_File_read)
		CASE_ADD_CALL(MPI_File_read_all)
		CASE_ADD_CALL(MPI_File_read_all_begin)
		CASE_ADD_CALL(MPI_File_read_all_end)
		CASE_ADD_CALL(MPI_File_read_at)
		CASE_ADD_CALL(MPI_File_read_at_all)
		CASE_ADD_CALL(MPI_File_read_at_all_begin)
		CASE_ADD_CALL(MPI_File_read_at_all_end)
		CASE_ADD_CALL(MPI_File_read_ordered)
		CASE_ADD_CALL(MPI_File_read_ordered_begin)
		CASE_ADD_CALL(MPI_File_read_ordered_end)
		CASE_ADD_CALL(MPI_File_read_shared)
		CASE_ADD_CALL(MPI_File_seek)
		CASE_ADD_CALL(MPI_File_seek_shared)
		CASE_ADD_CALL(MPI_File_set_atomicity)
		CASE_ADD_CALL(MPI_File_set_errhandler)
		CASE_ADD_CALL(MPI_File_set_info)
		CASE_ADD_CALL(MPI_File_set_size)
		CASE_ADD_CALL(MPI_File_set_view)
		CASE_ADD_CALL(MPI_File_sync)
		CASE_ADD_CALL(MPI_File_write)
		CASE_ADD_CALL(MPI_File_write_all)
		CASE_ADD_CALL(MPI_File_write_all_begin)
		CASE_ADD_CALL(MPI_File_write_all_end)
		CASE_ADD_CALL(MPI_File_write_at)
		CASE_ADD_CALL(MPI_File_write_at_all)
		CASE_ADD_CALL(MPI_File_write_at_all_begin)
		CASE_ADD_CALL(MPI_File_write_at_all_end)
		CASE_ADD_CALL(MPI_File_write_ordered)
		CASE_ADD_CALL(MPI_File_write_ordered_begin)
		CASE_ADD_CALL(MPI_File_write_ordered_end)
		CASE_ADD_CALL(MPI_File_write_shared)
		CASE_ADD_CALL(MPI_Finalize)
		CASE_IGNORE(MPI_Finalized)
		CASE_ADD_CALL(MPI_Free_mem)
		CASE_ADD_CALL(MPI_Gather)
		CASE_ADD_CALL(MPI_Gatherv)
		CASE_ADD_CALL(MPI_Get)
		CASE_ADD_CALL(MPI_Get_accumulate)
		CASE_ADD_CALL(MPI_Get_address)
		CASE_ADD_CALL(MPI_Get_count)
		CASE_ADD_CALL(MPI_Get_elements)
		CASE_ADD_CALL(MPI_Get_elements_x)
		CASE_ADD_CALL(MPI_Get_library_version)
		CASE_ADD_CALL(MPI_Get_processor_name)
		CASE_ADD_CALL(MPI_Get_version)
		CASE_ADD_CALL(MPI_Graph_create)
		CASE_ADD_CALL(MPI_Graph_get)
		CASE_ADD_CALL(MPI_Graph_map)
		CASE_ADD_CALL(MPI_Graph_neighbors)
		CASE_ADD_CALL(MPI_Graph_neighbors_count)
		CASE_ADD_CALL(MPI_Graphdims_get)
		CASE_ADD_CALL(MPI_Grequest_complete)
		CASE_ADD_CALL(MPI_Grequest_start)
		CASE_ADD_CALL(MPI_Group_compare)
		CASE_ADD_CALL(MPI_Group_difference)
		CASE_ADD_CALL(MPI_Group_excl)
		CASE_ADD_CALL(MPI_Group_free)
		CASE_ADD_CALL(MPI_Group_incl)
		CASE_ADD_CALL(MPI_Group_intersection)
		CASE_ADD_CALL(MPI_Group_range_excl)
		CASE_ADD_CALL(MPI_Group_range_incl)
		CASE_ADD_CALL(MPI_Group_rank)
		CASE_ADD_CALL(MPI_Group_size)
		CASE_ADD_CALL(MPI_Group_translate_ranks)
		CASE_ADD_CALL(MPI_Group_union)
		CASE_ADD_CALL(MPI_Iallgather)
		CASE_ADD_CALL(MPI_Iallgatherv)
		CASE_ADD_CALL(MPI_Iallreduce)
		CASE_ADD_CALL(MPI_Ialltoall)
		CASE_ADD_CALL(MPI_Ialltoallv)
		CASE_ADD_CALL(MPI_Ialltoallw)
		CASE_ADD_CALL(MPI_Ibarrier)
		CASE_ADD_CALL(MPI_Ibcast)
		CASE_ADD_CALL(MPI_Ibsend)
		CASE_ADD_CALL(MPI_Iexscan)
		CASE_ADD_CALL(MPI_Igather)
		CASE_ADD_CALL(MPI_Igatherv)
		CASE_ADD_CALL(MPI_Improbe)
		CASE_ADD_CALL(MPI_Imrecv)
		CASE_ADD_CALL(MPI_Ineighbor_allgather)
		CASE_ADD_CALL(MPI_Ineighbor_allgatherv)
		CASE_ADD_CALL(MPI_Ineighbor_alltoall)
		CASE_ADD_CALL(MPI_Ineighbor_alltoallv)
		CASE_ADD_CALL(MPI_Ineighbor_alltoallw)
		CASE_ADD_CALL(MPI_Info_create)
		CASE_ADD_CALL(MPI_Info_delete)
		CASE_ADD_CALL(MPI_Info_dup)
		CASE_ADD_CALL(MPI_Info_free)
		CASE_ADD_CALL(MPI_Info_get)
		CASE_ADD_CALL(MPI_Info_get_nkeys)
		CASE_ADD_CALL(MPI_Info_get_nthkey)
		CASE_ADD_CALL(MPI_Info_get_valuelen)
		CASE_ADD_CALL(MPI_Info_set)
		CASE_ADD_CALL(MPI_Init)
		CASE_ADD_CALL(MPI_Init_thread)
		CASE_IGNORE(MPI_Initialized)
		CASE_ADD_CALL(MPI_Intercomm_create)
		CASE_ADD_CALL(MPI_Intercomm_merge)
		CASE_ADD_CALL(MPI_Iprobe)
		CASE_ADD_CALL(MPI_Irecv)
		CASE_ADD_CALL(MPI_Ireduce)
		CASE_ADD_CALL(MPI_Ireduce_scatter)
		CASE_ADD_CALL(MPI_Ireduce_scatter_block)
		CASE_ADD_CALL(MPI_Irsend)
		CASE_ADD_CALL(MPI_Is_thread_main)
		CASE_ADD_CALL(MPI_Iscan)
		CASE_ADD_CALL(MPI_Iscatter)
		CASE_ADD_CALL(MPI_Iscatterv)
		CASE_ADD_CALL(MPI_Isend)
		CASE_ADD_CALL(MPI_Issend)
		CASE_ADD_CALL(MPI_Keyval_create)
		CASE_ADD_CALL(MPI_Keyval_free)
		CASE_ADD_CALL(MPI_Lookup_name)
		CASE_ADD_CALL(MPI_Mprobe)
		CASE_ADD_CALL(MPI_Mrecv)
		CASE_ADD_CALL(MPI_Neighbor_allgather)
		CASE_ADD_CALL(MPI_Neighbor_allgatherv)
		CASE_ADD_CALL(MPI_Neighbor_alltoall)
		CASE_ADD_CALL(MPI_Neighbor_alltoallv)
		CASE_ADD_CALL(MPI_Neighbor_alltoallw)
		CASE_ADD_CALL(MPI_Op_commute)
		CASE_ADD_CALL(MPI_Op_create)
		CASE_ADD_CALL(MPI_Op_free)
		CASE_ADD_CALL(MPI_Open_port)
		CASE_ADD_CALL(MPI_Pack)
		CASE_ADD_CALL(MPI_Pack_external)
		CASE_ADD_CALL(MPI_Pack_external_size)
		CASE_ADD_CALL(MPI_Pack_size)
		CASE_ADD_CALL(MPI_Pcontrol)
		CASE_ADD_CALL(MPI_Probe)
		CASE_ADD_CALL(MPI_Publish_name)
		CASE_ADD_CALL(MPI_Put)
		CASE_ADD_CALL(MPI_Query_thread)
		CASE_ADD_CALL(MPI_Raccumulate)
		CASE_ADD_CALL(MPI_Recv)
		CASE_ADD_CALL(MPI_Recv_init)
		CASE_ADD_CALL(MPI_Reduce)
		CASE_ADD_CALL(MPI_Reduce_local)
		CASE_ADD_CALL(MPI_Reduce_scatter)
		CASE_ADD_CALL(MPI_Reduce_scatter_block)
		CASE_ADD_CALL(MPI_Register_datarep)
		CASE_ADD_CALL(MPI_Request_free)
		CASE_ADD_CALL(MPI_Request_get_status)
		CASE_ADD_CALL(MPI_Rget)
		CASE_ADD_CALL(MPI_Rget_accumulate)
		CASE_ADD_CALL(MPI_Rput)
		CASE_ADD_CALL(MPI_Rsend)
		CASE_ADD_CALL(MPI_Rsend_init)
		CASE_ADD_CALL(MPI_Scan)
		CASE_ADD_CALL(MPI_Scatter)
		CASE_ADD_CALL(MPI_Scatterv)
		CASE_ADD_CALL(MPI_Send)
		CASE_ADD_CALL(MPI_Send_init)
		CASE_ADD_CALL(MPI_Sendrecv)
		CASE_ADD_CALL(MPI_Sendrecv_replace)
		CASE_ADD_CALL(MPI_Ssend)
		CASE_ADD_CALL(MPI_Ssend_init)
		CASE_ADD_CALL(MPI_Start)
		CASE_ADD_CALL(MPI_Startall)
		CASE_ADD_CALL(MPI_Status_set_cancelled)
		CASE_ADD_CALL(MPI_Status_set_elements)
		CASE_ADD_CALL(MPI_Status_set_elements_x)
		CASE_ADD_CALL(MPI_T_category_changed)
		CASE_ADD_CALL(MPI_T_category_get_categories)
		CASE_ADD_CALL(MPI_T_category_get_cvars)
		CASE_ADD_CALL(MPI_T_category_get_info)
		CASE_ADD_CALL(MPI_T_category_get_num)
		CASE_ADD_CALL(MPI_T_category_get_pvars)
		CASE_ADD_CALL(MPI_T_cvar_get_info)
		CASE_ADD_CALL(MPI_T_cvar_get_num)
		CASE_ADD_CALL(MPI_T_cvar_handle_alloc)
		CASE_ADD_CALL(MPI_T_cvar_handle_free)
		CASE_ADD_CALL(MPI_T_cvar_read)
		CASE_ADD_CALL(MPI_T_cvar_write)
		CASE_ADD_CALL(MPI_T_enum_get_info)
		CASE_ADD_CALL(MPI_T_enum_get_item)
		CASE_ADD_CALL(MPI_T_finalize)
		CASE_ADD_CALL(MPI_T_init_thread)
		CASE_ADD_CALL(MPI_T_pvar_get_info)
		CASE_ADD_CALL(MPI_T_pvar_get_num)
		CASE_ADD_CALL(MPI_T_pvar_handle_alloc)
		CASE_ADD_CALL(MPI_T_pvar_handle_free)
		CASE_ADD_CALL(MPI_T_pvar_read)
		CASE_ADD_CALL(MPI_T_pvar_readreset)
		CASE_ADD_CALL(MPI_T_pvar_reset)
		CASE_ADD_CALL(MPI_T_pvar_session_create)
		CASE_ADD_CALL(MPI_T_pvar_session_free)
		CASE_ADD_CALL(MPI_T_pvar_start)
		CASE_ADD_CALL(MPI_T_pvar_stop)
		CASE_ADD_CALL(MPI_T_pvar_write)
		CASE_ADD_CALL(MPI_Test)
		CASE_ADD_CALL(MPI_Test_cancelled)
		CASE_ADD_CALL(MPI_Testall)
		CASE_ADD_CALL(MPI_Testany)
		CASE_ADD_CALL(MPI_Testsome)
		CASE_ADD_CALL(MPI_Topo_test)
		CASE_ADD_CALL(MPI_Type_commit)
		CASE_ADD_CALL(MPI_Type_contiguous)
		CASE_ADD_CALL(MPI_Type_create_darray)
		CASE_ADD_CALL(MPI_Type_create_hindexed)
		CASE_ADD_CALL(MPI_Type_create_hindexed_block)
		CASE_ADD_CALL(MPI_Type_create_hvector)
		CASE_ADD_CALL(MPI_Type_create_indexed_block)
		CASE_ADD_CALL(MPI_Type_create_keyval)
		CASE_ADD_CALL(MPI_Type_create_resized)
		CASE_ADD_CALL(MPI_Type_create_struct)
		CASE_ADD_CALL(MPI_Type_create_subarray)
		CASE_ADD_CALL(MPI_Type_delete_attr)
		CASE_ADD_CALL(MPI_Type_dup)
		CASE_ADD_CALL(MPI_Type_extent)
		CASE_ADD_CALL(MPI_Type_free)
		CASE_ADD_CALL(MPI_Type_free_keyval)
		CASE_ADD_CALL(MPI_Type_get_attr)
		CASE_ADD_CALL(MPI_Type_get_contents)
		CASE_ADD_CALL(MPI_Type_get_envelope)
		CASE_ADD_CALL(MPI_Type_get_extent)
		CASE_ADD_CALL(MPI_Type_get_extent_x)
		CASE_ADD_CALL(MPI_Type_get_name)
		CASE_ADD_CALL(MPI_Type_get_true_extent)
		CASE_ADD_CALL(MPI_Type_get_true_extent_x)
		CASE_ADD_CALL(MPI_Type_hindexed)
		CASE_ADD_CALL(MPI_Type_hvector)
		CASE_ADD_CALL(MPI_Type_indexed)
		CASE_ADD_CALL(MPI_Type_lb)
		CASE_ADD_CALL(MPI_Type_match_size)
		CASE_ADD_CALL(MPI_Type_set_attr)
		CASE_ADD_CALL(MPI_Type_set_name)
		CASE_ADD_CALL(MPI_Type_size)
		CASE_ADD_CALL(MPI_Type_size_x)
		CASE_ADD_CALL(MPI_Type_struct)
		CASE_ADD_CALL(MPI_Type_ub)
		CASE_ADD_CALL(MPI_Type_vector)
		CASE_ADD_CALL(MPI_Unpack)
		CASE_ADD_CALL(MPI_Unpack_external)
		CASE_ADD_CALL(MPI_Unpublish_name)

		// only use mpi_wait
		ADD_SPECIAL_CASE(MPI_Waitall)
		ADD_SPECIAL_CASE(MPI_Waitany)
		ADD_SPECIAL_CASE(MPI_Waitsome)
		CASE_ADD_CALL(MPI_Wait)

		CASE_IGNORE(MPI_Win_allocate)
		CASE_IGNORE(MPI_Win_allocate_shared)
		CASE_IGNORE(MPI_Win_attach)
		CASE_IGNORE(MPI_Win_call_errhandler)
		CASE_IGNORE(MPI_Win_complete)
		CASE_IGNORE(MPI_Win_create)
		CASE_IGNORE(MPI_Win_create_dynamic)
		CASE_IGNORE(MPI_Win_create_errhandler)
		CASE_IGNORE(MPI_Win_create_keyval)
		CASE_IGNORE(MPI_Win_delete_attr)
		CASE_IGNORE(MPI_Win_detach)
		CASE_IGNORE(MPI_Win_fence)
		CASE_IGNORE(MPI_Win_flush)
		CASE_IGNORE(MPI_Win_flush_all)
		CASE_IGNORE(MPI_Win_flush_local)
		CASE_IGNORE(MPI_Win_flush_local_all)
		CASE_IGNORE(MPI_Win_free)
		CASE_IGNORE(MPI_Win_free_keyval)
		CASE_IGNORE(MPI_Win_get_attr)
		CASE_IGNORE(MPI_Win_get_errhandler)
		CASE_IGNORE(MPI_Win_get_group)
		CASE_IGNORE(MPI_Win_get_info)
		CASE_IGNORE(MPI_Win_get_name)
		CASE_IGNORE(MPI_Win_lock)
		CASE_IGNORE(MPI_Win_lock_all)
		CASE_IGNORE(MPI_Win_post)
		CASE_IGNORE(MPI_Win_set_attr)
		CASE_IGNORE(MPI_Win_set_errhandler)
		CASE_IGNORE(MPI_Win_set_info)
		CASE_IGNORE(MPI_Win_set_name)
		CASE_IGNORE(MPI_Win_shared_query)
		CASE_IGNORE(MPI_Win_start)
		CASE_IGNORE(MPI_Win_sync)
		CASE_IGNORE(MPI_Win_test)
		CASE_IGNORE(MPI_Win_unlock)
		CASE_IGNORE(MPI_Win_unlock_all)
		CASE_IGNORE(MPI_Win_wait)
		CASE_ADD_CALL(MPI_Wtick)
		CASE_ADD_CALL(MPI_Wtime)
		CASE_ADD_CALL(MPIX_Comm_agree)
		CASE_ADD_CALL(MPIX_Comm_failure_ack)
		CASE_ADD_CALL(MPIX_Comm_failure_get_acked)
		CASE_ADD_CALL(MPIX_Comm_revoke)
		CASE_ADD_CALL(MPIX_Comm_shrink)

    default:
        cout << "ERROR: 'event_enter' callback did not capture event: " << id << endl;
    }

#undef ADD_CALL
    return OTF2_CALLBACK_SUCCESS;
}

// Record end time and trigger the call
#define CASE_READY(call_id, ...) case ID_##call_id : { \
 auto* call = callqueue.find_latest(ID_##call_id); \
 MpiCall::assert_call(call, "Lookup for " #call_id " in 'event_leave' returned NULL"); \
 if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()){ \
  EVENT_PRINT("LEAVE " << call->ToString() << " time: " << time); \
 } \
 call->end_time = time; \
 __VA_ARGS__; \
 callqueue.CallReady(call); \
 break; \
}


// Record end time and do not trigger the call. This happens when
// there is not enough information yet available in the callback.
#define CASE_NOT_READY(call_id) case ID_##call_id : { \
  auto* call = callqueue.find_latest(ID_##call_id); \
  MpiCall::assert_call(call, "Lookup for " #call_id " in 'event_leave' returned NULL"); \
  if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()) \
    EVENT_PRINT("LEAVE " << call->ToString() << " time: " << time); \
  call->end_time = time; \
  break; \
}

OTF2_CallbackCode event_leave(
  OTF2_LocationRef    location,
  OTF2_TimeStamp      time,
  uint64_t            eventPosition,
  void*               userData,
  OTF2_AttributeList* attributes,
  OTF2_RegionRef      region )
{
  auto app = (OTF2TraceReplayApp*)userData;
  CallQueue& callqueue = app->GetCallQueue();

  auto iter = app->otf2_regions.find(region);
  if (iter == app->otf2_regions.end()){
    if (app->PrintUnknownCallback()) {
      std::cout << "unknown OTF2 region " << region << std::endl;
    }
    return OTF2_CALLBACK_SUCCESS;
  }

  MPI_CALL_ID id = iter->second;

  switch (id) {
  CASE_READY(MPI_Abort)
  CASE_READY(MPI_Accumulate)
  CASE_READY(MPI_Add_error_class)
  CASE_READY(MPI_Add_error_code)
  CASE_READY(MPI_Add_error_string)
  CASE_READY(MPI_Address)
  CASE_READY(MPI_Aint_add)
  CASE_READY(MPI_Aint_diff)
  CASE_READY(MPI_Allgather)
  CASE_READY(MPI_Allgatherv)
  CASE_READY(MPI_Alloc_mem)
  CASE_READY(MPI_Allreduce)
  CASE_READY(MPI_Alltoall)
  CASE_READY(MPI_Alltoallv)
  CASE_READY(MPI_Alltoallw)
  CASE_READY(MPI_Attr_delete)
  CASE_READY(MPI_Attr_get)
  CASE_READY(MPI_Attr_put)
  CASE_READY(MPI_Barrier)
  CASE_READY(MPI_Bcast)
  CASE_READY(MPI_Bsend)
  CASE_READY(MPI_Bsend_init)
  CASE_READY(MPI_Buffer_attach)
  CASE_READY(MPI_Buffer_detach)
  CASE_READY(MPI_Cancel)
  CASE_READY(MPI_Cart_coords)
  CASE_READY(MPI_Cart_create)
  CASE_READY(MPI_Cart_get)
  CASE_READY(MPI_Cart_map)
  CASE_READY(MPI_Cart_rank)
  CASE_READY(MPI_Cart_shift)
  CASE_READY(MPI_Cart_sub)
  CASE_READY(MPI_Cartdim_get)
  CASE_READY(MPI_Close_port)
  CASE_READY(MPI_Comm_accept)
  CASE_READY(MPI_Comm_call_errhandler)
  CASE_READY(MPI_Comm_compare)
  CASE_READY(MPI_Comm_connect)
  CASE_READY(MPI_Comm_create)
  CASE_READY(MPI_Comm_create_errhandler)
  CASE_READY(MPI_Comm_create_group)
  CASE_READY(MPI_Comm_create_keyval)
  CASE_READY(MPI_Comm_delete_attr)
  CASE_READY(MPI_Comm_disconnect)
  CASE_READY(MPI_Comm_dup)
  CASE_READY(MPI_Comm_dup_with_info)
  CASE_READY(MPI_Comm_free)
  CASE_READY(MPI_Comm_free_keyval)
  CASE_READY(MPI_Comm_get_attr)
  CASE_READY(MPI_Comm_get_errhandler)
  CASE_READY(MPI_Comm_get_info)
  CASE_READY(MPI_Comm_get_name)
  CASE_READY(MPI_Comm_get_parent)
  CASE_READY(MPI_Comm_group)
  CASE_READY(MPI_Comm_idup)
  CASE_READY(MPI_Comm_join)
  CASE_IGNORE(MPI_Comm_rank)
  CASE_READY(MPI_Comm_remote_group)
  CASE_READY(MPI_Comm_remote_size)
  CASE_READY(MPI_Comm_set_attr)
  CASE_READY(MPI_Comm_set_errhandler)
  CASE_READY(MPI_Comm_set_info)
  CASE_READY(MPI_Comm_set_name)
  CASE_IGNORE(MPI_Comm_size)
  CASE_IGNORE(MPI_Comm_spawn)
  CASE_IGNORE(MPI_Comm_spawn_multiple)
  CASE_IGNORE(MPI_Comm_split)
  CASE_IGNORE(MPI_Comm_split_type)
  CASE_IGNORE(MPI_Comm_test_inter)
  CASE_READY(MPI_Compare_and_swap)
  CASE_READY(MPI_Dims_create)
  CASE_READY(MPI_Dist_graph_create)
  CASE_READY(MPI_Dist_graph_create_adjacent)
  CASE_READY(MPI_Dist_graph_neighbors)
  CASE_READY(MPI_Dist_graph_neighbors_count)
  CASE_READY(MPI_Errhandler_create)
  CASE_READY(MPI_Errhandler_free)
  CASE_READY(MPI_Errhandler_get)
  CASE_READY(MPI_Errhandler_set)
  CASE_READY(MPI_Error_class)
  CASE_READY(MPI_Error_string)
  CASE_READY(MPI_Exscan)
  CASE_READY(MPI_Fetch_and_op)
  CASE_READY(MPI_File_c2f)
  CASE_READY(MPI_File_call_errhandler)
  CASE_READY(MPI_File_close)
  CASE_READY(MPI_File_create_errhandler)
  CASE_READY(MPI_File_delete)
  CASE_READY(MPI_File_f2c)
  CASE_READY(MPI_File_get_amode)
  CASE_READY(MPI_File_get_atomicity)
  CASE_READY(MPI_File_get_byte_offset)
  CASE_READY(MPI_File_get_errhandler)
  CASE_READY(MPI_File_get_group)
  CASE_READY(MPI_File_get_info)
  CASE_READY(MPI_File_get_position)
  CASE_READY(MPI_File_get_position_shared)
  CASE_READY(MPI_File_get_size)
  CASE_READY(MPI_File_get_type_extent)
  CASE_READY(MPI_File_get_view)
  CASE_READY(MPI_File_iread)
  CASE_READY(MPI_File_iread_all)
  CASE_READY(MPI_File_iread_at)
  CASE_READY(MPI_File_iread_at_all)
  CASE_READY(MPI_File_iread_shared)
  CASE_READY(MPI_File_iwrite)
  CASE_READY(MPI_File_iwrite_all)
  CASE_READY(MPI_File_iwrite_at)
  CASE_READY(MPI_File_iwrite_at_all)
  CASE_READY(MPI_File_iwrite_shared)
  CASE_READY(MPI_File_open)
  CASE_READY(MPI_File_preallocate)
  CASE_READY(MPI_File_read)
  CASE_READY(MPI_File_read_all)
  CASE_READY(MPI_File_read_all_begin)
  CASE_READY(MPI_File_read_all_end)
  CASE_READY(MPI_File_read_at)
  CASE_READY(MPI_File_read_at_all)
  CASE_READY(MPI_File_read_at_all_begin)
  CASE_READY(MPI_File_read_at_all_end)
  CASE_READY(MPI_File_read_ordered)
  CASE_READY(MPI_File_read_ordered_begin)
  CASE_READY(MPI_File_read_ordered_end)
  CASE_READY(MPI_File_read_shared)
  CASE_READY(MPI_File_seek)
  CASE_READY(MPI_File_seek_shared)
  CASE_READY(MPI_File_set_atomicity)
  CASE_READY(MPI_File_set_errhandler)
  CASE_READY(MPI_File_set_info)
  CASE_READY(MPI_File_set_size)
  CASE_READY(MPI_File_set_view)
  CASE_READY(MPI_File_sync)
  CASE_READY(MPI_File_write)
  CASE_READY(MPI_File_write_all)
  CASE_READY(MPI_File_write_all_begin)
  CASE_READY(MPI_File_write_all_end)
  CASE_READY(MPI_File_write_at)
  CASE_READY(MPI_File_write_at_all)
  CASE_READY(MPI_File_write_at_all_begin)
  CASE_READY(MPI_File_write_at_all_end)
  CASE_READY(MPI_File_write_ordered)
  CASE_READY(MPI_File_write_ordered_begin)
  CASE_READY(MPI_File_write_ordered_end)
  CASE_READY(MPI_File_write_shared)
  CASE_READY(MPI_Finalize, call->on_trigger = [=] () {call->app->GetMpi()->finalize();})
  CASE_IGNORE(MPI_Finalized)
  CASE_READY(MPI_Free_mem)
  CASE_READY(MPI_Gather)
  CASE_READY(MPI_Gatherv)
  CASE_READY(MPI_Get)
  CASE_READY(MPI_Get_accumulate)
  CASE_READY(MPI_Get_address)
  CASE_READY(MPI_Get_count)
  CASE_READY(MPI_Get_elements)
  CASE_READY(MPI_Get_elements_x)
  CASE_READY(MPI_Get_library_version)
  CASE_READY(MPI_Get_processor_name)
  CASE_READY(MPI_Get_version)
  CASE_READY(MPI_Graph_create)
  CASE_READY(MPI_Graph_get)
  CASE_READY(MPI_Graph_map)
  CASE_READY(MPI_Graph_neighbors)
  CASE_READY(MPI_Graph_neighbors_count)
  CASE_READY(MPI_Graphdims_get)
  CASE_READY(MPI_Grequest_complete)
  CASE_READY(MPI_Grequest_start)
  CASE_READY(MPI_Group_compare)
  CASE_READY(MPI_Group_difference)
  CASE_READY(MPI_Group_excl)
  CASE_READY(MPI_Group_free)
  CASE_READY(MPI_Group_incl)
  CASE_READY(MPI_Group_intersection)
  CASE_READY(MPI_Group_range_excl)
  CASE_READY(MPI_Group_range_incl)
  CASE_READY(MPI_Group_rank)
  CASE_READY(MPI_Group_size)
  CASE_READY(MPI_Group_translate_ranks)
  CASE_READY(MPI_Group_union)
  CASE_READY(MPI_Iallgather)
  CASE_READY(MPI_Iallgatherv)
  CASE_READY(MPI_Iallreduce)
  CASE_READY(MPI_Ialltoall)
  CASE_READY(MPI_Ialltoallv)
  CASE_READY(MPI_Ialltoallw)
  CASE_READY(MPI_Ibarrier)
  CASE_READY(MPI_Ibcast)
  CASE_READY(MPI_Ibsend)
  CASE_READY(MPI_Iexscan)
  CASE_READY(MPI_Igather)
  CASE_READY(MPI_Igatherv)
  CASE_READY(MPI_Improbe)
  CASE_READY(MPI_Imrecv)
  CASE_READY(MPI_Ineighbor_allgather)
  CASE_READY(MPI_Ineighbor_allgatherv)
  CASE_READY(MPI_Ineighbor_alltoall)
  CASE_READY(MPI_Ineighbor_alltoallv)
  CASE_READY(MPI_Ineighbor_alltoallw)
  CASE_READY(MPI_Info_create)
  CASE_READY(MPI_Info_delete)
  CASE_READY(MPI_Info_dup)
  CASE_READY(MPI_Info_free)
  CASE_READY(MPI_Info_get)
  CASE_READY(MPI_Info_get_nkeys)
  CASE_READY(MPI_Info_get_nthkey)
  CASE_READY(MPI_Info_get_valuelen)
  CASE_READY(MPI_Info_set)
  CASE_READY(MPI_Init, call->on_trigger = [=] () { }; ) 
  //call->app->GetMpi()->init(nullptr, nullptr);};) - this gets invoked automatically
  CASE_READY(MPI_Init_thread)
  CASE_IGNORE(MPI_Initialized)
  CASE_READY(MPI_Intercomm_create)
  CASE_READY(MPI_Intercomm_merge)
  CASE_READY(MPI_Iprobe)
  CASE_NOT_READY(MPI_Irecv)
  CASE_READY(MPI_Ireduce)
  CASE_READY(MPI_Ireduce_scatter)
  CASE_READY(MPI_Ireduce_scatter_block)
  CASE_READY(MPI_Irsend)
  CASE_READY(MPI_Is_thread_main)
  CASE_READY(MPI_Iscan)
  CASE_READY(MPI_Iscatter)
  CASE_READY(MPI_Iscatterv)
  CASE_READY(MPI_Isend)
  CASE_READY(MPI_Issend)
  CASE_READY(MPI_Keyval_create)
  CASE_READY(MPI_Keyval_free)
  CASE_READY(MPI_Lookup_name)
  CASE_READY(MPI_Mprobe)
  CASE_READY(MPI_Mrecv)
  CASE_READY(MPI_Neighbor_allgather)
  CASE_READY(MPI_Neighbor_allgatherv)
  CASE_READY(MPI_Neighbor_alltoall)
  CASE_READY(MPI_Neighbor_alltoallv)
  CASE_READY(MPI_Neighbor_alltoallw)
  CASE_READY(MPI_Op_commute)
  CASE_READY(MPI_Op_create)
  CASE_READY(MPI_Op_free)
  CASE_READY(MPI_Open_port)
  CASE_READY(MPI_Pack)
  CASE_READY(MPI_Pack_external)
  CASE_READY(MPI_Pack_external_size)
  CASE_READY(MPI_Pack_size)
  CASE_READY(MPI_Pcontrol)
  CASE_READY(MPI_Probe)
  CASE_READY(MPI_Publish_name)
  CASE_READY(MPI_Put)
  CASE_READY(MPI_Query_thread)
  CASE_READY(MPI_Raccumulate)
  CASE_READY(MPI_Recv)
  CASE_READY(MPI_Recv_init)
  CASE_READY(MPI_Reduce)
  CASE_READY(MPI_Reduce_local)
  CASE_READY(MPI_Reduce_scatter)
  CASE_READY(MPI_Reduce_scatter_block)
  CASE_READY(MPI_Register_datarep)
  CASE_READY(MPI_Request_free)
  CASE_READY(MPI_Request_get_status)
  CASE_READY(MPI_Rget)
  CASE_READY(MPI_Rget_accumulate)
  CASE_READY(MPI_Rput)
  CASE_READY(MPI_Rsend)
  CASE_READY(MPI_Rsend_init)
  CASE_READY(MPI_Scan)
  CASE_READY(MPI_Scatter)
  CASE_READY(MPI_Scatterv)
  CASE_READY(MPI_Send)
  CASE_READY(MPI_Send_init)
  CASE_READY(MPI_Sendrecv)
  CASE_READY(MPI_Sendrecv_replace)
  CASE_READY(MPI_Ssend)
  CASE_READY(MPI_Ssend_init)
  CASE_READY(MPI_Start)
  CASE_READY(MPI_Startall)
  CASE_READY(MPI_Status_set_cancelled)
  CASE_READY(MPI_Status_set_elements)
  CASE_READY(MPI_Status_set_elements_x)
  CASE_READY(MPI_T_category_changed)
  CASE_READY(MPI_T_category_get_categories)
  CASE_READY(MPI_T_category_get_cvars)
  CASE_READY(MPI_T_category_get_info)
  CASE_READY(MPI_T_category_get_num)
  CASE_READY(MPI_T_category_get_pvars)
  CASE_READY(MPI_T_cvar_get_info)
  CASE_READY(MPI_T_cvar_get_num)
  CASE_READY(MPI_T_cvar_handle_alloc)
  CASE_READY(MPI_T_cvar_handle_free)
  CASE_READY(MPI_T_cvar_read)
  CASE_READY(MPI_T_cvar_write)
  CASE_READY(MPI_T_enum_get_info)
  CASE_READY(MPI_T_enum_get_item)
  CASE_READY(MPI_T_finalize)
  CASE_READY(MPI_T_init_thread)
  CASE_READY(MPI_T_pvar_get_info)
  CASE_READY(MPI_T_pvar_get_num)
  CASE_READY(MPI_T_pvar_handle_alloc)
  CASE_READY(MPI_T_pvar_handle_free)
  CASE_READY(MPI_T_pvar_read)
  CASE_READY(MPI_T_pvar_readreset)
  CASE_READY(MPI_T_pvar_reset)
  CASE_READY(MPI_T_pvar_session_create)
  CASE_READY(MPI_T_pvar_session_free)
  CASE_READY(MPI_T_pvar_start)
  CASE_READY(MPI_T_pvar_stop)
  CASE_READY(MPI_T_pvar_write)
  CASE_READY(MPI_Test)
  CASE_READY(MPI_Test_cancelled)
  CASE_READY(MPI_Testall)
  CASE_READY(MPI_Testany)
  CASE_READY(MPI_Testsome)
  CASE_READY(MPI_Topo_test)
  CASE_READY(MPI_Type_commit)
  CASE_READY(MPI_Type_contiguous)
  CASE_READY(MPI_Type_create_darray)
  CASE_READY(MPI_Type_create_hindexed)
  CASE_READY(MPI_Type_create_hindexed_block)
  CASE_READY(MPI_Type_create_hvector)
  CASE_READY(MPI_Type_create_indexed_block)
  CASE_READY(MPI_Type_create_keyval)
  CASE_READY(MPI_Type_create_resized)
  CASE_READY(MPI_Type_create_struct)
  CASE_READY(MPI_Type_create_subarray)
  CASE_READY(MPI_Type_delete_attr)
  CASE_READY(MPI_Type_dup)
  CASE_READY(MPI_Type_extent)
  CASE_READY(MPI_Type_free)
  CASE_READY(MPI_Type_free_keyval)
  CASE_READY(MPI_Type_get_attr)
  CASE_READY(MPI_Type_get_contents)
  CASE_READY(MPI_Type_get_envelope)
  CASE_READY(MPI_Type_get_extent)
  CASE_READY(MPI_Type_get_extent_x)
  CASE_READY(MPI_Type_get_name)
  CASE_READY(MPI_Type_get_true_extent)
  CASE_READY(MPI_Type_get_true_extent_x)
  CASE_READY(MPI_Type_hindexed)
  CASE_READY(MPI_Type_hvector)
  CASE_READY(MPI_Type_indexed)
  CASE_READY(MPI_Type_lb)
  CASE_READY(MPI_Type_match_size)
  CASE_READY(MPI_Type_set_attr)
  CASE_READY(MPI_Type_set_name)
  CASE_READY(MPI_Type_size)
  CASE_READY(MPI_Type_size_x)
  CASE_READY(MPI_Type_struct)
  CASE_READY(MPI_Type_ub)
  CASE_READY(MPI_Type_vector)
  CASE_READY(MPI_Unpack)
  CASE_READY(MPI_Unpack_external)
  CASE_READY(MPI_Unpublish_name)

  // MPI_Wait is used for all waits
  ADD_SPECIAL_CASE(MPI_Waitall)
  ADD_SPECIAL_CASE(MPI_Waitany)
  ADD_SPECIAL_CASE(MPI_Waitsome)
  CASE_READY(MPI_Wait)

  CASE_IGNORE(MPI_Win_allocate)
  CASE_IGNORE(MPI_Win_allocate_shared)
  CASE_IGNORE(MPI_Win_attach)
  CASE_IGNORE(MPI_Win_call_errhandler)
  CASE_IGNORE(MPI_Win_complete)
  CASE_IGNORE(MPI_Win_create)
  CASE_IGNORE(MPI_Win_create_dynamic)
  CASE_IGNORE(MPI_Win_create_errhandler)
  CASE_IGNORE(MPI_Win_create_keyval)
  CASE_IGNORE(MPI_Win_delete_attr)
  CASE_IGNORE(MPI_Win_detach)
  CASE_IGNORE(MPI_Win_fence)
  CASE_IGNORE(MPI_Win_flush)
  CASE_IGNORE(MPI_Win_flush_all)
  CASE_IGNORE(MPI_Win_flush_local)
  CASE_IGNORE(MPI_Win_flush_local_all)
  CASE_IGNORE(MPI_Win_free)
  CASE_IGNORE(MPI_Win_free_keyval)
  CASE_IGNORE(MPI_Win_get_attr)
  CASE_IGNORE(MPI_Win_get_errhandler)
  CASE_IGNORE(MPI_Win_get_group)
  CASE_IGNORE(MPI_Win_get_info)
  CASE_IGNORE(MPI_Win_get_name)
  CASE_IGNORE(MPI_Win_lock)
  CASE_IGNORE(MPI_Win_lock_all)
  CASE_IGNORE(MPI_Win_post)
  CASE_IGNORE(MPI_Win_set_attr)
  CASE_IGNORE(MPI_Win_set_errhandler)
  CASE_IGNORE(MPI_Win_set_info)
  CASE_IGNORE(MPI_Win_set_name)
  CASE_IGNORE(MPI_Win_shared_query)
  CASE_IGNORE(MPI_Win_start)
  CASE_IGNORE(MPI_Win_sync)
  CASE_IGNORE(MPI_Win_test)
  CASE_IGNORE(MPI_Win_unlock)
  CASE_IGNORE(MPI_Win_unlock_all)
  CASE_IGNORE(MPI_Win_wait)
  CASE_READY(MPI_Wtick)
  CASE_READY(MPI_Wtime)
  CASE_READY(MPIX_Comm_agree)
  CASE_READY(MPIX_Comm_failure_ack)
  CASE_READY(MPIX_Comm_failure_get_acked)
  CASE_READY(MPIX_Comm_revoke)
  CASE_READY(MPIX_Comm_shrink)

  default:
      cout << "ERROR: 'event_leave' callback did not capture event: " << id << endl;
  }

#undef CASE_READY
#undef CASE_NOT_READY
  return OTF2_CALLBACK_SUCCESS;
}

#undef CASE_IGNORE
