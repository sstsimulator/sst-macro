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
    #define EVENT_PRINT(...) cerr << "EVT (#" << setw(2) << ((OTF2TraceReplayApp*)userData)->rank << "): " __VA_ARGS__ << endl;
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
  app->otf2_string_table.push_back(str);
  MPI_CALL_ID id = MPI_call_to_id.get(str);
  if (id != ID_NULL){
    app->otf2_mpi_call_map[self] = id;
  }
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
  DEF_PRINT("LOCATION\n");
  return OTF2_CALLBACK_SUCCESS;
}

// probably not needed
OTF2_CallbackCode def_location_group(
  void*                  userData,
  OTF2_LocationGroupRef  self,
  OTF2_StringRef         name,
  OTF2_LocationGroupType locationGroupType,
  OTF2_SystemTreeNodeRef systemTreeParent )
{
  DEF_PRINT("LOCATION GROUP\n");
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
  app->otf2_regions.push_back({
    name, canonicalName, description, regionRole, paradigm, sourceFile});
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
  OTF2_GroupRef   self,
  OTF2_StringRef  name,
  OTF2_GroupType  groupType,
  OTF2_Paradigm   paradigm,
  OTF2_GroupFlag  groupFlags,
  uint32_t        numberOfMembers,
  const uint64_t* members )
{
  auto app = (OTF2TraceReplayApp*)userData;
  app->otf2_groups.push_back({
      name, groupType, paradigm, groupFlags
  });

  DEF_PRINT("GROUP\n");
  return OTF2_CALLBACK_SUCCESS;
}

OTF2_CallbackCode def_comm(
  void*          userData,
  OTF2_CommRef   self,
  OTF2_StringRef name,
  OTF2_GroupRef  group,
  OTF2_CommRef   parent )
{
  auto app = (OTF2TraceReplayApp*)userData;
  app->otf2_comms.push_back({name, group, parent});

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

/******************************************************************************
 * Event callbacks
 *
 * OTF2 event reader will use these as callbacks when streaming through a trace.
 */

// puts call into wait when wait does not have one, otherwise it will make a new wait
// Fundamental assumption: requests are always resolved inside MPI_Waits. This means
// an event_enter must have put one on the stack before this function is called.
void add_wait(OTF2TraceReplayApp* app, CallQueue& queue, MPI_Request requestID) {

	auto wait_event = [=]() {
		MPI_Request req = requestID;
		app->GetMpi()->wait(&req, MPI_STATUS_IGNORE);
	};

	MpiCall* wait_call = queue.PeekBack();
  if (wait_call == nullptr){
    spkt_abort_printf("expected MPI_Wait at the back of the call queue");
  }

	if (wait_call->on_trigger == nullptr) {
    // this is the first wait request
    // this could be part of a waitall, waitany, wait, or waitsome
		wait_call->on_trigger = wait_event;
  } else {
    MpiCall* prev_wait = wait_call;
    // for trace replay, we have to breakdown MPI_Waitall into
    // a sequence of individual MPI_Wait calls
    // this is a 2nd or later wait request in a waitall (or waitsome)
    MpiCall* new_call = new MpiCall(prev_wait->start_time,app,
                                    prev_wait->id,prev_wait->name); //transfer start time
    // the previous call should not advance any time when executing
    prev_wait->end_time = prev_wait->start_time;
    new_call->on_trigger = wait_event;
		new_call->id = ID_MPI_Wait;

    // for trace replay, we have to breakdown MPI_Waitall into
    // a sequence of individual MPI_Wait calls - add the next wait
		app->GetCallQueue().AddCall(new_call);
    // because of subtletlies with event enter/leave, we should only need
    // one wait call pending at a time - the previous wait can be declared ready
    app->GetCallQueue().CallReady(prev_wait);
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

    call->on_trigger = [=]() { call->app->GetMpi()->send(nullptr, msgLength, MPI_BYTE, receiver, msgTag, communicator); };

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
    MpiCall* call = app->GetCallQueue().PeekBack();
    MpiCall::assert_call(call, "Lookup for MPI_Send in 'event_mpi_isend' returned NULL");

    app->GetCallQueue().AddRequest(requestID, call);
    call->on_trigger = [=]() {
      MPI_Request req = requestID;
      call->app->GetMpi()->isend(nullptr, msgLength, MPI_BYTE, receiver, msgTag,
                                 communicator, &req);
    };

    if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()) EVENT_PRINT("ISEND count" << msgLength << " tag " << msgTag << " id " << requestID << " comm " << communicator << " dest " << receiver);
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
    MpiCall* call = app->GetCallQueue().PeekBack();
    MpiCall::assert_call(call, "Lookup for MPI_Irecv in 'event_mpi_irecv_request' returned NULL");

    app->GetCallQueue().AddRequest(requestID, call);

    if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()) EVENT_PRINT("IRECV REQUEST id: " << requestID );
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

    call->on_trigger = [=]() {app->GetMpi()->recv(nullptr, msgLength, MPI_BYTE, sender, msgTag, communicator, MPI_STATUS_IGNORE);};

    if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()) EVENT_PRINT("RECV count: " << msgLength << " source: " << sender << " tag: " << msgTag);
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
#define HANDLE_CASE(op, ...) case op : { \
            auto call = app->GetCallQueue().PeekBack(); \
            __VA_ARGS__; \
            } break;

    switch (collectiveOp) {
		HANDLE_CASE(OTF2_COLLECTIVE_OP_BARRIER, call->on_trigger = [=]() {call->app->GetMpi()->barrier(comm);})
		HANDLE_CASE(OTF2_COLLECTIVE_OP_BCAST, call->on_trigger = [=]() {call->app->GetMpi()->bcast(sizeSent, MPI_BYTE, root, comm);})
		HANDLE_CASE(OTF2_COLLECTIVE_OP_GATHER, call->on_trigger = [=]() {call->app->GetMpi()->gather(sizeSent, MPI_BYTE, sizeReceived, MPI_BYTE, root, comm);})
//		HANDLE_CASE(OTF2_COLLECTIVE_OP_GATHERV, call->on_trigger = [=]() {call->app->get_mpi()->gatherv(sizeSent, MPI_BYTE, nullptr, MPI_BYTE, root, comm);})
		HANDLE_CASE(OTF2_COLLECTIVE_OP_SCATTER, call->on_trigger = [=]() {call->app->GetMpi()->scatter(sizeSent, MPI_BYTE, sizeReceived, MPI_BYTE, root, comm);})
//		HANDLE_CASE(OTF2_COLLECTIVE_OP_SCATTERV, call->on_trigger = [=]() {call->app->get_mpi()->scatterv(nullptr, nullptr, nullptr, MPI_BYTE, nullptr, sizeReceived, root, comm);})
		HANDLE_CASE(OTF2_COLLECTIVE_OP_ALLGATHER, call->on_trigger = [=]() {call->app->GetMpi()->allgather(sizeSent, MPI_BYTE, sizeReceived, MPI_BYTE, comm);}) //allgather(int count, MPI_Datatype type, MPI_Comm comm)
//		HANDLE_CASE(OTF2_COLLECTIVE_OP_ALLGATHERV, call->on_trigger = [=]() {call->app->get_mpi()->barrier();})
		HANDLE_CASE(OTF2_COLLECTIVE_OP_ALLTOALL, call->on_trigger = [=]() {call->app->GetMpi()->alltoall(sizeSent, MPI_BYTE, sizeReceived, MPI_BYTE, comm);}) // test
//		HANDLE_CASE(OTF2_COLLECTIVE_OP_ALLTOALLV, call->on_trigger = [=]() {call->app->get_mpi()->barrier();})
//		HANDLE_CASE(OTF2_COLLECTIVE_OP_ALLTOALLW, call->on_trigger = [=]() {call->app->get_mpi()->barrier();})
		HANDLE_CASE(OTF2_COLLECTIVE_OP_ALLREDUCE, call->on_trigger = [=]() {call->app->GetMpi()->allreduce(sizeSent, MPI_BYTE, OTF2_OP, comm);})           //allreduce(int count, MPI_Datatype type, MPI_Op op, MPI_Comm comm);
		HANDLE_CASE(OTF2_COLLECTIVE_OP_REDUCE, call->on_trigger = [=]() {call->app->GetMpi()->reduce(sizeSent, MPI_BYTE, OTF2_OP, root, comm);})           //reduce(int count, MPI_Datatype type, MPI_Op op, int root, MPI_Comm comm);
		HANDLE_CASE(OTF2_COLLECTIVE_OP_REDUCE_SCATTER, call->on_trigger = [=]() {call->app->GetMpi()->reduce_scatter_block(sizeReceived, MPI_BYTE, OTF2_OP, comm);}) //reduce_scatter_block(int recvcnt, MPI_Datatype type, MPI_Op op, MPI_Comm comm);
		HANDLE_CASE(OTF2_COLLECTIVE_OP_SCAN, call->on_trigger = [=]() {call->app->GetMpi()->scan(sizeSent, MPI_BYTE, OTF2_OP, comm);})                     // scan(int count, MPI_Datatype type, MPI_Op op, MPI_Comm comm);

    default:
        cout << "ERROR: Collective not handled; " << (int)collectiveOp << endl;
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
#define CASE_IGNORE(call_id) case call_id : \
	break;
#define CASE_ADD_CALL(call_id) case call_id: \
    { \
  const char* name = #call_id; \
  MpiCall* call = new MpiCall(time,app,call_id,name + 3/*strip ID_*/); \
  app->GetCallQueue().AddCall(call); \
  if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()) \
    EVENT_PRINT("ENTER " << call->ToString() << " time: " << time); \
  } \
  break;
#define ADD_SPECIAL_CASE(call_id) case call_id:
#define END_SPECIAL_CASE break;

OTF2_CallbackCode event_enter(
    OTF2_LocationRef    location,
    OTF2_TimeStamp      time,
    uint64_t            eventPosition,
    void*               userData,
    OTF2_AttributeList* attributes,
    OTF2_RegionRef      region ) {

    auto app = (OTF2TraceReplayApp*)userData;

    auto iter = app->otf2_mpi_call_map.find(app->otf2_regions[region].name);
    if (iter == app->otf2_mpi_call_map.end()){
      if (app->PrintUnknownCallback()) {
        std::cout << "unknown OTF2 region name \""
                  << app->otf2_string_table[app->otf2_regions[region].name].c_str() << "\"" << std::endl;
      }
      return OTF2_CALLBACK_SUCCESS;
    }
    MPI_CALL_ID id = iter->second;

    switch (id) {
		CASE_ADD_CALL(ID_MPI_Abort)
		CASE_ADD_CALL(ID_MPI_Accumulate)
		CASE_ADD_CALL(ID_MPI_Add_error_class)
		CASE_ADD_CALL(ID_MPI_Add_error_code)
		CASE_ADD_CALL(ID_MPI_Add_error_string)
		CASE_ADD_CALL(ID_MPI_Address)
		CASE_ADD_CALL(ID_MPI_Aint_add)
		CASE_ADD_CALL(ID_MPI_Aint_diff)
		CASE_ADD_CALL(ID_MPI_Allgather)
		CASE_ADD_CALL(ID_MPI_Allgatherv)
		CASE_ADD_CALL(ID_MPI_Alloc_mem)
		CASE_ADD_CALL(ID_MPI_Allreduce)
		CASE_ADD_CALL(ID_MPI_Alltoall)
		CASE_ADD_CALL(ID_MPI_Alltoallv)
		CASE_ADD_CALL(ID_MPI_Alltoallw)
		CASE_ADD_CALL(ID_MPI_Attr_delete)
		CASE_ADD_CALL(ID_MPI_Attr_get)
		CASE_ADD_CALL(ID_MPI_Attr_put)
		CASE_ADD_CALL(ID_MPI_Barrier)
		CASE_ADD_CALL(ID_MPI_Bcast)
		CASE_ADD_CALL(ID_MPI_Bsend)
		CASE_ADD_CALL(ID_MPI_Bsend_init)
		CASE_ADD_CALL(ID_MPI_Buffer_attach)
		CASE_ADD_CALL(ID_MPI_Buffer_detach)
		CASE_ADD_CALL(ID_MPI_Cancel)
		CASE_ADD_CALL(ID_MPI_Cart_coords)
		CASE_ADD_CALL(ID_MPI_Cart_create)
		CASE_ADD_CALL(ID_MPI_Cart_get)
		CASE_ADD_CALL(ID_MPI_Cart_map)
		CASE_ADD_CALL(ID_MPI_Cart_rank)
		CASE_ADD_CALL(ID_MPI_Cart_shift)
		CASE_ADD_CALL(ID_MPI_Cart_sub)
		CASE_ADD_CALL(ID_MPI_Cartdim_get)
		CASE_ADD_CALL(ID_MPI_Close_port)
		CASE_ADD_CALL(ID_MPI_Comm_accept)
		CASE_ADD_CALL(ID_MPI_Comm_call_errhandler)
		CASE_ADD_CALL(ID_MPI_Comm_compare)
		CASE_ADD_CALL(ID_MPI_Comm_connect)
		CASE_ADD_CALL(ID_MPI_Comm_create)
		CASE_ADD_CALL(ID_MPI_Comm_create_errhandler)
		CASE_ADD_CALL(ID_MPI_Comm_create_group)
		CASE_ADD_CALL(ID_MPI_Comm_create_keyval)
		CASE_ADD_CALL(ID_MPI_Comm_delete_attr)
		CASE_ADD_CALL(ID_MPI_Comm_disconnect)
		CASE_ADD_CALL(ID_MPI_Comm_dup)
		CASE_ADD_CALL(ID_MPI_Comm_dup_with_info)
		CASE_ADD_CALL(ID_MPI_Comm_free)
		CASE_ADD_CALL(ID_MPI_Comm_free_keyval)
		CASE_ADD_CALL(ID_MPI_Comm_get_attr)
		CASE_ADD_CALL(ID_MPI_Comm_get_errhandler)
		CASE_ADD_CALL(ID_MPI_Comm_get_info)
		CASE_ADD_CALL(ID_MPI_Comm_get_name)
		CASE_ADD_CALL(ID_MPI_Comm_get_parent)
		CASE_ADD_CALL(ID_MPI_Comm_group)
		CASE_ADD_CALL(ID_MPI_Comm_idup)
		CASE_ADD_CALL(ID_MPI_Comm_join)
		CASE_IGNORE(ID_MPI_Comm_rank)
		CASE_ADD_CALL(ID_MPI_Comm_remote_group)
		CASE_ADD_CALL(ID_MPI_Comm_remote_size)
		CASE_ADD_CALL(ID_MPI_Comm_set_attr)
		CASE_ADD_CALL(ID_MPI_Comm_set_errhandler)
		CASE_ADD_CALL(ID_MPI_Comm_set_info)
		CASE_ADD_CALL(ID_MPI_Comm_set_name)
		CASE_IGNORE(ID_MPI_Comm_size)
		CASE_ADD_CALL(ID_MPI_Comm_spawn)
		CASE_ADD_CALL(ID_MPI_Comm_spawn_multiple)
		CASE_ADD_CALL(ID_MPI_Comm_split)
		CASE_ADD_CALL(ID_MPI_Comm_split_type)
		CASE_ADD_CALL(ID_MPI_Comm_test_inter)
		CASE_ADD_CALL(ID_MPI_Compare_and_swap)
		CASE_ADD_CALL(ID_MPI_Dims_create)
		CASE_ADD_CALL(ID_MPI_Dist_graph_create)
		CASE_ADD_CALL(ID_MPI_Dist_graph_create_adjacent)
		CASE_ADD_CALL(ID_MPI_Dist_graph_neighbors)
		CASE_ADD_CALL(ID_MPI_Dist_graph_neighbors_count)
		CASE_ADD_CALL(ID_MPI_Errhandler_create)
		CASE_ADD_CALL(ID_MPI_Errhandler_free)
		CASE_ADD_CALL(ID_MPI_Errhandler_get)
		CASE_ADD_CALL(ID_MPI_Errhandler_set)
		CASE_ADD_CALL(ID_MPI_Error_class)
		CASE_ADD_CALL(ID_MPI_Error_string)
		CASE_ADD_CALL(ID_MPI_Exscan)
		CASE_ADD_CALL(ID_MPI_Fetch_and_op)
		CASE_ADD_CALL(ID_MPI_File_c2f)
		CASE_ADD_CALL(ID_MPI_File_call_errhandler)
		CASE_ADD_CALL(ID_MPI_File_close)
		CASE_ADD_CALL(ID_MPI_File_create_errhandler)
		CASE_ADD_CALL(ID_MPI_File_delete)
		CASE_ADD_CALL(ID_MPI_File_f2c)
		CASE_ADD_CALL(ID_MPI_File_get_amode)
		CASE_ADD_CALL(ID_MPI_File_get_atomicity)
		CASE_ADD_CALL(ID_MPI_File_get_byte_offset)
		CASE_ADD_CALL(ID_MPI_File_get_errhandler)
		CASE_ADD_CALL(ID_MPI_File_get_group)
		CASE_ADD_CALL(ID_MPI_File_get_info)
		CASE_ADD_CALL(ID_MPI_File_get_position)
		CASE_ADD_CALL(ID_MPI_File_get_position_shared)
		CASE_ADD_CALL(ID_MPI_File_get_size)
		CASE_ADD_CALL(ID_MPI_File_get_type_extent)
		CASE_ADD_CALL(ID_MPI_File_get_view)
		CASE_ADD_CALL(ID_MPI_File_iread)
		CASE_ADD_CALL(ID_MPI_File_iread_all)
		CASE_ADD_CALL(ID_MPI_File_iread_at)
		CASE_ADD_CALL(ID_MPI_File_iread_at_all)
		CASE_ADD_CALL(ID_MPI_File_iread_shared)
		CASE_ADD_CALL(ID_MPI_File_iwrite)
		CASE_ADD_CALL(ID_MPI_File_iwrite_all)
		CASE_ADD_CALL(ID_MPI_File_iwrite_at)
		CASE_ADD_CALL(ID_MPI_File_iwrite_at_all)
		CASE_ADD_CALL(ID_MPI_File_iwrite_shared)
		CASE_ADD_CALL(ID_MPI_File_open)
		CASE_ADD_CALL(ID_MPI_File_preallocate)
		CASE_ADD_CALL(ID_MPI_File_read)
		CASE_ADD_CALL(ID_MPI_File_read_all)
		CASE_ADD_CALL(ID_MPI_File_read_all_begin)
		CASE_ADD_CALL(ID_MPI_File_read_all_end)
		CASE_ADD_CALL(ID_MPI_File_read_at)
		CASE_ADD_CALL(ID_MPI_File_read_at_all)
		CASE_ADD_CALL(ID_MPI_File_read_at_all_begin)
		CASE_ADD_CALL(ID_MPI_File_read_at_all_end)
		CASE_ADD_CALL(ID_MPI_File_read_ordered)
		CASE_ADD_CALL(ID_MPI_File_read_ordered_begin)
		CASE_ADD_CALL(ID_MPI_File_read_ordered_end)
		CASE_ADD_CALL(ID_MPI_File_read_shared)
		CASE_ADD_CALL(ID_MPI_File_seek)
		CASE_ADD_CALL(ID_MPI_File_seek_shared)
		CASE_ADD_CALL(ID_MPI_File_set_atomicity)
		CASE_ADD_CALL(ID_MPI_File_set_errhandler)
		CASE_ADD_CALL(ID_MPI_File_set_info)
		CASE_ADD_CALL(ID_MPI_File_set_size)
		CASE_ADD_CALL(ID_MPI_File_set_view)
		CASE_ADD_CALL(ID_MPI_File_sync)
		CASE_ADD_CALL(ID_MPI_File_write)
		CASE_ADD_CALL(ID_MPI_File_write_all)
		CASE_ADD_CALL(ID_MPI_File_write_all_begin)
		CASE_ADD_CALL(ID_MPI_File_write_all_end)
		CASE_ADD_CALL(ID_MPI_File_write_at)
		CASE_ADD_CALL(ID_MPI_File_write_at_all)
		CASE_ADD_CALL(ID_MPI_File_write_at_all_begin)
		CASE_ADD_CALL(ID_MPI_File_write_at_all_end)
		CASE_ADD_CALL(ID_MPI_File_write_ordered)
		CASE_ADD_CALL(ID_MPI_File_write_ordered_begin)
		CASE_ADD_CALL(ID_MPI_File_write_ordered_end)
		CASE_ADD_CALL(ID_MPI_File_write_shared)
		CASE_ADD_CALL(ID_MPI_Finalize)
		CASE_IGNORE(ID_MPI_Finalized)
		CASE_ADD_CALL(ID_MPI_Free_mem)
		CASE_ADD_CALL(ID_MPI_Gather)
		CASE_ADD_CALL(ID_MPI_Gatherv)
		CASE_ADD_CALL(ID_MPI_Get)
		CASE_ADD_CALL(ID_MPI_Get_accumulate)
		CASE_ADD_CALL(ID_MPI_Get_address)
		CASE_ADD_CALL(ID_MPI_Get_count)
		CASE_ADD_CALL(ID_MPI_Get_elements)
		CASE_ADD_CALL(ID_MPI_Get_elements_x)
		CASE_ADD_CALL(ID_MPI_Get_library_version)
		CASE_ADD_CALL(ID_MPI_Get_processor_name)
		CASE_ADD_CALL(ID_MPI_Get_version)
		CASE_ADD_CALL(ID_MPI_Graph_create)
		CASE_ADD_CALL(ID_MPI_Graph_get)
		CASE_ADD_CALL(ID_MPI_Graph_map)
		CASE_ADD_CALL(ID_MPI_Graph_neighbors)
		CASE_ADD_CALL(ID_MPI_Graph_neighbors_count)
		CASE_ADD_CALL(ID_MPI_Graphdims_get)
		CASE_ADD_CALL(ID_MPI_Grequest_complete)
		CASE_ADD_CALL(ID_MPI_Grequest_start)
		CASE_ADD_CALL(ID_MPI_Group_compare)
		CASE_ADD_CALL(ID_MPI_Group_difference)
		CASE_ADD_CALL(ID_MPI_Group_excl)
		CASE_ADD_CALL(ID_MPI_Group_free)
		CASE_ADD_CALL(ID_MPI_Group_incl)
		CASE_ADD_CALL(ID_MPI_Group_intersection)
		CASE_ADD_CALL(ID_MPI_Group_range_excl)
		CASE_ADD_CALL(ID_MPI_Group_range_incl)
		CASE_ADD_CALL(ID_MPI_Group_rank)
		CASE_ADD_CALL(ID_MPI_Group_size)
		CASE_ADD_CALL(ID_MPI_Group_translate_ranks)
		CASE_ADD_CALL(ID_MPI_Group_union)
		CASE_ADD_CALL(ID_MPI_Iallgather)
		CASE_ADD_CALL(ID_MPI_Iallgatherv)
		CASE_ADD_CALL(ID_MPI_Iallreduce)
		CASE_ADD_CALL(ID_MPI_Ialltoall)
		CASE_ADD_CALL(ID_MPI_Ialltoallv)
		CASE_ADD_CALL(ID_MPI_Ialltoallw)
		CASE_ADD_CALL(ID_MPI_Ibarrier)
		CASE_ADD_CALL(ID_MPI_Ibcast)
		CASE_ADD_CALL(ID_MPI_Ibsend)
		CASE_ADD_CALL(ID_MPI_Iexscan)
		CASE_ADD_CALL(ID_MPI_Igather)
		CASE_ADD_CALL(ID_MPI_Igatherv)
		CASE_ADD_CALL(ID_MPI_Improbe)
		CASE_ADD_CALL(ID_MPI_Imrecv)
		CASE_ADD_CALL(ID_MPI_Ineighbor_allgather)
		CASE_ADD_CALL(ID_MPI_Ineighbor_allgatherv)
		CASE_ADD_CALL(ID_MPI_Ineighbor_alltoall)
		CASE_ADD_CALL(ID_MPI_Ineighbor_alltoallv)
		CASE_ADD_CALL(ID_MPI_Ineighbor_alltoallw)
		CASE_ADD_CALL(ID_MPI_Info_create)
		CASE_ADD_CALL(ID_MPI_Info_delete)
		CASE_ADD_CALL(ID_MPI_Info_dup)
		CASE_ADD_CALL(ID_MPI_Info_free)
		CASE_ADD_CALL(ID_MPI_Info_get)
		CASE_ADD_CALL(ID_MPI_Info_get_nkeys)
		CASE_ADD_CALL(ID_MPI_Info_get_nthkey)
		CASE_ADD_CALL(ID_MPI_Info_get_valuelen)
		CASE_ADD_CALL(ID_MPI_Info_set)
		CASE_ADD_CALL(ID_MPI_Init)
		CASE_ADD_CALL(ID_MPI_Init_thread)
		CASE_IGNORE(ID_MPI_Initialized)
		CASE_ADD_CALL(ID_MPI_Intercomm_create)
		CASE_ADD_CALL(ID_MPI_Intercomm_merge)
		CASE_ADD_CALL(ID_MPI_Iprobe)
		CASE_ADD_CALL(ID_MPI_Irecv)
		CASE_ADD_CALL(ID_MPI_Ireduce)
		CASE_ADD_CALL(ID_MPI_Ireduce_scatter)
		CASE_ADD_CALL(ID_MPI_Ireduce_scatter_block)
		CASE_ADD_CALL(ID_MPI_Irsend)
		CASE_ADD_CALL(ID_MPI_Is_thread_main)
		CASE_ADD_CALL(ID_MPI_Iscan)
		CASE_ADD_CALL(ID_MPI_Iscatter)
		CASE_ADD_CALL(ID_MPI_Iscatterv)
		CASE_ADD_CALL(ID_MPI_Isend)
		CASE_ADD_CALL(ID_MPI_Issend)
		CASE_ADD_CALL(ID_MPI_Keyval_create)
		CASE_ADD_CALL(ID_MPI_Keyval_free)
		CASE_ADD_CALL(ID_MPI_Lookup_name)
		CASE_ADD_CALL(ID_MPI_Mprobe)
		CASE_ADD_CALL(ID_MPI_Mrecv)
		CASE_ADD_CALL(ID_MPI_Neighbor_allgather)
		CASE_ADD_CALL(ID_MPI_Neighbor_allgatherv)
		CASE_ADD_CALL(ID_MPI_Neighbor_alltoall)
		CASE_ADD_CALL(ID_MPI_Neighbor_alltoallv)
		CASE_ADD_CALL(ID_MPI_Neighbor_alltoallw)
		CASE_ADD_CALL(ID_MPI_Op_commute)
		CASE_ADD_CALL(ID_MPI_Op_create)
		CASE_ADD_CALL(ID_MPI_Op_free)
		CASE_ADD_CALL(ID_MPI_Open_port)
		CASE_ADD_CALL(ID_MPI_Pack)
		CASE_ADD_CALL(ID_MPI_Pack_external)
		CASE_ADD_CALL(ID_MPI_Pack_external_size)
		CASE_ADD_CALL(ID_MPI_Pack_size)
		CASE_ADD_CALL(ID_MPI_Pcontrol)
		CASE_ADD_CALL(ID_MPI_Probe)
		CASE_ADD_CALL(ID_MPI_Publish_name)
		CASE_ADD_CALL(ID_MPI_Put)
		CASE_ADD_CALL(ID_MPI_Query_thread)
		CASE_ADD_CALL(ID_MPI_Raccumulate)
		CASE_ADD_CALL(ID_MPI_Recv)
		CASE_ADD_CALL(ID_MPI_Recv_init)
		CASE_ADD_CALL(ID_MPI_Reduce)
		CASE_ADD_CALL(ID_MPI_Reduce_local)
		CASE_ADD_CALL(ID_MPI_Reduce_scatter)
		CASE_ADD_CALL(ID_MPI_Reduce_scatter_block)
		CASE_ADD_CALL(ID_MPI_Register_datarep)
		CASE_ADD_CALL(ID_MPI_Request_free)
		CASE_ADD_CALL(ID_MPI_Request_get_status)
		CASE_ADD_CALL(ID_MPI_Rget)
		CASE_ADD_CALL(ID_MPI_Rget_accumulate)
		CASE_ADD_CALL(ID_MPI_Rput)
		CASE_ADD_CALL(ID_MPI_Rsend)
		CASE_ADD_CALL(ID_MPI_Rsend_init)
		CASE_ADD_CALL(ID_MPI_Scan)
		CASE_ADD_CALL(ID_MPI_Scatter)
		CASE_ADD_CALL(ID_MPI_Scatterv)
		CASE_ADD_CALL(ID_MPI_Send)
		CASE_ADD_CALL(ID_MPI_Send_init)
		CASE_ADD_CALL(ID_MPI_Sendrecv)
		CASE_ADD_CALL(ID_MPI_Sendrecv_replace)
		CASE_ADD_CALL(ID_MPI_Ssend)
		CASE_ADD_CALL(ID_MPI_Ssend_init)
		CASE_ADD_CALL(ID_MPI_Start)
		CASE_ADD_CALL(ID_MPI_Startall)
		CASE_ADD_CALL(ID_MPI_Status_set_cancelled)
		CASE_ADD_CALL(ID_MPI_Status_set_elements)
		CASE_ADD_CALL(ID_MPI_Status_set_elements_x)
		CASE_ADD_CALL(ID_MPI_T_category_changed)
		CASE_ADD_CALL(ID_MPI_T_category_get_categories)
		CASE_ADD_CALL(ID_MPI_T_category_get_cvars)
		CASE_ADD_CALL(ID_MPI_T_category_get_info)
		CASE_ADD_CALL(ID_MPI_T_category_get_num)
		CASE_ADD_CALL(ID_MPI_T_category_get_pvars)
		CASE_ADD_CALL(ID_MPI_T_cvar_get_info)
		CASE_ADD_CALL(ID_MPI_T_cvar_get_num)
		CASE_ADD_CALL(ID_MPI_T_cvar_handle_alloc)
		CASE_ADD_CALL(ID_MPI_T_cvar_handle_free)
		CASE_ADD_CALL(ID_MPI_T_cvar_read)
		CASE_ADD_CALL(ID_MPI_T_cvar_write)
		CASE_ADD_CALL(ID_MPI_T_enum_get_info)
		CASE_ADD_CALL(ID_MPI_T_enum_get_item)
		CASE_ADD_CALL(ID_MPI_T_finalize)
		CASE_ADD_CALL(ID_MPI_T_init_thread)
		CASE_ADD_CALL(ID_MPI_T_pvar_get_info)
		CASE_ADD_CALL(ID_MPI_T_pvar_get_num)
		CASE_ADD_CALL(ID_MPI_T_pvar_handle_alloc)
		CASE_ADD_CALL(ID_MPI_T_pvar_handle_free)
		CASE_ADD_CALL(ID_MPI_T_pvar_read)
		CASE_ADD_CALL(ID_MPI_T_pvar_readreset)
		CASE_ADD_CALL(ID_MPI_T_pvar_reset)
		CASE_ADD_CALL(ID_MPI_T_pvar_session_create)
		CASE_ADD_CALL(ID_MPI_T_pvar_session_free)
		CASE_ADD_CALL(ID_MPI_T_pvar_start)
		CASE_ADD_CALL(ID_MPI_T_pvar_stop)
		CASE_ADD_CALL(ID_MPI_T_pvar_write)
		CASE_ADD_CALL(ID_MPI_Test)
		CASE_ADD_CALL(ID_MPI_Test_cancelled)
		CASE_ADD_CALL(ID_MPI_Testall)
		CASE_ADD_CALL(ID_MPI_Testany)
		CASE_ADD_CALL(ID_MPI_Testsome)
		CASE_ADD_CALL(ID_MPI_Topo_test)
		CASE_ADD_CALL(ID_MPI_Type_commit)
		CASE_ADD_CALL(ID_MPI_Type_contiguous)
		CASE_ADD_CALL(ID_MPI_Type_create_darray)
		CASE_ADD_CALL(ID_MPI_Type_create_hindexed)
		CASE_ADD_CALL(ID_MPI_Type_create_hindexed_block)
		CASE_ADD_CALL(ID_MPI_Type_create_hvector)
		CASE_ADD_CALL(ID_MPI_Type_create_indexed_block)
		CASE_ADD_CALL(ID_MPI_Type_create_keyval)
		CASE_ADD_CALL(ID_MPI_Type_create_resized)
		CASE_ADD_CALL(ID_MPI_Type_create_struct)
		CASE_ADD_CALL(ID_MPI_Type_create_subarray)
		CASE_ADD_CALL(ID_MPI_Type_delete_attr)
		CASE_ADD_CALL(ID_MPI_Type_dup)
		CASE_ADD_CALL(ID_MPI_Type_extent)
		CASE_ADD_CALL(ID_MPI_Type_free)
		CASE_ADD_CALL(ID_MPI_Type_free_keyval)
		CASE_ADD_CALL(ID_MPI_Type_get_attr)
		CASE_ADD_CALL(ID_MPI_Type_get_contents)
		CASE_ADD_CALL(ID_MPI_Type_get_envelope)
		CASE_ADD_CALL(ID_MPI_Type_get_extent)
		CASE_ADD_CALL(ID_MPI_Type_get_extent_x)
		CASE_ADD_CALL(ID_MPI_Type_get_name)
		CASE_ADD_CALL(ID_MPI_Type_get_true_extent)
		CASE_ADD_CALL(ID_MPI_Type_get_true_extent_x)
		CASE_ADD_CALL(ID_MPI_Type_hindexed)
		CASE_ADD_CALL(ID_MPI_Type_hvector)
		CASE_ADD_CALL(ID_MPI_Type_indexed)
		CASE_ADD_CALL(ID_MPI_Type_lb)
		CASE_ADD_CALL(ID_MPI_Type_match_size)
		CASE_ADD_CALL(ID_MPI_Type_set_attr)
		CASE_ADD_CALL(ID_MPI_Type_set_name)
		CASE_ADD_CALL(ID_MPI_Type_size)
		CASE_ADD_CALL(ID_MPI_Type_size_x)
		CASE_ADD_CALL(ID_MPI_Type_struct)
		CASE_ADD_CALL(ID_MPI_Type_ub)
		CASE_ADD_CALL(ID_MPI_Type_vector)
		CASE_ADD_CALL(ID_MPI_Unpack)
		CASE_ADD_CALL(ID_MPI_Unpack_external)
		CASE_ADD_CALL(ID_MPI_Unpublish_name)

		// only use mpi_wait
		ADD_SPECIAL_CASE(ID_MPI_Waitall)
		ADD_SPECIAL_CASE(ID_MPI_Waitany)
		ADD_SPECIAL_CASE(ID_MPI_Waitsome)
		CASE_ADD_CALL(ID_MPI_Wait)

		CASE_IGNORE(ID_MPI_Win_allocate)
		CASE_IGNORE(ID_MPI_Win_allocate_shared)
		CASE_IGNORE(ID_MPI_Win_attach)
		CASE_IGNORE(ID_MPI_Win_call_errhandler)
		CASE_IGNORE(ID_MPI_Win_complete)
		CASE_IGNORE(ID_MPI_Win_create)
		CASE_IGNORE(ID_MPI_Win_create_dynamic)
		CASE_IGNORE(ID_MPI_Win_create_errhandler)
		CASE_IGNORE(ID_MPI_Win_create_keyval)
		CASE_IGNORE(ID_MPI_Win_delete_attr)
		CASE_IGNORE(ID_MPI_Win_detach)
		CASE_IGNORE(ID_MPI_Win_fence)
		CASE_IGNORE(ID_MPI_Win_flush)
		CASE_IGNORE(ID_MPI_Win_flush_all)
		CASE_IGNORE(ID_MPI_Win_flush_local)
		CASE_IGNORE(ID_MPI_Win_flush_local_all)
		CASE_IGNORE(ID_MPI_Win_free)
		CASE_IGNORE(ID_MPI_Win_free_keyval)
		CASE_IGNORE(ID_MPI_Win_get_attr)
		CASE_IGNORE(ID_MPI_Win_get_errhandler)
		CASE_IGNORE(ID_MPI_Win_get_group)
		CASE_IGNORE(ID_MPI_Win_get_info)
		CASE_IGNORE(ID_MPI_Win_get_name)
		CASE_IGNORE(ID_MPI_Win_lock)
		CASE_IGNORE(ID_MPI_Win_lock_all)
		CASE_IGNORE(ID_MPI_Win_post)
		CASE_IGNORE(ID_MPI_Win_set_attr)
		CASE_IGNORE(ID_MPI_Win_set_errhandler)
		CASE_IGNORE(ID_MPI_Win_set_info)
		CASE_IGNORE(ID_MPI_Win_set_name)
		CASE_IGNORE(ID_MPI_Win_shared_query)
		CASE_IGNORE(ID_MPI_Win_start)
		CASE_IGNORE(ID_MPI_Win_sync)
		CASE_IGNORE(ID_MPI_Win_test)
		CASE_IGNORE(ID_MPI_Win_unlock)
		CASE_IGNORE(ID_MPI_Win_unlock_all)
		CASE_IGNORE(ID_MPI_Win_wait)
		CASE_ADD_CALL(ID_MPI_Wtick)
		CASE_ADD_CALL(ID_MPI_Wtime)
		CASE_ADD_CALL(ID_MPIX_Comm_agree)
		CASE_ADD_CALL(ID_MPIX_Comm_failure_ack)
		CASE_ADD_CALL(ID_MPIX_Comm_failure_get_acked)
		CASE_ADD_CALL(ID_MPIX_Comm_revoke)
		CASE_ADD_CALL(ID_MPIX_Comm_shrink)

    default:
        cout << "ERROR: 'event_enter' callback did not capture event: " << id << endl;
    }

#undef ADD_CALL
    return OTF2_CALLBACK_SUCCESS;
}

// Record end time and trigger the call
#define CASE_READY(call_id, ...) case call_id : { \
 auto call = callqueue.find_latest(call_id); \
 MpiCall::assert_call(call, "Lookup for " #call_id " in 'event_leave' returned NULL"); \
 if (((OTF2TraceReplayApp*)userData)->PrintTraceEvents()) \
  EVENT_PRINT("LEAVE " << call->ToString() << " time: " << time); \
 call->end_time = time; \
 __VA_ARGS__; \
 callqueue.CallReady(call); \
 break; \
}


// Record end time and do not trigger the call. This happens when
// there is not enough information yet available in the callback.
#define CASE_NOT_READY(call_id) case call_id : { \
  auto call = callqueue.find_latest(call_id); \
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

  auto iter = app->otf2_mpi_call_map.find(app->otf2_regions[region].name);
  if (iter == app->otf2_mpi_call_map.end()){
    if (app->PrintUnknownCallback()) {
      std::cout << "unknown OTF2 region name \""
                << app->otf2_string_table[app->otf2_regions[region].name].c_str() << "\""
                << std::endl;
    }
    return OTF2_CALLBACK_SUCCESS;
  }

  MPI_CALL_ID id = iter->second;

  switch (id) {
  CASE_READY(ID_MPI_Abort)
  CASE_READY(ID_MPI_Accumulate)
  CASE_READY(ID_MPI_Add_error_class)
  CASE_READY(ID_MPI_Add_error_code)
  CASE_READY(ID_MPI_Add_error_string)
  CASE_READY(ID_MPI_Address)
  CASE_READY(ID_MPI_Aint_add)
  CASE_READY(ID_MPI_Aint_diff)
  CASE_READY(ID_MPI_Allgather)
  CASE_READY(ID_MPI_Allgatherv)
  CASE_READY(ID_MPI_Alloc_mem)
  CASE_READY(ID_MPI_Allreduce)
  CASE_READY(ID_MPI_Alltoall)
  CASE_READY(ID_MPI_Alltoallv)
  CASE_READY(ID_MPI_Alltoallw)
  CASE_READY(ID_MPI_Attr_delete)
  CASE_READY(ID_MPI_Attr_get)
  CASE_READY(ID_MPI_Attr_put)
  CASE_READY(ID_MPI_Barrier)
  CASE_READY(ID_MPI_Bcast)
  CASE_READY(ID_MPI_Bsend)
  CASE_READY(ID_MPI_Bsend_init)
  CASE_READY(ID_MPI_Buffer_attach)
  CASE_READY(ID_MPI_Buffer_detach)
  CASE_READY(ID_MPI_Cancel)
  CASE_READY(ID_MPI_Cart_coords)
  CASE_READY(ID_MPI_Cart_create)
  CASE_READY(ID_MPI_Cart_get)
  CASE_READY(ID_MPI_Cart_map)
  CASE_READY(ID_MPI_Cart_rank)
  CASE_READY(ID_MPI_Cart_shift)
  CASE_READY(ID_MPI_Cart_sub)
  CASE_READY(ID_MPI_Cartdim_get)
  CASE_READY(ID_MPI_Close_port)
  CASE_READY(ID_MPI_Comm_accept)
  CASE_READY(ID_MPI_Comm_call_errhandler)
  CASE_READY(ID_MPI_Comm_compare)
  CASE_READY(ID_MPI_Comm_connect)
  CASE_READY(ID_MPI_Comm_create)
  CASE_READY(ID_MPI_Comm_create_errhandler)
  CASE_READY(ID_MPI_Comm_create_group)
  CASE_READY(ID_MPI_Comm_create_keyval)
  CASE_READY(ID_MPI_Comm_delete_attr)
  CASE_READY(ID_MPI_Comm_disconnect)
  CASE_READY(ID_MPI_Comm_dup)
  CASE_READY(ID_MPI_Comm_dup_with_info)
  CASE_READY(ID_MPI_Comm_free)
  CASE_READY(ID_MPI_Comm_free_keyval)
  CASE_READY(ID_MPI_Comm_get_attr)
  CASE_READY(ID_MPI_Comm_get_errhandler)
  CASE_READY(ID_MPI_Comm_get_info)
  CASE_READY(ID_MPI_Comm_get_name)
  CASE_READY(ID_MPI_Comm_get_parent)
  CASE_READY(ID_MPI_Comm_group)
  CASE_READY(ID_MPI_Comm_idup)
  CASE_READY(ID_MPI_Comm_join)
  CASE_IGNORE(ID_MPI_Comm_rank)
  CASE_READY(ID_MPI_Comm_remote_group)
  CASE_READY(ID_MPI_Comm_remote_size)
  CASE_READY(ID_MPI_Comm_set_attr)
  CASE_READY(ID_MPI_Comm_set_errhandler)
  CASE_READY(ID_MPI_Comm_set_info)
  CASE_READY(ID_MPI_Comm_set_name)
  CASE_IGNORE(ID_MPI_Comm_size)
  CASE_READY(ID_MPI_Comm_spawn)
  CASE_READY(ID_MPI_Comm_spawn_multiple)
  CASE_READY(ID_MPI_Comm_split)
  CASE_READY(ID_MPI_Comm_split_type)
  CASE_READY(ID_MPI_Comm_test_inter)
  CASE_READY(ID_MPI_Compare_and_swap)
  CASE_READY(ID_MPI_Dims_create)
  CASE_READY(ID_MPI_Dist_graph_create)
  CASE_READY(ID_MPI_Dist_graph_create_adjacent)
  CASE_READY(ID_MPI_Dist_graph_neighbors)
  CASE_READY(ID_MPI_Dist_graph_neighbors_count)
  CASE_READY(ID_MPI_Errhandler_create)
  CASE_READY(ID_MPI_Errhandler_free)
  CASE_READY(ID_MPI_Errhandler_get)
  CASE_READY(ID_MPI_Errhandler_set)
  CASE_READY(ID_MPI_Error_class)
  CASE_READY(ID_MPI_Error_string)
  CASE_READY(ID_MPI_Exscan)
  CASE_READY(ID_MPI_Fetch_and_op)
  CASE_READY(ID_MPI_File_c2f)
  CASE_READY(ID_MPI_File_call_errhandler)
  CASE_READY(ID_MPI_File_close)
  CASE_READY(ID_MPI_File_create_errhandler)
  CASE_READY(ID_MPI_File_delete)
  CASE_READY(ID_MPI_File_f2c)
  CASE_READY(ID_MPI_File_get_amode)
  CASE_READY(ID_MPI_File_get_atomicity)
  CASE_READY(ID_MPI_File_get_byte_offset)
  CASE_READY(ID_MPI_File_get_errhandler)
  CASE_READY(ID_MPI_File_get_group)
  CASE_READY(ID_MPI_File_get_info)
  CASE_READY(ID_MPI_File_get_position)
  CASE_READY(ID_MPI_File_get_position_shared)
  CASE_READY(ID_MPI_File_get_size)
  CASE_READY(ID_MPI_File_get_type_extent)
  CASE_READY(ID_MPI_File_get_view)
  CASE_READY(ID_MPI_File_iread)
  CASE_READY(ID_MPI_File_iread_all)
  CASE_READY(ID_MPI_File_iread_at)
  CASE_READY(ID_MPI_File_iread_at_all)
  CASE_READY(ID_MPI_File_iread_shared)
  CASE_READY(ID_MPI_File_iwrite)
  CASE_READY(ID_MPI_File_iwrite_all)
  CASE_READY(ID_MPI_File_iwrite_at)
  CASE_READY(ID_MPI_File_iwrite_at_all)
  CASE_READY(ID_MPI_File_iwrite_shared)
  CASE_READY(ID_MPI_File_open)
  CASE_READY(ID_MPI_File_preallocate)
  CASE_READY(ID_MPI_File_read)
  CASE_READY(ID_MPI_File_read_all)
  CASE_READY(ID_MPI_File_read_all_begin)
  CASE_READY(ID_MPI_File_read_all_end)
  CASE_READY(ID_MPI_File_read_at)
  CASE_READY(ID_MPI_File_read_at_all)
  CASE_READY(ID_MPI_File_read_at_all_begin)
  CASE_READY(ID_MPI_File_read_at_all_end)
  CASE_READY(ID_MPI_File_read_ordered)
  CASE_READY(ID_MPI_File_read_ordered_begin)
  CASE_READY(ID_MPI_File_read_ordered_end)
  CASE_READY(ID_MPI_File_read_shared)
  CASE_READY(ID_MPI_File_seek)
  CASE_READY(ID_MPI_File_seek_shared)
  CASE_READY(ID_MPI_File_set_atomicity)
  CASE_READY(ID_MPI_File_set_errhandler)
  CASE_READY(ID_MPI_File_set_info)
  CASE_READY(ID_MPI_File_set_size)
  CASE_READY(ID_MPI_File_set_view)
  CASE_READY(ID_MPI_File_sync)
  CASE_READY(ID_MPI_File_write)
  CASE_READY(ID_MPI_File_write_all)
  CASE_READY(ID_MPI_File_write_all_begin)
  CASE_READY(ID_MPI_File_write_all_end)
  CASE_READY(ID_MPI_File_write_at)
  CASE_READY(ID_MPI_File_write_at_all)
  CASE_READY(ID_MPI_File_write_at_all_begin)
  CASE_READY(ID_MPI_File_write_at_all_end)
  CASE_READY(ID_MPI_File_write_ordered)
  CASE_READY(ID_MPI_File_write_ordered_begin)
  CASE_READY(ID_MPI_File_write_ordered_end)
  CASE_READY(ID_MPI_File_write_shared)
  CASE_READY(ID_MPI_Finalize, call->on_trigger = [=] () {call->app->GetMpi()->finalize();})
  CASE_IGNORE(ID_MPI_Finalized)
  CASE_READY(ID_MPI_Free_mem)
  CASE_READY(ID_MPI_Gather)
  CASE_READY(ID_MPI_Gatherv)
  CASE_READY(ID_MPI_Get)
  CASE_READY(ID_MPI_Get_accumulate)
  CASE_READY(ID_MPI_Get_address)
  CASE_READY(ID_MPI_Get_count)
  CASE_READY(ID_MPI_Get_elements)
  CASE_READY(ID_MPI_Get_elements_x)
  CASE_READY(ID_MPI_Get_library_version)
  CASE_READY(ID_MPI_Get_processor_name)
  CASE_READY(ID_MPI_Get_version)
  CASE_READY(ID_MPI_Graph_create)
  CASE_READY(ID_MPI_Graph_get)
  CASE_READY(ID_MPI_Graph_map)
  CASE_READY(ID_MPI_Graph_neighbors)
  CASE_READY(ID_MPI_Graph_neighbors_count)
  CASE_READY(ID_MPI_Graphdims_get)
  CASE_READY(ID_MPI_Grequest_complete)
  CASE_READY(ID_MPI_Grequest_start)
  CASE_READY(ID_MPI_Group_compare)
  CASE_READY(ID_MPI_Group_difference)
  CASE_READY(ID_MPI_Group_excl)
  CASE_READY(ID_MPI_Group_free)
  CASE_READY(ID_MPI_Group_incl)
  CASE_READY(ID_MPI_Group_intersection)
  CASE_READY(ID_MPI_Group_range_excl)
  CASE_READY(ID_MPI_Group_range_incl)
  CASE_READY(ID_MPI_Group_rank)
  CASE_READY(ID_MPI_Group_size)
  CASE_READY(ID_MPI_Group_translate_ranks)
  CASE_READY(ID_MPI_Group_union)
  CASE_READY(ID_MPI_Iallgather)
  CASE_READY(ID_MPI_Iallgatherv)
  CASE_READY(ID_MPI_Iallreduce)
  CASE_READY(ID_MPI_Ialltoall)
  CASE_READY(ID_MPI_Ialltoallv)
  CASE_READY(ID_MPI_Ialltoallw)
  CASE_READY(ID_MPI_Ibarrier)
  CASE_READY(ID_MPI_Ibcast)
  CASE_READY(ID_MPI_Ibsend)
  CASE_READY(ID_MPI_Iexscan)
  CASE_READY(ID_MPI_Igather)
  CASE_READY(ID_MPI_Igatherv)
  CASE_READY(ID_MPI_Improbe)
  CASE_READY(ID_MPI_Imrecv)
  CASE_READY(ID_MPI_Ineighbor_allgather)
  CASE_READY(ID_MPI_Ineighbor_allgatherv)
  CASE_READY(ID_MPI_Ineighbor_alltoall)
  CASE_READY(ID_MPI_Ineighbor_alltoallv)
  CASE_READY(ID_MPI_Ineighbor_alltoallw)
  CASE_READY(ID_MPI_Info_create)
  CASE_READY(ID_MPI_Info_delete)
  CASE_READY(ID_MPI_Info_dup)
  CASE_READY(ID_MPI_Info_free)
  CASE_READY(ID_MPI_Info_get)
  CASE_READY(ID_MPI_Info_get_nkeys)
  CASE_READY(ID_MPI_Info_get_nthkey)
  CASE_READY(ID_MPI_Info_get_valuelen)
  CASE_READY(ID_MPI_Info_set)
  CASE_READY(ID_MPI_Init, call->on_trigger = [=] () {call->app->GetMpi()->init(nullptr, nullptr);};)
  CASE_READY(ID_MPI_Init_thread)
  CASE_IGNORE(ID_MPI_Initialized)
  CASE_READY(ID_MPI_Intercomm_create)
  CASE_READY(ID_MPI_Intercomm_merge)
  CASE_READY(ID_MPI_Iprobe)
  CASE_NOT_READY(ID_MPI_Irecv)
  CASE_READY(ID_MPI_Ireduce)
  CASE_READY(ID_MPI_Ireduce_scatter)
  CASE_READY(ID_MPI_Ireduce_scatter_block)
  CASE_READY(ID_MPI_Irsend)
  CASE_READY(ID_MPI_Is_thread_main)
  CASE_READY(ID_MPI_Iscan)
  CASE_READY(ID_MPI_Iscatter)
  CASE_READY(ID_MPI_Iscatterv)
  CASE_READY(ID_MPI_Isend)
  CASE_READY(ID_MPI_Issend)
  CASE_READY(ID_MPI_Keyval_create)
  CASE_READY(ID_MPI_Keyval_free)
  CASE_READY(ID_MPI_Lookup_name)
  CASE_READY(ID_MPI_Mprobe)
  CASE_READY(ID_MPI_Mrecv)
  CASE_READY(ID_MPI_Neighbor_allgather)
  CASE_READY(ID_MPI_Neighbor_allgatherv)
  CASE_READY(ID_MPI_Neighbor_alltoall)
  CASE_READY(ID_MPI_Neighbor_alltoallv)
  CASE_READY(ID_MPI_Neighbor_alltoallw)
  CASE_READY(ID_MPI_Op_commute)
  CASE_READY(ID_MPI_Op_create)
  CASE_READY(ID_MPI_Op_free)
  CASE_READY(ID_MPI_Open_port)
  CASE_READY(ID_MPI_Pack)
  CASE_READY(ID_MPI_Pack_external)
  CASE_READY(ID_MPI_Pack_external_size)
  CASE_READY(ID_MPI_Pack_size)
  CASE_READY(ID_MPI_Pcontrol)
  CASE_READY(ID_MPI_Probe)
  CASE_READY(ID_MPI_Publish_name)
  CASE_READY(ID_MPI_Put)
  CASE_READY(ID_MPI_Query_thread)
  CASE_READY(ID_MPI_Raccumulate)
  CASE_READY(ID_MPI_Recv)
  CASE_READY(ID_MPI_Recv_init)
  CASE_READY(ID_MPI_Reduce)
  CASE_READY(ID_MPI_Reduce_local)
  CASE_READY(ID_MPI_Reduce_scatter)
  CASE_READY(ID_MPI_Reduce_scatter_block)
  CASE_READY(ID_MPI_Register_datarep)
  CASE_READY(ID_MPI_Request_free)
  CASE_READY(ID_MPI_Request_get_status)
  CASE_READY(ID_MPI_Rget)
  CASE_READY(ID_MPI_Rget_accumulate)
  CASE_READY(ID_MPI_Rput)
  CASE_READY(ID_MPI_Rsend)
  CASE_READY(ID_MPI_Rsend_init)
  CASE_READY(ID_MPI_Scan)
  CASE_READY(ID_MPI_Scatter)
  CASE_READY(ID_MPI_Scatterv)
  CASE_READY(ID_MPI_Send)
  CASE_READY(ID_MPI_Send_init)
  CASE_READY(ID_MPI_Sendrecv)
  CASE_READY(ID_MPI_Sendrecv_replace)
  CASE_READY(ID_MPI_Ssend)
  CASE_READY(ID_MPI_Ssend_init)
  CASE_READY(ID_MPI_Start)
  CASE_READY(ID_MPI_Startall)
  CASE_READY(ID_MPI_Status_set_cancelled)
  CASE_READY(ID_MPI_Status_set_elements)
  CASE_READY(ID_MPI_Status_set_elements_x)
  CASE_READY(ID_MPI_T_category_changed)
  CASE_READY(ID_MPI_T_category_get_categories)
  CASE_READY(ID_MPI_T_category_get_cvars)
  CASE_READY(ID_MPI_T_category_get_info)
  CASE_READY(ID_MPI_T_category_get_num)
  CASE_READY(ID_MPI_T_category_get_pvars)
  CASE_READY(ID_MPI_T_cvar_get_info)
  CASE_READY(ID_MPI_T_cvar_get_num)
  CASE_READY(ID_MPI_T_cvar_handle_alloc)
  CASE_READY(ID_MPI_T_cvar_handle_free)
  CASE_READY(ID_MPI_T_cvar_read)
  CASE_READY(ID_MPI_T_cvar_write)
  CASE_READY(ID_MPI_T_enum_get_info)
  CASE_READY(ID_MPI_T_enum_get_item)
  CASE_READY(ID_MPI_T_finalize)
  CASE_READY(ID_MPI_T_init_thread)
  CASE_READY(ID_MPI_T_pvar_get_info)
  CASE_READY(ID_MPI_T_pvar_get_num)
  CASE_READY(ID_MPI_T_pvar_handle_alloc)
  CASE_READY(ID_MPI_T_pvar_handle_free)
  CASE_READY(ID_MPI_T_pvar_read)
  CASE_READY(ID_MPI_T_pvar_readreset)
  CASE_READY(ID_MPI_T_pvar_reset)
  CASE_READY(ID_MPI_T_pvar_session_create)
  CASE_READY(ID_MPI_T_pvar_session_free)
  CASE_READY(ID_MPI_T_pvar_start)
  CASE_READY(ID_MPI_T_pvar_stop)
  CASE_READY(ID_MPI_T_pvar_write)
  CASE_READY(ID_MPI_Test)
  CASE_READY(ID_MPI_Test_cancelled)
  CASE_READY(ID_MPI_Testall)
  CASE_READY(ID_MPI_Testany)
  CASE_READY(ID_MPI_Testsome)
  CASE_READY(ID_MPI_Topo_test)
  CASE_READY(ID_MPI_Type_commit)
  CASE_READY(ID_MPI_Type_contiguous)
  CASE_READY(ID_MPI_Type_create_darray)
  CASE_READY(ID_MPI_Type_create_hindexed)
  CASE_READY(ID_MPI_Type_create_hindexed_block)
  CASE_READY(ID_MPI_Type_create_hvector)
  CASE_READY(ID_MPI_Type_create_indexed_block)
  CASE_READY(ID_MPI_Type_create_keyval)
  CASE_READY(ID_MPI_Type_create_resized)
  CASE_READY(ID_MPI_Type_create_struct)
  CASE_READY(ID_MPI_Type_create_subarray)
  CASE_READY(ID_MPI_Type_delete_attr)
  CASE_READY(ID_MPI_Type_dup)
  CASE_READY(ID_MPI_Type_extent)
  CASE_READY(ID_MPI_Type_free)
  CASE_READY(ID_MPI_Type_free_keyval)
  CASE_READY(ID_MPI_Type_get_attr)
  CASE_READY(ID_MPI_Type_get_contents)
  CASE_READY(ID_MPI_Type_get_envelope)
  CASE_READY(ID_MPI_Type_get_extent)
  CASE_READY(ID_MPI_Type_get_extent_x)
  CASE_READY(ID_MPI_Type_get_name)
  CASE_READY(ID_MPI_Type_get_true_extent)
  CASE_READY(ID_MPI_Type_get_true_extent_x)
  CASE_READY(ID_MPI_Type_hindexed)
  CASE_READY(ID_MPI_Type_hvector)
  CASE_READY(ID_MPI_Type_indexed)
  CASE_READY(ID_MPI_Type_lb)
  CASE_READY(ID_MPI_Type_match_size)
  CASE_READY(ID_MPI_Type_set_attr)
  CASE_READY(ID_MPI_Type_set_name)
  CASE_READY(ID_MPI_Type_size)
  CASE_READY(ID_MPI_Type_size_x)
  CASE_READY(ID_MPI_Type_struct)
  CASE_READY(ID_MPI_Type_ub)
  CASE_READY(ID_MPI_Type_vector)
  CASE_READY(ID_MPI_Unpack)
  CASE_READY(ID_MPI_Unpack_external)
  CASE_READY(ID_MPI_Unpublish_name)

  // MPI_Wait is used for all waits
  ADD_SPECIAL_CASE(ID_MPI_Waitall)
  ADD_SPECIAL_CASE(ID_MPI_Waitany)
  ADD_SPECIAL_CASE(ID_MPI_Waitsome)
  CASE_READY(ID_MPI_Wait)

  CASE_IGNORE(ID_MPI_Win_allocate)
  CASE_IGNORE(ID_MPI_Win_allocate_shared)
  CASE_IGNORE(ID_MPI_Win_attach)
  CASE_IGNORE(ID_MPI_Win_call_errhandler)
  CASE_IGNORE(ID_MPI_Win_complete)
  CASE_IGNORE(ID_MPI_Win_create)
  CASE_IGNORE(ID_MPI_Win_create_dynamic)
  CASE_IGNORE(ID_MPI_Win_create_errhandler)
  CASE_IGNORE(ID_MPI_Win_create_keyval)
  CASE_IGNORE(ID_MPI_Win_delete_attr)
  CASE_IGNORE(ID_MPI_Win_detach)
  CASE_IGNORE(ID_MPI_Win_fence)
  CASE_IGNORE(ID_MPI_Win_flush)
  CASE_IGNORE(ID_MPI_Win_flush_all)
  CASE_IGNORE(ID_MPI_Win_flush_local)
  CASE_IGNORE(ID_MPI_Win_flush_local_all)
  CASE_IGNORE(ID_MPI_Win_free)
  CASE_IGNORE(ID_MPI_Win_free_keyval)
  CASE_IGNORE(ID_MPI_Win_get_attr)
  CASE_IGNORE(ID_MPI_Win_get_errhandler)
  CASE_IGNORE(ID_MPI_Win_get_group)
  CASE_IGNORE(ID_MPI_Win_get_info)
  CASE_IGNORE(ID_MPI_Win_get_name)
  CASE_IGNORE(ID_MPI_Win_lock)
  CASE_IGNORE(ID_MPI_Win_lock_all)
  CASE_IGNORE(ID_MPI_Win_post)
  CASE_IGNORE(ID_MPI_Win_set_attr)
  CASE_IGNORE(ID_MPI_Win_set_errhandler)
  CASE_IGNORE(ID_MPI_Win_set_info)
  CASE_IGNORE(ID_MPI_Win_set_name)
  CASE_IGNORE(ID_MPI_Win_shared_query)
  CASE_IGNORE(ID_MPI_Win_start)
  CASE_IGNORE(ID_MPI_Win_sync)
  CASE_IGNORE(ID_MPI_Win_test)
  CASE_IGNORE(ID_MPI_Win_unlock)
  CASE_IGNORE(ID_MPI_Win_unlock_all)
  CASE_IGNORE(ID_MPI_Win_wait)
  CASE_READY(ID_MPI_Wtick)
  CASE_READY(ID_MPI_Wtime)
  CASE_READY(ID_MPIX_Comm_agree)
  CASE_READY(ID_MPIX_Comm_failure_ack)
  CASE_READY(ID_MPIX_Comm_failure_get_acked)
  CASE_READY(ID_MPIX_Comm_revoke)
  CASE_READY(ID_MPIX_Comm_shrink)

  default:
      cout << "ERROR: 'event_leave' callback did not capture event: " << id << endl;
  }

#undef CASE_READY
#undef CASE_NOT_READY
  return OTF2_CALLBACK_SUCCESS;
}

#undef CASE_IGNORE