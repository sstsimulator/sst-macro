/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/skeletons/otf2_trace_replay/callqueue.h>
#include <sstmac/skeletons/otf2_trace_replay/otf2_trace_replay.h>

using namespace std;

#if 1
    #define TRIGGER_PRINT(...) cerr << "TRIGGERED CALL (#" << app->rank << "): " << __VA_ARGS__ << endl;
#else
    #define TRIGGER_PRINT(...)
#endif

/******************************************************************************
 *  BaseCall members and functions
 */


MpiCall::MpiCall(OTF2_TimeStamp start, OTF2TraceReplayApp* app,
                 MPI_CALL_ID ID, const char* _name) :
  isready(false), app(app),
  start_time(start),
  end_time(0),
  name(_name),
  id(ID)
{
}

bool MpiCall::IsReady() {
    return isready;
}

const char* MpiCall::ToString() {
	return name;
}

void MpiCall::assert_call(MpiCall* cb, string msg) {
  if (cb == NULL) {
      spkt_throw(sprockit::io_error, "ASSERT FAILED: ", msg.c_str());
  }
}

sstmac::timestamp MpiCall::convert_time(const OTF2_TimeStamp ts) {
	const auto start_offset = app->otf2_clock_properties.globalOffset;
	const auto ticks_per_second = app->otf2_clock_properties.timerResolution;

	return sstmac::timestamp(((double(ts) - start_offset)/ticks_per_second));
}

sstmac::timestamp MpiCall::GetStart() {
	if (start_time == 0) cerr << "Warning: start timestamp is not initialized for " << ToString() << endl;
	return convert_time(start_time);
}

sstmac::timestamp MpiCall::GetEnd() {
	if (end_time == 0) cerr << "Warning: end timestamp is not initialized for " << ToString() << endl;
    return convert_time(end_time);
}

void MpiCall::Trigger() {
  app->StartMpi(GetStart());
  if (on_trigger){
    on_trigger();
  }
  app->EndMpi(GetEnd());
}

/******************************************************************************
 *  CallQueue functions
 */

CallQueue::CallQueue() : CallQueue(NULL) {};
CallQueue::CallQueue(OTF2TraceReplayApp* app) {
  this->app = app;
}

void CallQueue::AddCall(MpiCall* cb) {
  // another strategy would be to make 'app' an argument that gets passed into 'Trigger'.
  // This would decrease the size of each callback by sizeof(ptr)
  cb->app = app;
  call_queue.push(cb);
}

int CallQueue::CallReady(MpiCall* call) {
  int triggered = 0;
  call->isready = true;

  if (call == call_queue.front()) {
    // when a call at the front of the queue is ready, there may be a
    // cascade of other ready calls behind it.
    while (call_queue.size() > 0 && call_queue.front()->IsReady()) {
      auto front = call_queue.front();
      front->Trigger();

      if (app->PrintMpiCalls()) {
         TRIGGER_PRINT(front->ToString());
      }
      call_queue.pop();
      delete front;
      triggered++;
    }
  }

  return triggered;
}

void CallQueue::AddRequest(MPI_Request req, MpiCall* cb) {
  request_map[req] = cb;
}

MpiCall* CallQueue::FindRequest(MPI_Request req)
{
  auto iter = request_map.find(req);
  if (iter == request_map.end()){
    spkt_abort_printf("Rank %d cannot find request %ld\n",
                      app->GetMpi()->rank(), req);
  }
  return iter->second;
}

void CallQueue::RemoveRequest(MPI_Request req)
{
  request_map.erase(req);
}

int CallQueue::GetDepth() {
  return call_queue.size();
}

MpiCall* CallQueue::Peek() {
  return call_queue.front();
}

MpiCall* CallQueue::PeekBack() {
  return call_queue.back();
}
