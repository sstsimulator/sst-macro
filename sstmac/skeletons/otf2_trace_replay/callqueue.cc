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

#include "callqueue.h"
#include "otf2_trace_replay.h"

/******************************************************************************
 *  BaseCall functions
 */

CallBase::CallBase() : CallBase(NULL) {}
CallBase::CallBase(OTF2_trace_replay_app* app) : CallBase(0, 0, app) {}
CallBase::CallBase(OTF2_LocationRef location, OTF2_TimeStamp _start, OTF2_trace_replay_app* app=NULL) : CallBase(location, _start, 0, app) {}
CallBase::CallBase(OTF2_LocationRef location, OTF2_TimeStamp _start, OTF2_TimeStamp _stop, OTF2_trace_replay_app* app=NULL) : isready(false), location(location), app(app) {
    start_time = _start;
    end_time = _stop;
}

bool CallBase::IsReady() {
    return isready;
}

string const CallBase::ToString() {
    return "CallBase";
}

void CallBase::assert_call(CallBase* cb, string msg) {
    if (cb == NULL) {
        cerr << "ASSERT FAILED: " << msg << endl;
        exit(1);
    }
}

sstmac::timestamp CallBase::convert_time(const OTF2_TimeStamp ts) {
	const auto start_offset = app->otf2_clock_properties.globalOffset;
	const auto ticks_per_second = app->otf2_clock_properties.timerResolution;

	return sstmac::timestamp(((double(ts) - start_offset)/ticks_per_second));
}

sstmac::timestamp CallBase::GetStart() {
	if (start_time == 0) cerr << "Warning: start timestamp is not initialized for " << ToString() << endl;
	return convert_time(start_time);
}

sstmac::timestamp CallBase::GetEnd() {
	if (end_time == 0) cerr << "Warning: end timestamp is not initialized for " << ToString() << endl;
    return convert_time(end_time);
}

/******************************************************************************
 *  CallQueue functions
 */

CallQueue::CallQueue() : CallQueue(NULL) {};
CallQueue::CallQueue(OTF2_trace_replay_app* app) {
    this->app = app;
}

void CallQueue::AddCall(CallBase* cb) {
    // another strategy would be to make 'app' an argument that gets passed into 'Trigger'.
    // This would decrease the size of each callback by sizeof(ptr)
    cb->app = app;
    call_queue.push(cb);
}

int CallQueue::CallReady(CallBase* call) {
    int triggered = 0;
    call->isready = true;

    if (call == call_queue.front()) {
        // when a call at the front of the queue is resolved, there may be a
        // cascade of other ready calls behind it.
        while (call_queue.size() > 0 && call_queue.front()->IsReady()) {
            auto front = call_queue.front();
            front->Trigger();
            call_queue.pop();
            delete front;
            triggered++;
        }
    }

    return triggered;
}

int CallQueue::GetDepth() {
    return call_queue.size();
}

CallBase* CallQueue::Peek() {
    return call_queue.front();
}
