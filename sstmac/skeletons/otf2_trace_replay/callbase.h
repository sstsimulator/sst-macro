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

#ifndef SSTMAC_SKELETONS_OTF2_TRACE_REPLAY_CALLBASE_H_
#define SSTMAC_SKELETONS_OTF2_TRACE_REPLAY_CALLBASE_H_

#include <sstmac/software/process/operating_system.h>
#include <sumi-mpi/mpi_api.h>
#include <otf2/otf2.h>
#include <string>
#include <functional>

#include "mpi_calls.h"

// forward declare
class OTF2TraceReplayApp;

class CallBase {
public:
    // Ctors
    CallBase();
    CallBase(OTF2TraceReplayApp*);
    CallBase(OTF2_LocationRef, OTF2_TimeStamp, OTF2TraceReplayApp* app = nullptr);
    CallBase(OTF2_LocationRef, OTF2_TimeStamp, OTF2_TimeStamp, OTF2TraceReplayApp* app = nullptr);
    virtual ~CallBase() {}

    // Methods
    sstmac::timestamp GetStart();
    sstmac::timestamp GetEnd();
    void SetTrigger(std::function<void()>);
    bool IsReady();
    void Trigger();
    const char* ToString();

    // Members
    OTF2_TimeStamp start_time, end_time;
    OTF2_LocationRef location;
    MPI_Request request_id;
    std::function<void()> on_trigger;
    OTF2TraceReplayApp* app;
    bool isready;
    int id;
    const char* name;

    static void assert_call(CallBase* cb, std::string msg);

private:
    sstmac::timestamp convert_time(const OTF2_TimeStamp);
};

#endif /* SSTMAC_SKELETONS_OTF2_TRACE_REPLAY_CALLBASE_H_ */
