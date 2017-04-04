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
  MpiCall(OTF2_TimeStamp start, OTF2TraceReplayApp* app,
          MPI_CALL_ID id, const char* name);

  ~MpiCall() {}

  // Methods
  sstmac::timestamp GetStart();
  sstmac::timestamp GetEnd();
  void SetTrigger(std::function<void()>);
  bool IsReady();
  void Trigger();
  const char* ToString();

  // Members
  OTF2_TimeStamp start_time, end_time;
  std::function<void()> on_trigger;
  OTF2TraceReplayApp* app;
  bool isready;
  MPI_CALL_ID id;
  const char* name;

  static void assert_call(MpiCall* cb, std::string msg);

 private:
  sstmac::timestamp convert_time(const OTF2_TimeStamp);
};

#endif /* SSTMAC_SKELETONS_OTF2_TRACE_REPLAY_MpiCall_H_ */
