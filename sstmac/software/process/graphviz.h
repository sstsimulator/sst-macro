/**
Copyright 2009-2021 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2021, NTESS

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

#ifndef SSTMAC_SOFTWARE_PROCESS_GRAPHVIZ_H
#define SSTMAC_SOFTWARE_PROCESS_GRAPHVIZ_H

#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/software/process/thread_fwd.h>

namespace sstmac {
namespace sw {

#if !SSTMAC_INTEGRATED_SST_CORE
class CallGraph : public SST::Statistics::CustomStatistic
{
 public:
  struct FunctionCall {
    uint64_t ncalls;
    uint64_t time_ticks;
    FunctionCall() : ncalls(0), time_ticks(0){}
  };

  /**
   * @brief The FunctionTrace class
   * Trace the time spent and number of calls for this function and
   * the functions called from this function
   */
  class FunctionTrace  {
   friend class CallGraph;
   private:
    std::vector<FunctionCall> calls_;

    uint64_t self_time_ticks_;

   public:
    FunctionTrace(int nfxn) : 
      calls_(nfxn),
      self_time_ticks_(0)
    {}

    uint64_t selfTime() const {
      return self_time_ticks_;
    }

    const std::vector<FunctionCall>& calls() const {
      return calls_;
    }

    std::string summary(const char* fxn) const;

    bool include() const;

    /**
     * @brief addCall Record a function called from this function
     * @param fxnId   The ID of the function called
     * @param ncalls  The number of calls to increment by
     * @param ticks   The number of time ticks elapsed
     */
    void addCall(int fxnId, int ncalls, uint64_t ticks) {
      auto& call = calls_[fxnId];
      call.ncalls += ncalls;
      call.time_ticks += ticks;
    }

    void addSelf(uint64_t ticks) {
      self_time_ticks_ += ticks;
    }

    /**
     * @brief reassignSelf Convert time previously recorded
     * as self (time spent directly in this function) to time
     * spent in a subroutine
     * @param fxnId  The function or subroutine to add time to
     * @param ticks  The length of time elapsed
     */
    void reassignSelf(int fxnId, uint64_t ticks) {
      self_time_ticks_ -= ticks;
      auto& call = calls_[fxnId];
      call.ncalls += 1;
      call.time_ticks += ticks;
    }

    void substractSelf(uint64_t ticks) {
      self_time_ticks_ -= ticks;
    }

  };

 public:
  SST_ELI_REGISTER_CUSTOM_STATISTIC(
    CallGraph,
    "macro",
    "call_graph",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "Creates a call graph output readable by KCacheGrind")

  CallGraph(SST::BaseComponent* comp, const std::string& name,
           const std::string& subName, SST::Params& params);

  ~CallGraph() override;

  void collect(uint64_t count, sw::Thread* thr);

  void reassign(int fxnId, uint64_t count, Thread* thr);

  const std::vector<FunctionTrace>& traces() const {
    return traces_;
  }

 private:
  /**
   * @brief addCall
   * @param ncalls    The number of calls to increment by
   * @param ticks     The time elapsed (in ticks)
   * @param fxnId     The parent function running the child function
   * @param callFxnId The child function time is spent in
   */
  void addCall(int ncalls, uint64_t ticks, int fxnId, int callFxnId);

  /**
   * @brief addSelf
   * @param fxnId
   * @param count
   */
  void addSelf(int fxnId, uint64_t count);

  std::vector<FunctionTrace> traces_;

  //friend class FunctionTrace;
  //friend class CallGraphOutput;

};

class CallGraphOutput : public sstmac::StatisticOutput
{
 public:
  SST_ELI_REGISTER_DERIVED(
    SST::Statistics::StatisticOutput,
    CallGraphOutput,
    "macro",
    "cachegrind",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "Writes call graphs of the simulation")

  CallGraphOutput(SST::Params& params);

  void registerStatistic(SST::Statistics::StatisticBase*) override {}

  void startOutputGroup(SST::Statistics::StatisticGroup * grp) override;
  void stopOutputGroup() override;

  void output(SST::Statistics::StatisticBase* statistic, bool endOfSimFlag) override;

  bool checkOutputParameters() override { return true; }
  void startOfSimulation() override {}
  void endOfSimulation() override {}
  void printUsage() override {}

  void dumpCallGraph(CallGraph* cgr);
  void dumpSummary(CallGraph* cgr);

 private:
  std::ofstream csv_summary_;

};
#endif


#define BACKTRACE_NFXN 50
typedef int CallGraphTrace[BACKTRACE_NFXN];

}
}


#endif // GRAPHVIZ_H
