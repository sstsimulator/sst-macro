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

#ifndef sstmac_common_stats_FTQ_H
#define sstmac_common_stats_FTQ_H

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/software/process/thread_fwd.h>
#include <stdint.h>
#include <vector>

#include <unordered_map>

#include <string>
#include <stdlib.h>

/**
  "Fixed time quanta" collection of the amount of work done in particular time intervals
  over the course of the computation
*/

#if !SSTMAC_INTEGRATED_SST_CORE
namespace sstmac {


class FTQAccumulator : public SST::Statistics::MultiStatistic<int,uint64_t,uint64_t>
{
  using Parent=SST::Statistics::MultiStatistic<int,uint64_t,uint64_t>;
 public:
  SST_ELI_REGISTER_MULTI_STATISTIC(
    Parent,
    FTQAccumulator,
    "macro",
    "ftq_accumulator",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "fixed-time quanta activity of individual processes")

  FTQAccumulator(SST::BaseComponent* comp, const std::string& name,
                 const std::string& subName, SST::Params& params);

  ~FTQAccumulator(){}

  void registerOutputFields(SST::Statistics::StatisticOutput* statOutput) override;
  void outputStatisticData(SST::Statistics::StatisticOutput* statOutput, bool endOfSim) override;

  void addData_impl(int event_typeid, uint64_t  /*ticks_begin*/, uint64_t num_ticks) override {
    event_counts_[event_typeid] += num_ticks;
  }

 private:
  std::vector<uint64_t> event_counts_;

};

class FTQCalendar : public SST::Statistics::MultiStatistic<int,uint64_t,uint64_t>
{
  using Parent=SST::Statistics::MultiStatistic<int,uint64_t,uint64_t>;
 public:
  struct Event {
    int type;
    uint64_t start;
    uint64_t length;
    Event(int t, uint64_t s, uint64_t l) :
      type(t), start(s), length(l)
    {
    }
  };

  SST_ELI_REGISTER_MULTI_STATISTIC(
    Parent,
    FTQCalendar,
    "macro",
    "ftq_calendar",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "fixed-time quanta activity of individual processes")

  FTQCalendar(SST::BaseComponent* comp, const std::string& name,
              const std::string& subName, SST::Params& params);

  ~FTQCalendar(){}

  void addData_impl(int event_typeid, uint64_t ticks_begin, uint64_t num_ticks);

  bool empty() const {
    return events_.empty();
  }

  uint64_t eventsUsed() const {
    return events_used_;
  }

  void registerOutputFields(StatisticFieldsOutput *statOutput);

  void outputStatisticData(StatisticFieldsOutput *output, bool endOfSimFlag);

  /**
  * The calendar may not have collected events until the very end. 
  * Pad with the "inactive" tag until the end
  */
  void padToMaxTick(uint64_t max_tick);

  uint64_t maxTick() const {
    if (events_.empty()){
      return 0;
    } else {
      auto& ev = events_.back();
      return ev.start + ev.length;
    }
  }

  const std::vector<Event>& events() const {
    return events_;
  }

 private:
  std::vector<Event> events_;
  uint64_t events_used_;

};

class FTQOutput : public sstmac::StatisticOutput
{
 public:
  SST_ELI_REGISTER_DERIVED(
    SST::Statistics::StatisticOutput,
    FTQOutput,
    "macro",
    "ftq",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "Writes a matplotlib file for FTQ plots")

  FTQOutput(SST::Params& params);

  ~FTQOutput(){}

  void registerStatistic(SST::Statistics::StatisticBase* stat) override {}

  void startOutputGroup(SST::Statistics::StatisticGroup * grp) override;
  void stopOutputGroup() override;

  void output(SST::Statistics::StatisticBase* statistic, bool endOfSimFlag) override;

  bool checkOutputParameters() override { return true; }
  void startOfSimulation() override {}
  void endOfSimulation() override {}
  void printUsage() override {}
  void dump(const std::vector<FTQCalendar*>& calendars, std::ostream& os,
            bool includeHeaders, const std::string& outname);

 private:
  struct EpochList {
    uint64_t& operator()(int event, uint64_t epoch){
      uint64_t index = epoch * num_events_ + event;
      return event_counts_[index];
    }

    EpochList(int num_events, uint64_t num_epochs) :
      event_counts_(num_events*num_epochs,0),
      num_events_(num_events)
    {
    }

    std::vector<uint64_t> event_counts_;
    int num_events_;
  };

  uint64_t ticks_per_epoch_;
  std::vector<FTQCalendar*> aggregateCalendars_;
  std::map<std::string,std::vector<FTQCalendar*>> individualCalendars_;
  std::string active_group_;
  bool use_ftq_tags_;
  bool compute_mean_;
  bool aggregate_;
  std::ofstream out_;
  bool includeHeaders_;

};

}
#endif
//end not integrated core

#endif // FTQ_H
