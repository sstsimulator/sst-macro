/**
Copyright 2009-2023 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2023, NTESS

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
#ifndef MPI_DELAY_STATS_H_INCLUDED
#define MPI_DELAY_STATS_H_INCLUDED

#include <sstmac/common/stats/stat_collector.h>

#if !SSTMAC_INTEGRATED_SST_CORE

namespace sumi {

class DelayStats : public SST::Statistics::MultiStatistic<int,int,int,int,uint64_t,uint64_t,double,double,double,double,double,double,double,double,double> {
 public:
  using Parent=SST::Statistics::MultiStatistic<int,int,int,int,uint64_t,uint64_t,double,double,double,double,double,double,double,double,double>;

  struct Message {
    int src;
    int dst;
    int type;
    int stage;
    uint64_t length;
    uint64_t flow_id;
    double send_sync_delay;
    double recv_sync_delay;
    double contention_delay;
    double inj_delay;
    double min_delay;
    double active_sync_delay;
    double active_delay;
    double time_since_quiesce;
    double time;
    Message(int s, int d, int t, int st, uint64_t l, uint64_t fid,
            double ssd, double rsd, double cd, double id, double md, 
            double asd, double ad, double tsq, double tme) :
      src(s), dst(d), type(t), stage(st), length(l), flow_id(fid),
      send_sync_delay(ssd), recv_sync_delay(rsd), 
      contention_delay(cd),
      inj_delay(id), min_delay(md),
      active_sync_delay(asd), active_delay(ad),
      time_since_quiesce(tsq), time(tme)
    {
    }
  };

  SST_ELI_REGISTER_MULTI_STATISTIC(
    Parent,
    DelayStats,
    "macro",
    "message_delay",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "fixed-time quanta activity of individual processes")

  DelayStats(SST::BaseComponent* comp, const std::string& name,
              const std::string& subName, SST::Params& params);

  ~DelayStats() override{}

  void addData_impl(int src, int dst, int type, int stage, 
                    uint64_t bytes, uint64_t flow_id,
                    double send_sync_delay,
                    double recv_sync_delay, double contention_delay,
                    double comm_delay, double min_delay, 
                    double active_sync_delay, double active_delay,
                    double time_since_quiesce, double time) override;

  void registerOutputFields(SST::Statistics::StatisticFieldsOutput *statOutput) override;

  void outputStatisticFields(SST::Statistics::StatisticFieldsOutput *output, bool endOfSimFlag) override;

  std::vector<Message>::const_iterator begin() const {
    return messages_.begin();
  }

  std::vector<Message>::const_iterator end() const {
    return messages_.end();
  }

 private:
  std::vector<Message> messages_;

};

class DelayStatsOutput : public sstmac::StatisticOutput
{
 public:
  SST_ELI_REGISTER_DERIVED(
    SST::Statistics::StatisticOutput,
    DelayStatsOutput,
    "macro",
    "message_delay",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "Dumps a CSV file with all the point-to-point stats")

  DelayStatsOutput(SST::Params& params);

  ~DelayStatsOutput() override{}

  void registerStatistic(SST::Statistics::StatisticBase*) override {}

  void startOutputGroup(SST::Statistics::StatisticGroup*) override;
  void stopOutputGroup() override;

  void output(SST::Statistics::StatisticBase* statistic, bool endOfSimFlag) override;

  bool checkOutputParameters() override { return true; }
  void startOfSimulation() override {}
  void endOfSimulation() override {}
  void printUsage() override {}

 private:
  std::ofstream out_;

};


}

#endif

#endif

