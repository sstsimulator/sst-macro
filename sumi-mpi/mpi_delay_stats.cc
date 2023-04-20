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
#include <sumi-mpi/mpi_delay_stats.h>

#if !SSTMAC_INTEGRATED_SST_CORE

namespace sumi {

DelayStats::DelayStats(SST::BaseComponent* comp, const std::string& name,
            const std::string& subName, SST::Params& params)
  : Parent(comp, name, subName, params)
{
}

void
DelayStats::addData_impl(int src, int dst, int type, int stage, 
                         uint64_t bytes, uint64_t flow_id,
                         double send_sync_delay, double recv_sync_delay,
                         double contention_delay,
                         double comm_delay, double min_delay, 
                         double active_sync_delay, double active_delay,
                         double time_since_quiesce, double time)
{
  messages_.emplace_back(src,dst,type,stage,bytes,flow_id,
                         send_sync_delay, recv_sync_delay,
                         contention_delay,
                         comm_delay, min_delay, 
                         active_sync_delay, active_delay, 
                         time_since_quiesce, time);
}

void
DelayStats::registerOutputFields(SST::Statistics::StatisticFieldsOutput * /*statOutput*/)
{
  sprockit::abort("DelayStats::registerOutputFields: should not be called");
}

void
DelayStats::outputStatisticFields(SST::Statistics::StatisticFieldsOutput * /*output*/, bool  /*endOfSimFlag*/)
{
  sprockit::abort("DelayStats::outputStatisticData: should not be called");
}

DelayStatsOutput::DelayStatsOutput(SST::Params& params) :
    sstmac::StatisticOutput(params)
{
}

void
DelayStatsOutput::startOutputGroup(sstmac::StatisticGroup *grp)
{
  auto outfile = grp->name + ".csv";
  out_.open(outfile);
  out_ << "component,src,dst,type,stage,size,flow,send_sync,recv_sync,injection,network,min,active_sync,active_total,quiesce_time,time";
}

void
DelayStatsOutput::stopOutputGroup()
{
  out_.close();
}

void
DelayStatsOutput::output(SST::Statistics::StatisticBase* statistic, bool  /*endOfSimFlag*/)
{
  DelayStats* stats = dynamic_cast<DelayStats*>(statistic);
  for (auto iter=stats->begin(); iter != stats->end(); ++iter){
    const DelayStats::Message& m = *iter;
    out_ << "\n" << stats->getStatSubId()
         << "," << m.src
         << "," << m.dst
         << "," << m.type
         << "," << m.stage
         << "," << m.length
         << "," << m.flow_id
         << "," << m.send_sync_delay
         << "," << m.recv_sync_delay
         << "," << m.inj_delay
         << "," << m.contention_delay
         << "," << m.min_delay
         << "," << m.active_sync_delay
         << "," << m.active_delay
         << "," << m.time_since_quiesce
         << "," << m.time
    ;
  }
}

}


#endif
