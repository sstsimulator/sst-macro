/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/errors.h>
#include <sprockit/keyword_registration.h>


#if !SSTMAC_INTEGRATED_SST_CORE

namespace sstmac {

SST_ELI_INSTANTIATE_STATISTIC(NullStatistic, int)
SST_ELI_INSTANTIATE_STATISTIC(NullStatistic, double)
SST_ELI_INSTANTIATE_STATISTIC(NullStatistic, uint64_t)
SST_ELI_INSTANTIATE_STATISTIC(NullStatistic, void)

StatisticBase::StatisticBase(MacroBaseComponent * /*parent*/, const std::string &name,
                             const std::string &subName, SST::Params &params) :
  name_(name), sub_id_(subName), group_(nullptr)
{
  group_name_ = params.find<std::string>("group", "default");
  output_ = params.find<std::string>("output", "csv");
}

StatisticFieldsOutput::fieldHandle_t
StatisticFieldsOutput::implRegisterField(const char *fieldName)
{
  auto* grp = active_group_;
  auto iter = grp->ids.find(fieldName);
  if (iter == grp->ids.end()){
    int idx = grp->ids.size();
    grp->ids[fieldName] = idx;
    grp->columns[idx] = fieldName;
    return idx;
  } else {
    return iter->second;
  }
}

void
StatisticFieldsOutput::registerStatistic(StatisticBase *stat)
{
  active_stat_ = stat;
  active_group_ = stat->group();
  stat->registerOutputFields(this);
  active_stat_ = nullptr;
  active_group_ = nullptr;
}

void
StatOutputCSV::startOutputGroup(StatisticGroup *grp)
{
  if (!grp->columns.empty()){
    std::string fname = grp->name + ".csv";
    csv_out_.open(fname);
    csv_out_ << "name,component";
    for (auto& pair : grp->columns){
      csv_out_ << "," << pair.second;
    }
  }
}

void
StatOutputCSV::startOutputEntries(StatisticBase *stat)
{
  next_field_ = 0;
  csv_out_ << "\n" << stat->getStatName() << "," << stat->getStatSubId();
}

void
StatOutputCSV::stopOutputEntries()
{
}

void
StatOutputCSV::outputPending()
{
  //we might get fields output out of order
  //hold the values to ensure we output in order
  for (auto it = pending_.cbegin(); it != pending_.cend(); ){
    if (it->first == next_field_){
      csv_out_ << "," << it->second;
      pending_.erase(it++);
      ++next_field_;
    } else {
      return;
    }
  }
}

void
Statistic<void>::outputStatisticFields(StatisticFieldsOutput * /*output*/, bool  /*endOfSimFlag*/)
{
  spkt_abort_printf("void statistic '%s' should never call outputStatisticData\n"
                    "ensure that correct output is set for group '%s'",
                    name().c_str(), groupName().c_str());
}

void
Statistic<void>::registerOutputFields(StatisticFieldsOutput * /*statOutput*/)
{
  spkt_abort_printf("void statistic '%s' should never call registerOutputFields\n"
                    "ensure that correct output is set for group '%s'",
                    name().c_str(), groupName().c_str());
}

} // end of namespace sstmac

#endif
