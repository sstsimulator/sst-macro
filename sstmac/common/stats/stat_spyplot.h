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

#ifndef SSTMAC_COMMON_STATS_STATS_COMMON_H_INCLUDED
#define SSTMAC_COMMON_STATS_STATS_COMMON_H_INCLUDED

#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/timestamp.h>
#include <sprockit/sim_parameters.h>
#include <iostream>
#include <fstream>
#include <map>
#include <unordered_map>

namespace sstmac {



/**
 * this stat_collector class keeps a spy plot
 */
template <class Dst, class Count>
class StatSpyplot : public SST::Statistics::MultiStatistic<Dst,Count>
{
  using StatSpyplotParent = SST::Statistics::MultiStatistic<Dst,Count>;
 public:
  SST_ELI_DECLARE_STATISTIC_TEMPLATE(
    StatSpyplot,
    "macro",
    "spyplot",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "spyplot showing traffic matrics",
    "Statistic<Src,Dst,Count>")

  StatSpyplot(SST::BaseComponent* comp, const std::string& name,
              const std::string& statName, SST::Params& params)
    : SST::Statistics::MultiStatistic<Dst,Count>(comp, name, statName, params)
  {
    n_dst_ = params.find<Dst>("ncols");
    vals_.resize(n_dst_);
    fields_.resize(n_dst_);
  }

  ~StatSpyplot() override {}

  void addData_impl(int dest, uint64_t num) override {
    vals_[dest] += num;
  }

  void registerOutputFields(SST::Statistics::StatisticFieldsOutput* output) override {
    for (int i=0; i < n_dst_; ++i){
      auto str = sprockit::sprintf("spy%d", i);
      fields_[i] = output->registerField<uint64_t>(str.c_str());
    }
  }

  void outputStatisticFields(SST::Statistics::StatisticFieldsOutput* output, bool  /*endOfSim*/) override {
    for (int i=0; i < n_dst_; ++i){
      output->outputField(fields_[i], vals_[i]);
    }
  }

 protected:
  std::vector<Count> vals_;
  Dst n_dst_;
  std::vector<SST::Statistics::StatisticOutput::fieldHandle_t> fields_;

};


}

#endif
