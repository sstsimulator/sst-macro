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
#include <iostream>
#include <fstream>
#include <map>
#include <unordered_map>

namespace sstmac {

using StatSpyplotParent =
  SST::Statistics::MultiCtor<int,int,int>::Statistic<int,int,uint64_t>;

/**
 * this stat_collector class keeps a spy plot
 */
class StatSpyplot : public StatSpyplotParent
{
 public:
  SST_ELI_REGISTER_CUSTOM_STATISTIC(
    StatSpyplotParent,
    StatSpyplot,
    "macro",
    "spyplot",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "spyplot showing traffic matrics")

  StatSpyplot(SST::BaseComponent* comp, const std::string& name,
              const std::string& statName, SST::Params& params,
              int id, int nSrc, int nDst) :
    vals_(nDst),
    my_id_(id),
    n_src_(nSrc),
    n_dst_(nDst),
    fields_(nDst),
    StatSpyplotParent(comp, name, statName, params)
  {
  }

  virtual ~StatSpyplot() {}

  void addData_impl(int source, int dest, uint64_t num) override;

  void registerOutputFields(SST::Statistics::StatisticOutput* output) override;

  void outputStatisticData(SST::Statistics::StatisticOutput* output, bool endOfSim) override;

 protected:
  std::vector<uint64_t> vals_;
  int n_src_;
  int n_dst_;
  int my_id_;
  std::vector<SST::Statistics::StatisticOutput::fieldHandle_t> fields_;

};


}

#endif
