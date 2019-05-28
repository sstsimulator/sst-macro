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

#ifndef sstmac_common_STAT_HISTOGRAM_H
#define sstmac_common_STAT_HISTOGRAM_H

#include <sstmac/common/stats/stat_collector.h>
#include <sprockit/sim_parameters.h>
#include <vector>
#include <cmath>

namespace sstmac {

template <class T>
class StatAccumulator : public Statistic<T> {
 public:
  SST_ELI_DECLARE_STATISTIC_TEMPLATE(
      StatAccumulator,
      "macro",
      "accumulator",
      SST_ELI_ELEMENT_VERSION(1,0,0),
      "a histogram",
      "Statistic<T>")


  StatAccumulator(SST::BaseComponent* comp, const std::string& name,
                  const std::string& subName, SST::Params& params) :
      Statistic<T>(comp, name, subName, params),
      total_(0)
  {
  }

  void addData_impl(T value) override {
    total_ += value;
  }

  void registerOutputFields(SST::Statistics::StatisticOutput* statOutput) override {
    field_ = statOutput->registerField<T>("total");
  }

  void outputStatisticData(SST::Statistics::StatisticOutput* statOutput, bool EndOfSimFlag) override {
    statOutput->outputField(field_, total_);
  }

 private:
  T total_;
  SST::Statistics::StatisticOutput::fieldHandle_t field_;

};

}

#endif // STAT_HISTOGRAM_H
