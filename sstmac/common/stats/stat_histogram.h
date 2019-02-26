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

namespace sstmac {

template <class BinType, class CountType>
class StatHistogram : public SST::Statistics::MultiStatistic<BinType,CountType>
{
  using Parent=SST::Statistics::MultiStatistic<double,uint64_t>;
 public:
  SST_ELI_REGISTER_STATISTIC_TEMPLATE(
      StatHistogram,
      "macro",
      "histogram",
      SST_ELI_ELEMENT_VERSION(1,0,0),
      "a histogram with flexible counting",
      "Statistic<Bin,Count>")

  StatHistogram(SST::BaseComponent* comp, const std::string& name,
                const std::string& subName, SST::Params& params) :
      bin_size_(0),
      is_log_(false),
      SST::Statistics::MultiStatistic<BinType,CountType>(comp, name, subName, params)
  {
    min_val_ = params.findUnits("min_value").toDouble();
    max_val_ = params.findUnits("max_value").toDouble();
    bin_size_ = params.findUnits("bin_size").toDouble();
    int num_bins = params.find<int>("num_bins", 20);
    is_log_ = params.find<bool>("logarithmic", false);
    if (is_log_){
      min_val_ = log10(min_val_);
      max_val_ = log10(min_val_);
    }
    increment_ = (max_val_ - min_val_) / num_bins;
    counts_.resize(num_bins);
    fields_.reserve(num_bins + 2);
  }

  void addData_impl(BinType value, CountType count) override {
    value = is_log_ ? log10(value) : value;

    if (value > max_val_) return; //drop
    if (value < min_val_) return; //drop

    BinType delta = value - max_val_;
    int bin = delta / increment_;
    counts_[bin] += count;
  }

  void registerOutputFields(SST::Statistics::StatisticOutput* statOutput) override {
    int fid = 0;
    statOutput->outputField(fields_[fid++], int(counts_.size()));
    statOutput->outputField(fields_[fid++], bin_size_);
    for (auto cnt : counts_){
      statOutput->outputField(fields_[fid++], cnt);
    }
  }

  void outputStatisticData(SST::Statistics::StatisticOutput* statOutput, bool EndOfSimFlag) override {
    fields_.push_back(statOutput->registerField<int>("numBins"));
    fields_.push_back(statOutput->registerField<double>("binSize"));
    for (int i=0; i < counts_.size(); ++i){
      std::string name = sprockit::printf("bin%d", i);
      fields_.push_back(statOutput->registerField<uint64_t>(name.c_str()));
    }
  }

 private:
  std::vector<CountType> counts_;
  BinType min_val_;
  BinType max_val_;
  BinType bin_size_;
  BinType increment_;
  bool is_log_;

  std::vector<SST::Statistics::StatisticOutput::fieldHandle_t> fields_;

};

template <class BinType>
class SimpleStatHistogram : public Statistic<BinType> {
 public:
  SST_ELI_REGISTER_STATISTIC_TEMPLATE(
      SimpleStatHistogram,
      "macro",
      "simple_histogram",
      SST_ELI_ELEMENT_VERSION(1,0,0),
      "a histogram",
      "Statistic<Bin,Count>")


  SimpleStatHistogram(SST::BaseComponent* comp, const std::string& name,
                const std::string& subName, SST::Params& params) :
      Statistic<BinType>(comp, name, subName, params),
      hist_(comp, name, subName, params)
  {
  }

  void addData_impl(BinType value) override {
    hist_.addData_impl(value, 1);
  }

  void registerOutputFields(SST::Statistics::StatisticOutput* statOutput) override {
    hist_.registerOutputFields(statOutput);
  }

  void outputStatisticData(SST::Statistics::StatisticOutput* statOutput, bool EndOfSimFlag) override {
    hist_.outputStatisticData(statOutput, EndOfSimFlag);
  }

 private:
  StatHistogram<BinType,uint64_t> hist_;
};

SST_ELI_INSTANTIATE_MULTI_STATISTIC(StatHistogram, double, uint64_t)
SST_ELI_INSTANTIATE_MULTI_STATISTIC(StatHistogram, double, uint32_t)
SST_ELI_INSTANTIATE_STATISTIC(SimpleStatHistogram, double, double)
SST_ELI_INSTANTIATE_STATISTIC(SimpleStatHistogram, uint32_t, u32)
SST_ELI_INSTANTIATE_STATISTIC(SimpleStatHistogram, uint64_t, u64)

}

#endif // STAT_HISTOGRAM_H
