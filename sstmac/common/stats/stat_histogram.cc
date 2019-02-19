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

#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/common/stats/stat_histogram.h>
#include <sstmac/common/event_scheduler.h>
#include <sprockit/spkt_string.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <math.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
{ "bin_size", "the size of each bin in the histogram" },
{ "num_bins", "the number of bins to include in the histogram" },
{ "logarithmic", "whether to track bins logarithmically" },
);

namespace sstmac {

StatHistogram::StatHistogram(SST::Params& params, SST::BaseComponent* comp,
                             const std::string& name, const std::string& subName) :
    bin_size_(0),
    is_log_(false),
  SST::Statistics::MultiStatistic<double,uint64_t>(comp, name, subName, params)
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

void
StatHistogram::outputStatisticData(SST::Statistics::StatisticOutput* statOutput, bool EndOfSimFlag)
{
  int fid = 0;
  statOutput->outputField(fields_[fid++], int(counts_.size()));
  statOutput->outputField(fields_[fid++], bin_size_);
  for (auto cnt : counts_){
    statOutput->outputField(fields_[fid++], cnt);
  }
}

void
StatHistogram::registerOutputFields(SST::Statistics::StatisticOutput *statOutput)
{
  fields_.push_back(statOutput->registerField<int>("numBins"));
  fields_.push_back(statOutput->registerField<double>("binSize"));
  for (int i=0; i < counts_.size(); ++i){
    std::string name = sprockit::printf("bin%d", i);
    fields_.push_back(statOutput->registerField<uint64_t>(name.c_str()));
  }
}

void
StatHistogram::addData_impl(double value, uint64_t count)
{
  value = is_log_ ? log10(value) : value;

  if (value > max_val_) return; //drop
  if (value < min_val_) return; //drop

  double delta = value - max_val_;
  int bin = delta / increment_;
  counts_[bin] += count;
}




}
