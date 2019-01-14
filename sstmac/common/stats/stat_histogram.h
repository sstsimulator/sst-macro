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

#ifndef STAT_HISTOGRAM_H
#define STAT_HISTOGRAM_H

#include <sstmac/common/stats/stat_collector.h>
#include <vector>

namespace sstmac {

class StatHistogram :
  public StatCollector
{
  FactoryRegister("histogram", StatCollector, StatHistogram)
 public:
  StatHistogram(sprockit::sim_parameters* params);

  std::string toString() const override {
    return "stat histogram";
  }

  void collect(double value);

  void collect(double value, int64_t num);

  void dumpLocalData() override;

  void dumpGlobalData() override;

  void globalReduce(ParallelRuntime *rt) override;

  void clear() override;

  void reduce(StatCollector* coll) override;

  StatCollector* doClone(sprockit::sim_parameters* params) const override {
    return new StatHistogram(params);
  }

 protected:
  void dump(const std::string& froot);

 protected:
  std::vector<int64_t> counts_;

  double bin_size_;

  int64_t max_bin_;

  bool is_log_;

};

class StatTimeHistogram : public StatHistogram
{
  FactoryRegister("time_histogram", StatHistogram, StatTimeHistogram)
 public:
  StatTimeHistogram(sprockit::sim_parameters* params) :
    StatHistogram(params)
  {
  }

  void record(Timestamp t, int64_t num);
};

}

#endif // STAT_HISTOGRAM_H
