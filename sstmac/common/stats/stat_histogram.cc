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

StatHistogram::StatHistogram(SST::Params& params) :
    bin_size_(0),
    max_bin_(-1),
    is_log_(false),
  Statistic<double>(params)
{
  bin_size_ = params->get_quantity("bin_size");
  int num_bins_guess = params->get_optional_int_param("num_bins", 20);
  is_log_ = params->get_optional_bool_param("logarithmic", false);
  counts_.reserve(num_bins_guess);
}

void
StatHistogram::dump(const std::string& froot)
{
  std::string data_file = froot + ".dat";
  std::fstream data_str(data_file.c_str());
  data_str << "Bin Count\n";
  for (int i=0; i < counts_.size(); ++i){
    double size  = (i+0.5) * bin_size_;
    data_str << sprockit::printf("%12.8f", size) << " " << counts_[i] << "\n";
  }
  data_str.close();

  std::string gnuplot_file = froot + ".p";
  std::fstream gnuplot_str(gnuplot_file.c_str());

  gnuplot_str << "reset\n";
  gnuplot_str << "set term png truecolor\n";
  gnuplot_str << "set output \"" << froot << ".png\"\n";
  gnuplot_str << "set xlabel \"" << (is_log_ ? "log(Size)" : "Size") << "\"\n";
  gnuplot_str << "set ylabel \"Count\"\n";
  gnuplot_str << "set boxwidth 0.95 relative\n";
  gnuplot_str << "set style fill transparent solid 0.5 noborder\n";
  gnuplot_str << "plot \"" << data_file << "\"" << " u 1:2 w boxes notitle\n";
  gnuplot_str.close();
}

void
StatHistogram::addData_impl(double value)
{
  addData_impl(value, 1);
}

void
StatHistogram::addData_impl(double value, uint64_t count)
{
  value = is_log_ ? log10(value) : value;
  long bin = value / bin_size_;
  if (bin > max_bin_){
    max_bin_ = bin;
    if (max_bin_ > 1e6){
      spkt_abort_printf("Too many histogram bins. Collected value %12.6e is much larger"
                        " than the bin size of %12.6e",
                        value, bin_size_);
    }
    counts_.resize(max_bin_+1);
  }
  counts_[bin] += count;
}




}
