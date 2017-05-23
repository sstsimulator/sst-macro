/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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
#include <sstmac/common/stats/stat_local_double.h>
#include <sprockit/spkt_string.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <math.h>

namespace sstmac {

stat_local_double::stat_local_double(sprockit::sim_parameters* params) :
    stat_value<double>(params)
{
}

void
stat_local_double::global_reduce(parallel_runtime *rt)
{
  if (rt->nproc() == 1)
    return;
  spkt_throw(sprockit::unimplemented_error, "stat_local_double::global_reduce");
}

void
stat_local_double::reduce(stat_collector *coll)
{
  stat_local_double* other = safe_cast(stat_local_double, coll);
  //std::cerr << "id: " << other->id_ << "\n";
  //if (other->id_ < 0) return;
  if ((other->id_ + 1) > values_.size()) {
    values_.resize(other->id_ + 1);
  }
  values_[other->id_] = other->value_;
}

void
stat_local_double::dump(const std::string& froot)
{
  std::string data_file = froot + ".dat";
  std::fstream data_str;
  check_open(data_str, data_file);
  data_str << "Id Value\n";
  for (int i=0; i < values_.size(); ++i)
    data_str << sprockit::printf("%i %lf\n", i, values_[i]);
  data_str.close();
}

void
stat_local_double::dump_global_data()
{
  dump(fileroot_);
}

void
stat_local_double::dump_local_data()
{
  std::string fname = sprockit::printf("%s.%d", fileroot_.c_str(), id_);
  dump(fname);
}

void
stat_local_double::clear()
{
}

void
stat_local_double::simulation_finished(timestamp end)
{
}




}