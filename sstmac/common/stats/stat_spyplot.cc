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

#include <sstmac/common/stats/stat_spyplot.h>
#include <sstmac/common/lodepng.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sprockit/output.h>
#include <sprockit/errors.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <algorithm>
#include <list>

namespace sstmac {

void
StatSpyplot::add_one(int source, int dest)
{
  add(source, dest, 1);
}

void
StatSpyplot::add(int source, int dest, long num)
{
  vals_[source][dest] += num;
  max_dest_ = std::max(max_dest_, dest);
}

void
StatSpyplot::reduce(StatCollector* coll)
{
  StatSpyplot* other = safe_cast(StatSpyplot, coll);
  for (auto& map_pair : other->vals_){
    int source = map_pair.first;
    auto& my_map = vals_[source];
    for (auto& val_pair : map_pair.second){
      int dest = val_pair.first;
      auto count = val_pair.second;
      max_dest_ = std::max(max_dest_, dest);
      my_map[dest] += count;
    }
  }
}

void
StatSpyplot::globalReduce(ParallelRuntime* rt)
{
  if (rt->nproc() == 1)
    return;

  int num_rows = max_dest_ + 1;
  num_rows = rt->globalMax(num_rows);
  int num_vals = num_rows * num_rows;
  uint64_t* vals = new uint64_t[num_vals];
  ::memset(vals, 0, num_vals*sizeof(uint64_t));

  for (auto& map_pair : vals_){
    int source = map_pair.first;
    for (auto& val_pair : map_pair.second){
      int dest = val_pair.first;
      auto idx = source * num_rows + dest;
      auto count = val_pair.second;
      vals[idx] = count;
    }
  }

  int root = 0;
  rt->globalSum(vals, num_vals, root);

  uint64_t* bufptr = vals;
  for (int i=0; i < num_rows; ++i){
    auto& my_map = vals_[i];
    for (int j=0; j < num_rows; ++j, ++bufptr){
      my_map[j] = *bufptr;
    }
  }

  delete[] vals;
}

void
StatSpyplot::clear()
{
  vals_.clear();
}

void
StatSpyplot::dumpLocalData()
{
  sprockit::abort("stat_spyplot::dump_local_data: not implemented");
}

void
StatSpyplot::dumpGlobalData()
{
  dumpToFile(fileroot_);
}

void
StatSpyplot::dumpToFile(const std::string& froot)
{
  std::string filename = froot + ".csv";
  std::fstream myfile;
  if (checkOpen(myfile, filename)) {
    std::list<long> nodes;

    {
      for (auto& pair : vals_) {
        nodes.push_back(pair.first);
      }
    }

    nodes.sort();

    for (auto nid : nodes) {
      auto& submap = vals_[nid];
      auto it2 = nodes.begin(), end2 = nodes.end();
      auto datapoint = submap[*it2];
      myfile << datapoint;
      ++it2;
      for (; it2 != end2; ++it2) {
        auto datapoint = submap[*it2];
        myfile << "," << datapoint;
      }
      myfile << "\n";
    }

    myfile.close();
  }
}


} //end namespace
