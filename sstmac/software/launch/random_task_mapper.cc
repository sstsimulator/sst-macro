/**
Copyright 2009-2022 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2022, NTESS

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

#include <sstmac/software/launch/random_task_mapper.h>
#include <sprockit/errors.h>
#include <sprockit/sim_parameters.h>
#include <algorithm>
#include <stdlib.h>

namespace sstmac {
namespace sw {

RandomTaskMapper::RandomTaskMapper(SST::Params& params) :
  TaskMapper(params)
{
  if(params.contains("random_indexer_seed")) {
    long seed = params.find<long>("random_indexer_seed");
    rng_ = RNG::SimpleCombo::construct(seed);
  } else {
    rng_ = RNG::SimpleCombo::construct();
  }
  smp_ = params.find<bool>("smp_allocation", false);
}

RandomTaskMapper::~RandomTaskMapper() throw ()
{
}

void
RandomTaskMapper::mapRanks(
  const ordered_node_set& nodes,
  int ppn,
  std::vector<NodeId> &result,
  int nproc)
{
  nproc = validateNproc(ppn, nodes.size(), nproc, "RandomTaskMapper");
  result.resize(nproc);
  RNG::UniformInteger_functor rngf(rng_);
  if (smp_){
    //keep contigous ranks on the same node for SMP
    //but randomize the nodes themselves
    std::vector<NodeId> smp_nodes(nodes.begin(), nodes.end());
    RNG::random_shuffle(smp_nodes.begin(), smp_nodes.end(), rngf);
    for (int nodeIdx=0; nodeIdx < smp_nodes.size(); ++nodeIdx){
      int rankOffset = nodeIdx * ppn;
      int rankStop = std::min(rankOffset+ppn,nproc);
      for (int rank=rankOffset; rank < rankStop; ++rank){
        result[rank] = smp_nodes[nodeIdx];
      }
    }
  } else {
    //totally random - even if there are multiple ranks
    //per node, scramble them anywhere
    int nodeIdx = 0;
    for (auto nid : nodes){
      int rankOffset = nodeIdx * ppn;
      int rankStop = std::min(rankOffset+ppn, nproc);
      for (int rank=rankOffset; rank < rankStop; ++rank){
        result[rank] = nid;
      }
      ++nodeIdx;
    }

    RNG::random_shuffle(result.begin(), result.end(), rngf);
  }
}

}
} // end of namespace sstmac
