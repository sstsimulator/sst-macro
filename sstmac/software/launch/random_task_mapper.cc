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

#include <sstmac/software/launch/random_task_mapper.h>
#include <sprockit/errors.h>
#include <sprockit/sim_parameters.h>
#include <algorithm>
#include <stdlib.h>

namespace sstmac {
namespace sw {

random_task_mapper::random_task_mapper(sprockit::sim_parameters *params) :
  task_mapper(params)
{
  if(params->has_param("random_indexer_seed")) {
    long seed = params->get_long_param("random_indexer_seed");
    rng_ = RNG::SimpleCombo::construct(seed);
  }
  else {
    rng_ = RNG::SimpleCombo::construct();
  }
}

random_task_mapper::~random_task_mapper() throw ()
{
}

void
random_task_mapper::map_ranks(
  const ordered_node_set& nodes,
  int ppn,
  std::vector<node_id> &result,
  int nproc)
{
  nproc = validate_nproc(ppn, nodes.size(), nproc, "randomindexing");

  ordered_node_set::iterator iter = nodes.begin();
  for(long i = 0; i < nproc / ppn; i++) {
    iter++;
  }

  // result = nodes;
  for (int i = 0; i < ppn; ++i) {
    result.insert(result.end(), nodes.begin(), iter);
  }

  RNG::UniformInteger_functor rngf(rng_);
  std::random_shuffle(result.begin(), result.end(), rngf);

  result.resize(nproc);
}

}
} // end of namespace sstmac