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

#include <sstmac/software/launch/random_allocation.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <algorithm>

RegisterKeywords(
{ "random_allocation_seed", "seed for random number generator" },
{ "random_indexer_seed", "seed for random number generator" },
);

namespace sstmac {
namespace sw {

random_allocation::~random_allocation() throw ()
{
}

random_allocation::random_allocation(sprockit::sim_parameters *params) :
  node_allocator(params)
{
  int seed = params->get_optional_int_param("random_allocation_seed", -1);
  if (seed == -1){
    seed = time(NULL);
  }
  rng_ = RNG::SimpleCombo::construct(seed);
}

bool
random_allocation::allocate(
  int nnode_requested,
  const ordered_node_set& available,
  ordered_node_set& allocation) const
{
  if (available.size() < nnode_requested){
    return false;
  }

  std::vector<node_id> availvec(available.size());
  std::copy(available.begin(), available.end(), availvec.begin());
  RNG::UniformInteger_functor rngf(rng_);
  std::random_shuffle(availvec.begin(), availvec.end(), rngf);
  for (int i = 0; i < nnode_requested; i++) {
    node_id node = availvec[i];
    allocation.insert(node);
  }

  return true;
}

} //end namespace sw
}
