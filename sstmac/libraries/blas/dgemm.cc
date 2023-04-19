/**
Copyright 2009-2023 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2023, NTESS

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

#include <sstmac/libraries/blas/blas_api.h>
#include <sprockit/sim_parameters.h>
#include <algorithm>
#include <cmath>
#include <sstmac/software/libraries/compute/compute_event.h>

namespace sstmac {
namespace sw {

class DefaultDGEMM :
  public BlasKernel
{
 public:
  SST_ELI_REGISTER_DERIVED(
    BlasKernel,
    DefaultDGEMM,
    "macro",
    "default_dgemm",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "defaut DGEMM kernel")

  DefaultDGEMM(SST::Params& params){
    cache_size_bytes_ = params.find<SST::UnitAlgebra>("dgemm_cache_size", "32KB").getRoundedValue();
    loop_unroll_ = params.find<double>("dgemm_loop_unroll", 4);
    pipeline_ = params.find<double>("dgemm_pipeline_efficiency", 2);
  }

  std::string toString() const override {
    return "default dgemm";
  }

  ComputeEvent* op_3d(int m, int k, int n) override;

 protected:
  double loop_unroll_;
  double pipeline_;
  int cache_size_bytes_;

};

ComputeEvent*
DefaultDGEMM::op_3d(int mm, int nn, int kk)
{
  int sizes[3];
  sizes[0] = mm;
  sizes[1] = kk;
  sizes[2] = nn;
  std::sort(sizes, sizes + 3);

  int k = sizes[0];
  int n = sizes[1];
  int m = sizes[2];

  long Csize = m*n;
  long Asize = m*k;
  long Bsize = k*n;

  int npartitions = 1;
  int nblocks = 1;
  long sum_size = Asize + Bsize + Csize;
  //get to the point where all chunks fit in cache
  while ( (sum_size/nblocks) > cache_size_bytes_ )
  {
    ++npartitions;
    nblocks=npartitions*npartitions;
  }

  BasicComputeEvent* ev = new BasicComputeEvent;
  basic_instructions_st& st = ev->data();

  // a single block costs..
  long nops = long(m) * long(n) * long(k);
  // assume we are do a smart Strassen or something - gets better with size
  double exp = 2.807 / 3.0; // log2(7) / log2(8)
  nops = pow(nops, exp);
  st.flops = nops / long(pipeline_);
  st.intops = nops / long(loop_unroll_) / long(pipeline_);
  st.mem_sequential = Csize + Asize*npartitions + Bsize*npartitions;
  return ev;
}

}
}
