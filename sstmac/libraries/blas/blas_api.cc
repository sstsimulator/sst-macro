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

#include <sstmac/libraries/blas/blas_api.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/libraries/compute/lib_compute_inst.h>
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sprockit/keyword_registration.h>


RegisterKeywords(
  { "dgemm", "" },
  { "dgemv", "" },
  { "daxpy", "" },
  { "ddot", "" },
  { "daxpy_loop_unroll", "" },
  { "daxpy_pipeline_efficiency", "" },
  { "ddot_loop_unroll", "" },
  { "ddot_pipeline_efficiency", "" },
  { "dgemm_loop_unroll", "" },
  { "dgemm_pipeline_efficiency", "" },
  { "dgemm_cache_size", "" },
  { "dgemv_loop_unroll", "" },
  { "dgemv_pipeline_efficiency", "" },
);

#define enumcase(x) case x: return #x;

RegisterDebugSlot(blas);

namespace sstmac {
namespace sw {

BlasKernel* BlasAPI::dgemm_kernel_;
BlasKernel* BlasAPI::dgemv_kernel_;
BlasKernel* BlasAPI::daxpy_kernel_;
BlasKernel* BlasAPI::ddot_kernel_;

BlasAPI::BlasAPI(SST::Params& params, App* app, SST::Component* comp)
  : API(params, app, comp)
{
  std::string libname = sprockit::printf("blas-compute%d", sid().toString().c_str());
  lib_compute_ = new LibComputeInst(params, libname, sid(), app->os());
  if (!dgemm_kernel_){
    initKernels(params);
  }
}

BlasAPI::~BlasAPI()
{
}

void
BlasAPI::initKernels(SST::Params& params)
{
  auto* lib = BlasKernel::getBuilderLibrary("macro");
  dgemm_kernel_ = lib->getBuilder(params.find<std::string>("dgemm", "default_dgemm"))->create(params);
  dgemv_kernel_ = lib->getBuilder(params.find<std::string>("dgemv", "default_dgemv"))->create(params);
  daxpy_kernel_ = lib->getBuilder(params.find<std::string>("daxpy", "default_daxpy"))->create(params);
  ddot_kernel_ = lib->getBuilder(params.find<std::string>("ddot", "default_ddot"))->create(params);
}

void
BlasAPI::ddot(int n)
{
  ComputeEvent* msg = ddot_kernel_->op_1d(n);
  lib_compute_->computeInst(msg);
  //msg is done
  delete msg;
}

void
BlasAPI::dgemm(int m, int n, int k)
{
  ComputeEvent* msg = dgemm_kernel_->op_3d(m, n, k);
  lib_compute_->computeInst(msg);
  delete msg;
}

void
BlasAPI::dgemv(int m, int n)
{
  ComputeEvent* msg = dgemv_kernel_->op_2d(m,n);
  lib_compute_->computeInst(msg);
  delete msg;
}

void
BlasAPI::daxpy(int n)
{
  ComputeEvent* msg = daxpy_kernel_->op_1d(n);
  lib_compute_->computeInst(msg);
  delete msg;
}

ComputeEvent*
BlasKernel::op_3d(int m, int k, int n)
{
  spkt_throw_printf(sprockit::UnimplementedError,
    "blas_kernel::mult_op: %s does not implement 3D ops",
    toString().c_str());
}

ComputeEvent*
BlasKernel::op_2d(int m, int n)
{
  spkt_throw_printf(sprockit::UnimplementedError,
    "blas_kernel::mult_op: %s does not implement 2D ops",
    toString().c_str());
}

ComputeEvent*
BlasKernel::op_1d(int n)
{
  spkt_throw_printf(sprockit::UnimplementedError,
    "blas_kernel::mult_op: %s does not implement 1D ops",
    toString().c_str());
}


}
}
