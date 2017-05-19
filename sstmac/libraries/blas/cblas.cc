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

#include <sstmac/libraries/blas/blas_api.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>


using namespace sstmac;
using namespace sstmac::sw;

static blas_api*
get_lib_blas()
{
  thread* t = operating_system::current_thread();
  return t->get_api<blas_api>();
}

extern "C" void
sstmac_simple_dgemm(int m, int n, int k)
{
  get_lib_blas()->dgemm(m, n, k);
}

extern "C" void
sstmac_dgemm(char *transa, char *transb, int* m, int* n, int* k, 
  double* alpha, double* a, int* lda, 
  double* b, int* ldb, double* beta, 
  double* c, int* ldc
)
{
  sstmac_simple_dgemm(*m, *n, *k);
}

extern "C" void
sstmac_simple_dgemv(int m, int n)
{
  get_lib_blas()->dgemv(m, n);
}

extern "C" void
sstmac_dgemv(char *transa, int* m, int* n,
  double* alpha, double* a, int* lda,
  double* x, int* incx, double* beta,
  double* y, int* incy
)
{
  sstmac_simple_dgemv(*m, *n);
}


extern "C" void
sstmac_simple_daxpy(int n)
{
  get_lib_blas()->daxpy(n);
}

extern "C" void
sstmac_daxpy(int* n,
  double* alpha,
  double* x, int* incx,
  double* y, int* incy
)
{
  sstmac_simple_daxpy(*n);
}

extern "C" void
sstmac_simple_ddot(int n)
{
  get_lib_blas()->ddot(n);
}

extern "C" void
sstmac_ddot(int* n,
  double* x, int* incx,
  double* y, int* incy)
{
  sstmac_simple_ddot(*n);
}