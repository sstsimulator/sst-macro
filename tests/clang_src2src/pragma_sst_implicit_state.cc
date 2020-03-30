/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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

enum ImplicitStates {
  null=0,
  length,
  size,
  temp
};
 

#pragma sst memoize skeletonize(false) model(linear) inputs(niter)
void memoFxn(int niter);

void memoFxn(int n){
  for (int i=0; i < n; ++i);
}

#pragma sst implicit_state size(param) temp(param)
void testFxn(int param){
}

#pragma sst implicit_state size(param) temp(param)
void anotherTestFxn(int param){
  param += 5;
}

int fxn()
{
  int i=0;
  int mul = 0;
  double* x = new double[10];
  int* idx = new int[5];
#pragma sst memoize skeletonize(true) inputs(5) model(linear) name(forloop)
  for (i=0; i < 5; ++i){
    mul *= i;
    for (int j=0; j < 10; ++j){
      mul += (j-1);
      x[j] += i;
      mul -= x[j];
      j=7;
      mul += x[j];
      mul *= x[idx[i]];
      idx[i] -= 3;
      mul *= x[idx[i]];
    }
  }
#pragma sst implicit_state temp(mul)
  memoFxn(10);
#pragma sst implicit_state length(5)
  memoFxn(i+12);
  return 0;
}


