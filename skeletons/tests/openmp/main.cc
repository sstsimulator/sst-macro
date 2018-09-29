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

#include <stddef.h>
#include <stdio.h>
#include <sys/time.h>

#define sstmac_app_name runopenmp

double get_time()
{
  timeval t_st;
  gettimeofday(&t_st, 0);
  double t = t_st.tv_sec + 1e-6 * t_st.tv_usec;
  return t;
}

int main(int argc, char** argv)
{
  int worksize = 100000;
  double* array = new double[worksize];

  printf("Starting work at   T=%8.5f\n", get_time());
#pragma omp parallel for num_threads(1)
  for (int i=0; i < worksize; ++i){
    array[i] = array[i/2]*array[1/4] + 5*array[i];
  }

  printf("Continuing work at T=%8.5f\n", get_time());
#pragma omp parallel for 
  for (int i=0; i < worksize; ++i){
    array[i] = array[i/2]*array[1/4] + 5*array[i];
  }

  printf("Continuing work at T=%8.5f\n", get_time());
#pragma omp parallel for num_threads(4)
  for (int i=0; i < worksize; ++i){
    array[i] = array[i/2]*array[1/4] + 5*array[i];
  }

  printf("Finishing work at  T=%8.5f\n", get_time());
  return 0;
}

