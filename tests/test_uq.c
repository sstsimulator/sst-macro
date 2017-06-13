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

#include <sstmac/libraries/uq/uq.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define debug_print(...) printf(__VA_ARGS__)
//#define debug_print(...) 

int main(int argc, char** argv)
{
  int nproc = 64;
  int npartners = 4;
  int niterations = 2;
  int nresults = nproc*npartners*niterations;
  int nparams = 2; 
  int njobs = 4;
  const char* param_names[] = { 
    "injection_bandwidth", 
    "network_bandwidth",
  };

  double inj_bws[] = { 1.0, 1.0 }; int nInj = sizeof(inj_bws) / sizeof(double);
  double net_bws[] = { 1.0, 1.0 }; int nNet = sizeof(net_bws) / sizeof(double);
  const char* units[] = { "GB/s", "GB/s" };

  int worker_id;
  void* queue = sstmac_uq_init(argc, argv, &worker_id);

  double** param_values = allocate_values(queue, njobs, nparams);
  double** results = allocate_results(queue, njobs, nresults);

  int job = 0;
  int i,j;
  for (i=0; i < nInj; ++i){
    for (j=0; j < nNet; ++j, ++job){
      param_values[job][0] = inj_bws[i];
      param_values[job][1] = net_bws[j];
    }
  }

  int max_nthread = 1;
  sstmac_uq_run_units(queue,
    njobs, nparams, nresults, max_nthread,
    param_names, param_values, units,
    results, Fork);

  for (job=0; job < njobs; ++job){
    debug_print("Job %d: {\n", job);
    for (i=0; i < nresults; ++i){
      double bw = results[job][i];
      debug_print("   %12.8fGB/s\n", bw);
      if (bw < 0.1 || bw > 1.0){
        printf("UQ test failed: got invalid bandwidths\n");
        return 1;
      }
    }
    debug_print("}\n");
  }
  printf("UQ test passed: got all valid bandwidths\n");

  return 0;
}