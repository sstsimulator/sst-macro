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

#include <mpi.h>
#include <pthread.h>
#include <cmath>
#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <algorithm>
#include <sstmac/util.h>
#include <sstmac/software/process/thread.h>

static constexpr double cycle_time = 1e-8;

struct cfg {
  int niter;
  int nwarp;
  int nx;
  int minLatency;
  double spreadFactor;
  int nicCycles;
};

struct thread_cfg {
  cfg* c;
  int id;
  uint64_t instrCount;
};

int cyclesToCompute(int n)
{
  //use a geometric series probability
  //to randomly predict the number of cycles until a stall
  double oneMr = 1.0/n;
  double r = 1.0 - oneMr;
  double cdf = (rand() % 1000) / 1000.;
  double expN = log(1.0-cdf) / log(r);
  return expN;
}

int cyclesToGet(int minLatency, double spreadFactor)
{
  double oo_lambda = minLatency*spreadFactor;
  double cdf = (rand() % 1000) / 1000.;
  double x = -log(1.0 - cdf) * oo_lambda;
  return x + minLatency;
}

void* run_warp(void* args)
{
  thread_cfg* tc = (thread_cfg*) args;
  cfg* c = tc->c;
  tc->instrCount = 0;
  auto* thr = sstmac::sw::Thread::current();

  thr->zeroAffinity();
  thr->addAffinity(0);
  uint64_t comp_mask = thr->cpumask();

  thr->zeroAffinity();
  for (int i=1; i <= c->nwarp; ++i){
    thr->addAffinity(i);
  }
  uint64_t comm_mask = thr->cpumask();

  for (int i=0; i < c->niter; ++i){
    int compCycles = cyclesToCompute(c->nx);
    int idleCycles = cyclesToGet(c->minLatency, c->spreadFactor);
    //printf("Thread %d running %d active, %d inactive on iter %d, cycle %llu\n", 
    //       tc->id, compCycles, idleCycles, i, uint64_t(sstmac_now()/cycle_time));
    //fflush(stdout);
    if (compCycles){
      thr->setCpumask(comp_mask);
      sstmac_compute(compCycles*cycle_time);
    }
    if (idleCycles){
      thr->setCpumask(comm_mask);
      if (c->nicCycles) sstmac_compute(c->nicCycles*cycle_time);
      sstmac_sleep_precise(idleCycles*cycle_time);
    }
    tc->instrCount += compCycles;
  }
  return nullptr;
}

int main(int argc, char** argv)
{
  MPI_Init(&argc, &argv);
  int rank; MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  srand(time(NULL));

  cfg c;

  double start = sstmac_now();

  if (argc < 7){
    std::cerr << "Insufficient args: need <niter> <nwarp> <nx> <minLat> <spread> <nicLat>" << std::endl;
    return 0;
  }

  c.niter = atoi(argv[1]);
  c.nwarp = atoi(argv[2]);
  c.nx = atoi(argv[3]);
  c.minLatency = atoi(argv[4]);
  c.spreadFactor = atof(argv[5]);
  c.nicCycles = atoi(argv[6]);

  pthread_t* threads = new pthread_t[c.nwarp];
  thread_cfg* thr_cfgs = new thread_cfg[c.nwarp];
  for (int t=1; t < c.nwarp; ++t){
    thr_cfgs[t].id = t;
    thr_cfgs[t].c = &c;
    pthread_create(&threads[t], nullptr, &run_warp, &thr_cfgs[t]);
  }

  thr_cfgs[0].id = 0;
  thr_cfgs[0].c = &c;
  run_warp(&thr_cfgs[0]);

  uint64_t totalInstr = thr_cfgs[0].instrCount;
  for (int t=1; t < c.nwarp; ++t){
    void* ret;
    pthread_join(threads[t], &ret);
    totalInstr += thr_cfgs[t].instrCount;
  }


  MPI_Barrier(MPI_COMM_WORLD);
  double stop = sstmac_now();
  double cycles = ((stop-start)/cycle_time);
  double cpi = cycles / totalInstr;
  if (rank == 0){
    printf("Got CPI=%12.8f\n", cpi);
  }

  MPI_Finalize();
  return 0;
}

