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

#include <sched.h>
#include <pthread.h>

void*
pthread_run(void* args){
  int thr = *((int*)args);
  sstmac_compute(1);
  printf("Finishing compute at T=%8.4f on thread %d\n",
         sstmac_now(), thr);
  return 0;
}

#define sstmac_app_name user_app_cxx

int main(int argc, char** argv)
{
  int nthread = 10;
  pthread_attr_t* attrs = new pthread_attr_t[nthread];
  for (int i=0; i < nthread; ++i){
    pthread_attr_init(&attrs[i]);
  }
  
  pthread_t* threads = new pthread_t[nthread];
  int num_on_core = 4;
  int thread_id = 0;
  int core = 0;

  //assign 4->0, 3-1, 2->2, 1->3 for a total of ten threads
  while (thread_id < nthread){
    for (int i=0; i < num_on_core; ++i, ++thread_id){
      cpu_set_t set;
      CPU_ZERO(&set);
      CPU_SET(core, &set);
      pthread_attr_setaffinity_np(&attrs[thread_id], sizeof(cpu_set_t),  &set);
    }
    ++core;
    --num_on_core;
  }
  
  void* args = 0;
  int* thread_ids = new int[nthread];
  for (int i=0; i < nthread; ++i){
    thread_ids[i] = i;
    pthread_create(&threads[i], &attrs[i], pthread_run, &thread_ids[i]);
  }
  for (int i=0; i < nthread; ++i){
    pthread_join(threads[i], &args);
  }

  return 0;
}