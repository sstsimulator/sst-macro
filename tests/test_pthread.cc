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

#include <sprockit/test/test.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/libraries/pthread/sstmac_pthread.h>
#include <sstmac/skeleton.h>
#include <sstmac/compute.h>

using namespace sstmac;
using namespace sstmac::sw;
using namespace sstmac::hw;

extern "C" int ubuntu_cant_name_mangle() { return 0; }

void* ptest(void* args)
{
   SSTMAC_compute(1); 
   printf("Yes, I reach here!\n");
   return 0;
}

void* ptest3(void* args)
{
   SSTMAC_compute(1);
   printf("No, I do not reach here!\n");
   return 0;
}

struct pthread_args
{
  pthread_cond_t cond;
  pthread_mutex_t mutex;
};

void* ptest2(void* args)
{
  int status;
  pthread_args* pargs = (pthread_args*) args;
  SSTMAC_compute(0.001);
  status = pthread_mutex_lock(&pargs->mutex);
  if (status != 0){
    spkt_throw(sprockit::illformed_error,
        "mutex failed lock");
  }
  std::cout << "Mutex locked" << std::endl;
  SSTMAC_compute(0.001);
  std::cout << "Mutex unlocked" << std::endl;
  pthread_mutex_unlock(&pargs->mutex);
  status = pthread_mutex_lock(&pargs->mutex);
  if (status != 0){
    spkt_throw(sprockit::illformed_error,
        "mutex failed lock");
  }
  std::cout << "Condition locked" << std::endl;
  SSTMAC_compute(0.001);
  status = pthread_cond_wait(&pargs->cond, &pargs->mutex);
  if (status != 0){
    spkt_throw(sprockit::illformed_error,
        "thread failed wait");
  }
  std::cout << "Done waiting" << std::endl;
  SSTMAC_compute(0.001);

  return 0;
}

#define sstmac_app_name test_pthread


int USER_MAIN(int argc, char** argv)
{
    void* no_args = 0;
    pthread_t thr1, thr2;
    pthread_attr_t attr1, attr2;
    int status;
    status = pthread_create(&thr1, &attr1, &ptest, no_args);
    status = pthread_create(&thr2, &attr2, &ptest, no_args);

    void* ret;
    pthread_join(thr1, &ret);
    pthread_join(thr2, &ret);

    pthread_args pargs;
    pthread_mutex_init(&pargs.mutex,0);
    pthread_cond_init(&pargs.cond,0);
    status = pthread_create(&thr1, &attr1, &ptest2, &pargs);
    status = pthread_create(&thr2, &attr2, &ptest2, &pargs);

    std::cout << "Spawned threads" << std::endl;
    SSTMAC_compute(1);

    std::cout << "First signal" << std::endl;
    pthread_cond_signal(&pargs.cond);
    std::cout << "Second signal" << std::endl;
    pthread_cond_signal(&pargs.cond);

    pthread_join(thr1, &ret);
    pthread_join(thr2, &ret);

    //spin off another pthread
    status = pthread_create(&thr1, &attr1, &ptest, &pargs);

    return 0;
}