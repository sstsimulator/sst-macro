/**
Copyright 2009-2021 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2021, NTESS

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
#include <sstmac/libraries/pthread/sstmac_pthread_impl.h>
#include <sstmac/software/api/api.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/process/app.h>
#include <sstmac/hardware/processor/processor.h>
#include <sstmac/replacements/omp.h>

#undef omp_init_lock
#undef omp_destroy_lock
#undef omp_set_lock
#undef omp_unset_lock
#undef omp_test_lock
#undef omp_get_num_threads
#undef omp_get_thread_num
#undef omp_get_max_threads
#undef omp_get_wtime
#undef omp_get_num_procs
#undef omp_set_num_threads
#undef omp_in_parallel
#undef omp_get_level
#undef omp_get_ancestor_thread_num

namespace sstmac {
namespace sw {

extern "C"
void sstmac_omp_init_lock(sstmac_omp_lock_t *lock)
{
  SSTMAC_pthread_mutex_init(lock, nullptr);
}

extern "C"
void sstmac_omp_destroy_lock(sstmac_omp_lock_t *lock)
{
  SSTMAC_pthread_mutex_destroy(lock);
}

extern "C"
void sstmac_omp_set_lock(sstmac_omp_lock_t *lock)
{
  SSTMAC_pthread_mutex_lock(lock);
}

extern "C"
void sstmac_omp_unset_lock(sstmac_omp_lock_t *lock)
{
  SSTMAC_pthread_mutex_unlock(lock);
}

extern "C"
int sstmac_omp_test_lock(sstmac_omp_lock_t *lock)
{
  return SSTMAC_pthread_mutex_trylock(lock);
}

extern "C"
double sstmac_omp_get_wtime(){
  return sstmac::sw::OperatingSystem::currentOs()->now().sec();
}

extern "C"
int sstmac_omp_get_thread_num(){
  sstmac::sw::Thread* t = sstmac::sw::OperatingSystem::currentThread();
  return t->ompGetThreadNum();
}

extern "C"
int sstmac_omp_get_num_threads(){
  sstmac::sw::Thread* t = sstmac::sw::OperatingSystem::currentThread();
  return t->ompGetNumThreads();
}

extern "C"
int sstmac_omp_get_max_threads(){
  sstmac::sw::Thread* t = sstmac::sw::OperatingSystem::currentThread();
  //for now, just return number of cores
  return t->ompGetMaxThreads();
}

extern "C"
int sstmac_omp_get_num_procs()
{
  sstmac::sw::OperatingSystem* os = sstmac::sw::OperatingSystem::currentOs();
  return os->node()->proc()->ncores();
}

extern "C"
void sstmac_omp_set_num_threads(int nthr)
{
  sstmac::sw::Thread* t = sstmac::sw::OperatingSystem::currentThread();
  t->ompSetNumThreads(nthr);
}

extern "C"
int sstmac_omp_in_parallel()
{
  sstmac::sw::Thread* t = sstmac::sw::OperatingSystem::currentThread();
  return t->ompInParallel();
}

extern "C"
int sstmac_omp_get_level()
{
  sstmac::sw::Thread* t = sstmac::sw::OperatingSystem::currentThread();
  return t->ompGetLevel();
}

extern "C"
int sstmac_omp_get_ancestor_thread_num()
{
  sstmac::sw::Thread* t = sstmac::sw::OperatingSystem::currentThread();
  return t->ompGetAncestorThreadNum();
}

}
}
