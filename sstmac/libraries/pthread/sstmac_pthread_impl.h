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

#ifndef SSTMAC_SOFTWARE_PROCESS_PTHREAD_IMPL_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_PTHREAD_IMPL_H_INCLUDED

#include <stdlib.h>
#include <stdint.h>

//these have to be macros, to avoid pthread conflicts
#define sstmac_pthread_t int
#define sstmac_pthread_cond_t int
#define sstmac_pthread_mutex_t int
#define sstmac_pthread_spinlock_t int
#define sstmac_pthread_once_t int
#define sstmac_pthread_cond_attr int
#define sstmac_pthread_mutexattr_t int

typedef int sstmac_pthread_key_t;
typedef void (*sstmac_pthread_key_destructor_fxn)(void*);

#define SSTMAC_PTHREAD_ONCE_INIT 0
#define SSTMAC_PTHREAD_MUTEX_INITIALIZER -1
#define SSTMAC_PTHREAD_COND_INITIALIZER -1

#define SSTMAC_PTHREAD_THREADS_MAX 1000
#define SSTMAC_PTHREAD_KEYS_MAX 10000
#define SSTMAC_PTHREAD_STACK_MIN 16384
#define SSTAMC_PTHREAD_CREATE_DETACHED 0
#define SSTMAC_PTHREAD_CREATE_JOINABLE 1

#define PTHREAD_MUTEX_NORMAL     0
#define PTHREAD_MUTEX_ERRORCHECK 1
#define PTHREAD_MUTEX_RECURSIVE  2


enum {
 SSTMAC_PTHREAD_SCOPE_PROCESS,
 SSTMAC_PTHREAD_SCOPE_SYSTEM
};

#include <sstmac/libraries/pthread/sstmac_cpu_set.h>
#include <sstmac/software/process/time.h>

#ifdef __cplusplus
#include <cstring>
extern "C"
{
#endif

typedef struct {
  uint64_t cpumask;
} sstmac_pthread_attr_t;

int
SSTMAC_pthread_create(sstmac_pthread_t * thread,
                      const sstmac_pthread_attr_t * attr, void *
                      (*start_routine)(void *), void * arg);

void
SSTMAC_pthread_exit(void *retval);

int
SSTMAC_pthread_join(sstmac_pthread_t thread, void ** status);

sstmac_pthread_t
SSTMAC_pthread_self();

int
SSTMAC_pthread_equal(sstmac_pthread_t thread_1, sstmac_pthread_t thread_2);

int
SSTMAC_pthread_mutex_init(sstmac_pthread_mutex_t * mutex,
                          const sstmac_pthread_mutexattr_t *attr);

int
SSTMAC_pthread_attr_setaffinity_np(sstmac_pthread_attr_t *attr, size_t cpusetsize, const sstmac_cpu_set_t *cpuset);

int
SSTMAC_pthread_attr_getaffinity_np(sstmac_pthread_attr_t attr, size_t cpusetsize, sstmac_cpu_set_t *cpuset);

int
SSTMAC_pthread_mutexattr_init(sstmac_pthread_mutexattr_t *attr);

int
SSTMAC_pthread_mutexattr_destroy(sstmac_pthread_mutexattr_t *attr);

int
SSTMAC_pthread_attr_getstack(sstmac_pthread_attr_t* attr, void** stack, size_t* stacksize);

int
SSTMAC_pthread_kill(sstmac_pthread_t thr, int signal);

int
SSTMAC_pthread_yield();

int
SSTMAC_pthread_mutex_destroy(sstmac_pthread_mutex_t * mutex);

int
SSTMAC_pthread_mutex_lock(sstmac_pthread_mutex_t * mutex);

int
SSTMAC_pthread_mutex_trylock(sstmac_pthread_mutex_t * mutex);

int
SSTMAC_pthread_mutex_unlock(sstmac_pthread_mutex_t * mutex);

int 
SSTMAC_pthread_spin_init(sstmac_pthread_spinlock_t* lock, int pshared);

int
SSTMAC_pthread_spin_destroy(sstmac_pthread_spinlock_t* lock);

int
SSTMAC_pthread_spin_trylock(sstmac_pthread_spinlock_t* lock);

int
SSTMAC_pthread_spin_lock(sstmac_pthread_spinlock_t* lock);

int
SSTMAC_pthread_spin_unlock(sstmac_pthread_spinlock_t* lock);

int
SSTMAC_pthread_cond_init(sstmac_pthread_cond_t * cond,
                         const sstmac_pthread_cond_attr *attr);

int
SSTMAC_pthread_cond_destroy(sstmac_pthread_cond_t * cond);

int
SSTMAC_pthread_cond_wait(sstmac_pthread_cond_t * cond,
                         sstmac_pthread_mutex_t * mutex);

int
SSTMAC_pthread_cond_timedwait(sstmac_pthread_cond_t * cond,
                              sstmac_pthread_mutex_t * mutex, const struct timespec * abstime);

int
SSTMAC_pthread_cond_signal(sstmac_pthread_cond_t * cond);

int
SSTMAC_pthread_cond_broadcast(sstmac_pthread_cond_t * cond);

int
SSTMAC_pthread_once(sstmac_pthread_once_t * once_init, void
                    (*init_routine)(void));

int
SSTMAC_pthread_key_create(sstmac_pthread_key_t * key, void
                          (* dest_routine)(void *));

int
SSTMAC_pthread_key_delete(sstmac_pthread_key_t key);

int
SSTMAC_pthread_setspecific(sstmac_pthread_key_t key, const void * pointer);

void *
SSTMAC_pthread_getspecific(sstmac_pthread_key_t key);

void
SSTMAC_pthread_cleanup_push(void
                            (*routine)(void *), void *routine_arg);

void
SSTMAC_pthread_cleanup_pop(int execute);

int
SSTMAC_pthread_attr_init(sstmac_pthread_attr_t *attr);

int
SSTMAC_pthread_attr_destroy(sstmac_pthread_attr_t *attr);

int
SSTMAC_pthread_attr_getstacksize(sstmac_pthread_attr_t *attr,
                                 size_t * stacksize);

int
SSTMAC_pthread_attr_setstacksize(sstmac_pthread_attr_t *attr, size_t stacksize);

int
SSTMAC_pthread_attr_getdetachstate(const sstmac_pthread_attr_t *attr,
                                   int *state);

int
SSTMAC_pthread_attr_setdetachstate(sstmac_pthread_attr_t *attr, int state);

int
SSTMAC_pthread_attr_setscope(sstmac_pthread_attr_t*, int scope);

int
SSTMAC_pthread_attr_getscope(sstmac_pthread_attr_t*, int* scope);

int
SSTMAC_pthread_detach(sstmac_pthread_t);

#ifdef __cplusplus
}
#endif

#endif