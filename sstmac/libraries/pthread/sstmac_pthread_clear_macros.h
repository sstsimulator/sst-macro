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

#undef pthread_create
#undef pthread_join
#undef pthread_tryjoin_np
#undef pthread_timedjoin_np
#undef pthread_detach
#undef pthread_equal
#undef pthread_attr_init
#undef pthread_attr_destroy
#undef pthread_attr_getdetachstate
#undef pthread_attr_setdetachstate
#undef pthread_attr_getguardsize
#undef pthread_attr_setguardsize
#undef pthread_attr_getschedparam
#undef pthread_attr_setschedparam
#undef pthread_attr_getschedpolicy
#undef pthread_attr_setschedpolicy
#undef pthread_attr_getinheritsched
#undef pthread_attr_setinheritsched
#undef pthread_attr_getscope
#undef pthread_attr_setscope
#undef pthread_attr_getstackaddr
#undef pthread_attr_setstackaddr
#undef pthread_attr_getstacksize
#undef pthread_attr_setstacksize
#undef pthread_attr_getstack
#undef pthread_attr_setstack
#undef pthread_attr_setaffinity_np
#undef pthread_attr_getaffinity_np
#undef pthread_getattr_np
#undef pthread_setschedparam
#undef pthread_getschedparam
#undef pthread_setschedprio
#undef pthread_getname_np
#undef pthread_setname_np
#undef pthread_getconcurrency
#undef pthread_setconcurrency
#undef pthread_yield
#undef pthread_setaffinity_np
#undef pthread_getaffinity_np
#undef pthread_once
#undef pthread_setcancelstate
#undef pthread_setcanceltype
#undef pthread_cancel
#undef pthread_mutex_init
#undef pthread_mutex_destroy
#undef pthread_mutex_trylock
#undef pthread_mutex_lock
#undef pthread_mutex_timedlock
#undef pthread_mutex_unlock
#undef pthread_mutex_getprioceiling
#undef pthread_mutex_setprioceiling
#undef pthread_mutex_consistent
#undef pthread_mutex_consistent_np
#undef pthread_mutexattr_init
#undef pthread_mutexattr_destroy
#undef pthread_mutexattr_getpshared
#undef pthread_mutexattr_setpshared
#undef pthread_mutexattr_gettype
#undef pthread_mutexattr_settype
#undef pthread_mutexattr_getprotocol
#undef pthread_mutexattr_setprotocol
#undef pthread_mutexattr_getprioceiling
#undef pthread_mutexattr_setprioceiling
#undef pthread_mutexattr_getrobust
#undef pthread_mutexattr_getrobust_np
#undef pthread_mutexattr_setrobust
#undef pthread_mutexattr_setrobust_np
#undef pthread_rwlock_init
#undef pthread_rwlock_destroy
#undef pthread_rwlock_rdlock
#undef pthread_rwlock_tryrdlock
#undef pthread_rwlock_timedrdlock
#undef pthread_rwlock_wrlock
#undef pthread_rwlock_trywrlock
#undef pthread_rwlock_timedwrlock
#undef pthread_rwlock_unlock
#undef pthread_rwlockattr_init
#undef pthread_rwlockattr_destroy
#undef pthread_rwlockattr_getpshared
#undef pthread_rwlockattr_setpshared
#undef pthread_rwlockattr_getkind_np
#undef pthread_rwlockattr_setkind_np
#undef pthread_cond_init
#undef pthread_cond_destroy
#undef pthread_cond_signal
#undef pthread_cond_broadcast
#undef pthread_cond_wait
#undef pthread_cond_timedwait
#undef pthread_condattr_init
#undef pthread_condattr_destroy
#undef pthread_condattr_getpshared
#undef pthread_condattr_setpshared
#undef pthread_condattr_getclock
#undef pthread_condattr_setclock
#undef pthread_spin_init
#undef pthread_spin_destroy
#undef pthread_spin_lock
#undef pthread_spin_trylock
#undef pthread_spin_unlock
#undef pthread_barrier_init
#undef pthread_barrier_destroy
#undef pthread_barrier_wait
#undef pthread_barrierattr_init
#undef pthread_barrierattr_destroy
#undef pthread_barrierattr_getpshared
#undef pthread_barrierattr_setpshared
#undef pthread_key_create
#undef pthread_key_delete
#undef pthread_setspecific
#undef pthread_getcpuclockid
#undef pthread_atfork
#undef pthread_getspecific
#undef pthread_setspecific
#undef pthread_self
#undef pthread_kill

#undef PTHREAD_THREADS_MAX
#undef PTHREAD_KEYS_MAX
#undef PTHREAD_STACK_MIN
#undef PTHREAD_CREATE_DETACHED
#undef PTHREAD_CREATE_JOINABLE

#undef PTHREAD_ONCE_INIT
#undef PTHREAD_COND_INITIALIZER
#undef PTHREAD_MUTEX_INITIALIZER


#undef pthread_t
#undef pthread_attr_t
#undef pthread_key_t
#undef pthread_cond_t
#undef pthread_mutex_t
#undef pthread_once_t
#undef pthread_mutexattr_t

#undef PTHREAD_MUTEX_NORMAL
#undef PTHREAD_MUTEX_ERRORCHECK
#undef PTHREAD_MUTEX_RECURSIVE
#undef PTHREAD_MUTEX_DEFAULT

#undef __thread
#undef thread_local

#undef PTHREAD_SCOPE_PROCESS 
#undef PTHREAD_SCOPE_SYSTEM 