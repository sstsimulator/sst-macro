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

#ifndef MPITHREADTEST_H_INCLUDED
#define MPITHREADTEST_H_INCLUDED

#include "mpitestconf.h"

/* 
   Define routines to start a thread for different thread packages.  
   The routine that is started is expected to return data when it
   exits; the type of this data is 

   MTEST_THREAD_RETURN_TYPE
*/

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#define MTEST_THREAD_RETURN_TYPE DWORD
#define MTEST_THREAD_HANDLE HANDLE
#define MTEST_THREAD_LOCK_TYPE HANDLE
#elif defined(HAVE_PTHREAD_H)
#define USE_PTHREADS 1
#include <pthread.h>
#define MTEST_THREAD_RETURN_TYPE void *
#define MTEST_THREAD_HANDLE pthread_t
#define MTEST_THREAD_LOCK_TYPE pthread_mutex_t
#else
#error Unknown Thread Package
#endif

/* A dummy retval that is ignored */
#define MTEST_THREAD_RETVAL_IGN 0

int MTest_Start_thread(MTEST_THREAD_RETURN_TYPE (*fn)(void *p),void *arg);
int MTest_Join_threads( void );
int MTest_thread_lock_create( MTEST_THREAD_LOCK_TYPE *);
int MTest_thread_lock( MTEST_THREAD_LOCK_TYPE *);
int MTest_thread_unlock( MTEST_THREAD_LOCK_TYPE *);
int MTest_thread_lock_free( MTEST_THREAD_LOCK_TYPE *);
int MTest_thread_barrier_init(void);
int MTest_thread_barrier(int);
int MTest_thread_barrier_free(void);
#endif