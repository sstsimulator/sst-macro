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

#ifndef THREAD_BARRIER_H
#define THREAD_BARRIER_H

#include <pthread.h>
#include <vector>
#include <stdint.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/backends/native/clock_cycle_parallel/clock_cycle_event_container.h>

namespace sstmac {
namespace native {

class thread_barrier_functor {

 public:
  virtual int64_t
  execute(int64_t) = 0;

};


class thread_barrier {

 public:
#ifdef SSTMAC_USE_SPINLOCK
  typedef pthread_spinlock_t lock_t;
#else
  typedef pthread_mutex_t lock_t;
#endif

  thread_barrier(int nthread);

  thread_barrier(){}

  void
  start(int me, thread_barrier_functor* functor = 0);

  int64_t
  vote(int me, int64_t vote, vote_type_t ty,
       thread_barrier_functor* functor = 0);

  void init(int nthread);

 protected:
  int64_t
  run(int me, int level, int nthread, int64_t vote, vote_type_t ty,
      thread_barrier_functor* functor);

  void lock(lock_t* l);

  void unlock(lock_t* l);

  int init_level(int level, int num, int offset);



 protected:
  struct barrier_state {
    lock_t* in_lock;
    lock_t* out_lock;
    int64_t vote;
  };

  int nthread_;
  int twoPowN_;
  lock_t* out_locks_;
  lock_t* in_locks_;
  std::vector<std::vector<barrier_state> > levels_;
};

class multiuse_thread_barrier
{
 public:
  thread_barrier* b0;
  thread_barrier* b1;

 private:
  int next;

 public:
  multiuse_thread_barrier() :
   b0(0), b1(0),
   next(0)
  {
  }

  void
  start(int me){
    if (next==0){
      b0->start(me);
    } else {
      b1->start(me);
    }
    next=(next+1)%2;
  }
};

}
}

#endif // THREAD_BARRIER_H