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

#include <sstmac/backends/native/clock_cycle_parallel/thread_barrier.h>
#include <iostream>
#include <sprockit/spkt_string.h>

namespace sstmac {
namespace native {

void
thread_barrier::lock(lock_t* l)
{
#ifdef SSTMAC_USE_SPINLOCK
  pthread_spin_lock(l);
#else
  pthread_mutex_lock(l);
#endif
}

void
thread_barrier::unlock(lock_t* l)
{
#ifdef SSTMAC_USE_SPINLOCK
  pthread_spin_unlock(l);
#else
  pthread_mutex_unlock(l);
#endif
}

int64_t
thread_barrier::run(int me, int level, int nthread, 
   int64_t vote, vote_type_t ty,
   thread_barrier_functor* functor)
{
  if (nthread==1){
    if (level == 0) abort();
    if (functor){
      return functor->execute(vote);
    } else {
      return vote;
    }
  }

  barrier_state& myst = levels_[level][me];
  myst.vote = vote;

  int next_nthread = nthread/2 + (nthread%2);
  int next_me = me/2;
  int next_level = level+1;
  int role = me % 2;
  if (role == 0){
    int partner = me + 1;
    if (partner >= nthread){
      return run(next_me, next_level, next_nthread, vote, ty, functor); //just go right on
    } else {
      barrier_state& pst = levels_[level][partner];
      lock_t* plock = pst.in_lock;
      lock_t* mylock = myst.out_lock;
      lock(plock); //make sure my partner is unlocked
      //my partner arrived at the barrier if I reach here
      int64_t next_vote;
      switch (ty){
        case vote_type_t::min:
         next_vote = std::min(myst.vote, pst.vote);
         break;
        case vote_type_t::max:
         next_vote = std::max(myst.vote, pst.vote);
         break;
      }
      myst.vote = run(next_me, next_level, next_nthread, next_vote, ty, functor);
      //okay, I will now own a different lock
      //swap things for the next iteration
      myst.out_lock = plock;
      pst.in_lock = mylock;
      //at this point, my partner is waiting for me to unlock
      unlock(mylock);
    }
  } else {
    int partner = me - 1;
    barrier_state& myst = levels_[level][me];
    barrier_state& pst = levels_[level][partner];
    lock_t* plock = pst.out_lock;
    lock_t* mylock = myst.in_lock;
    unlock(mylock);
    //unlock myself so partner knows I am here
    //lock on partner to wait on partner signal
    lock(plock);
    myst.out_lock = plock;
    pst.in_lock = mylock;
    //I receive the vote, not merge it
    myst.vote = pst.vote;
  }

  return myst.vote;
}

int64_t
thread_barrier::vote(int me, int64_t vote, vote_type_t ty,
          thread_barrier_functor* functor)
{
    if (ty == vote_type_t::max){
  static thread_lock lock;
  lock.lock();
  lock.unlock();
    }
  return run(me, 0, nthread_, vote, ty, functor);
}

void
thread_barrier::start(int me, thread_barrier_functor* functor)
{
  //send a fake vote
  run(me, 0, nthread_, 0, vote_type_t::min, functor);
}

int
thread_barrier::init_level(int level, int num, int offset)
{
  std::vector<barrier_state>& vec = levels_[level];
  vec.resize(num);
  for (int i=0; i < num; ++i, ++offset){
    barrier_state& st = vec[i];
    st.in_lock = &in_locks_[offset];
    st.out_lock = &out_locks_[offset];
#ifdef SSTMAC_USE_SPINLOCK
    pthread_spin_init(st.in_lock, PTHREAD_PROCESS_PRIVATE);
    pthread_spin_init(st.out_lock, PTHREAD_PROCESS_PRIVATE);
#else
    pthread_mutex_init(st.in_lock, NULL);
    pthread_mutex_init(st.out_lock, NULL);
#endif
    lock(st.in_lock);
    lock(st.out_lock);
  }
  return offset;
}

void
thread_barrier::init(int nthread)
{
  nthread_ = nthread;
  if (nthread == 1){
    return; //nothing to do here
  }

  int nlevels = 0;
  int num_locks = 1;
  twoPowN_ = 1;
  while (twoPowN_ < nthread){
    nlevels++;
    twoPowN_ *= 2;
    num_locks += twoPowN_;
  }
  levels_.resize(nlevels);
  //maybe not a power of two, also no thread lock for top of tree(+1)
  int extra = twoPowN_ - nthread + 1;
  num_locks -= extra;
  in_locks_ = new lock_t[num_locks];
  out_locks_ = new lock_t[num_locks];

  int offset = init_level(0, nthread, 0);
  int numNextLevel = twoPowN_ / 2;
  for (int i=1; i < nlevels; ++i){
    offset = init_level(i, numNextLevel, offset);
    numNextLevel /= 2;
  }
}

thread_barrier::thread_barrier(int nthread)
{
  init(nthread);
}


}
}