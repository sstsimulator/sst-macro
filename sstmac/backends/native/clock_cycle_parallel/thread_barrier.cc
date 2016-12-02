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
    }
    else {
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
      //std::cout << sprockit::printf("Merged votes %d=%ld and %d=%ld to %ld\n",
      //  me, vote, partner, pst.vote, next_vote);
      myst.vote = run(next_me, next_level, next_nthread, next_vote, ty, functor);
      //okay, I will now own a different lock
      //swap things for the next iteration
      myst.out_lock = plock;
      pst.in_lock = mylock;
      //at this point, my partner is waiting for me to unlock
      unlock(mylock);
    }
  }
  else {
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



