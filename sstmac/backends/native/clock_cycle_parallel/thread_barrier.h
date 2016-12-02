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
