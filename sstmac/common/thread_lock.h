#ifndef THREAD_LOCK_H
#define THREAD_LOCK_H

#include <sstmac/common/sstmac_config.h>
#include <pthread.h>

#ifdef SSTMAC_PTHREAD_MACRO_H
#error sstmacro thread_lock.h should never be used with pthread.h replacement headers
#endif

namespace sstmac {

class mutex_thread_lock
{

 public:
  mutex_thread_lock();

  ~mutex_thread_lock();

  void lock();

  void unlock();

  bool trylock();

  bool locked() const {
    return locked_;
  }

 private:
  pthread_mutex_t mutex_;

  bool locked_;

};

#if SSTMAC_USE_SPINLOCK
class spin_thread_lock
{
 public:
  spin_thread_lock();

  ~spin_thread_lock();

  void lock();

  void unlock();

  bool trylock();

  bool locked() const {
    return locked_;
  }

 private:
  pthread_spinlock_t lock_;

  bool locked_;
};
#endif

#if SSTMAC_USE_SPINLOCK
typedef spin_thread_lock thread_lock;
#else
typedef mutex_thread_lock thread_lock;
#endif

class lockable {
 public:
  void lock(){
#if SSTMAC_USE_MULTITHREAD
    lock_.lock();
#endif
  }

  void unlock(){
#if SSTMAC_USE_MULTITHREAD
    lock_.unlock();
#endif
  }

 private:
#if SSTMAC_USE_MULTITHREAD
  thread_lock lock_;
#endif
};

}

#endif // THREAD_LOCK_H
