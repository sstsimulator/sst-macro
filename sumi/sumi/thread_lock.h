#ifndef sumi_THREAD_LOCK_H
#define sumi_THREAD_LOCK_H

#include <pthread.h>
#include <sumi/config.h>

namespace sumi {

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

#if SUMI_USE_SPINLOCK
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


}

#endif // THREAD_LOCK_H

