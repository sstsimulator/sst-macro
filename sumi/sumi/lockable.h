#ifndef LOCKABLE_H
#define LOCKABLE_H

#include <sumi/thread_lock.h>

namespace sumi
{

#ifndef SSTMAC
class lockable {
 public:
  void lock() const {
    lock_.lock();
  }

  void unlock() const {
    lock_.unlock();
  }

  bool locked() const {
    return lock_.locked();
  }

 private:
#if SUMI_USE_SPINLOCK
  mutable spin_thread_lock lock_;
#else
  mutable mutex_thread_lock lock_;
#endif
};
#else
class lockable {
 public:
  void lock() const { locked_ = true; }

  void unlock() const { locked_ = false; }

  bool locked() const {
    return locked_;
  }

  lockable() : locked_(false) {}

 private:
  mutable bool locked_;

};
#endif

}

#endif // LOCKABLE_H
