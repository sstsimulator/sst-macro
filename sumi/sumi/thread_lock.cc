
#include <sumi/thread_lock.h>
#include <sprockit/errors.h>
#include <string.h>

namespace sumi {

mutex_thread_lock::mutex_thread_lock()
{
  int signal = pthread_mutex_init(&mutex_, NULL);
  if (signal != 0) {
    spkt_throw_printf(sprockit::spkt_error,
        "mutex init error %d: %s",
        signal, ::strerror(signal));
  }
}

mutex_thread_lock::~mutex_thread_lock()
{
  /** Ignore the signal for now since whatever person wrote
    some of the pthread implementations doesn't know how turn off
    all of the locks. This often erroneously returns signal 16 EBUSY */
  int signal = pthread_mutex_destroy(&mutex_);
}

void
mutex_thread_lock::lock()
{
  int signal = pthread_mutex_lock(&mutex_);
  if (signal != 0) {
    spkt_throw_printf(sprockit::spkt_error,
        "pthread_lock::lock: mutex lock error %d: %s",
        signal, ::strerror(signal));
  }
  locked_ = true;
}

bool
mutex_thread_lock::trylock()
{
  int signal = pthread_mutex_trylock(&mutex_);
  bool locked = signal == 0;
  if (locked) {
    locked_ = true;
  }
  return locked;
}

void
mutex_thread_lock::unlock()
{
  locked_ = false;
  int signal = pthread_mutex_unlock(&mutex_);
  if (signal != 0) {
    spkt_throw(sprockit::spkt_error,
        "pthread_lock::unlock: unlocking mutex that I don't own: %d: %s",
        signal, ::strerror(signal));
  }
}

#if SUMI_USE_SPINLOCK
spin_thread_lock::spin_thread_lock()
{
  int signal = pthread_spin_init(&lock_, PTHREAD_PROCESS_PRIVATE);
  if (signal != 0) {
    spkt_throw_printf(sprockit::spkt_error,
        "mutex init error %d: %s",
        signal, ::strerror(signal));
  }
}

spin_thread_lock::~spin_thread_lock()
{
  /** Ignore the signal for now since whatever person wrote
    some of the pthread implementations doesn't know how turn off
    all of the locks. This often erroneously returns signal 16 EBUSY */
  int signal = pthread_spin_destroy(&lock_);
}

void
spin_thread_lock::lock()
{
  int signal = pthread_spin_lock(&lock_);
  if (signal != 0) {
    spkt_throw_printf(sprockit::spkt_error,
        "pthread_lock::lock: mutex lock error %d: %s",
        signal, ::strerror(signal));
  }
  locked_ = true;
}

bool
spin_thread_lock::trylock()
{
  int signal = pthread_spin_trylock(&lock_);
  bool locked = signal == 0;
  if (locked) {
    locked_ = true;
  }
  return locked;
}

void
spin_thread_lock::unlock()
{
  locked_ = false;
  int signal = pthread_spin_unlock(&lock_);
  if (signal != 0) {
    spkt_throw(sprockit::spkt_error,
        "pthread_lock::unlock: unlocking mutex that I don't own: %d: %s",
        signal, ::strerror(signal));
  }
}
#endif

}


