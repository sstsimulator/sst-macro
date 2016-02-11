#include <sstmac/common/thread_info.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/common/sstmac_config.h>
#include <sprockit/errors.h>
#include <iostream>
#include <string.h>
#include <stdint.h>
#include <list>

namespace sstmac {

size_t thread_info::stacksize_ = 0;
//thread_info::thread_type_t thread_info::virt_thr_type_ = thread_info::none;
static const int tls_sanity_check = 42042042;

struct active_thread {
  pthread_t* pthr;
  int id;
};
static std::list<active_thread> active_threads_;
static thread_lock lock_;

void
thread_info::register_kernel_space_virtual_thread(int thread_id, pthread_t* pthr, pthread_attr_t* attrs)
{
  //if (virt_thr_type_ == user){
  //  spkt_throw(sprockit::value_error,
  //      "thread_info: cannot mix kernel and user space threads");
  //}
  spkt_throw(sprockit::value_error,
    "thread_info: pthread+pthread no longer allowed for multithreading - use pthread+ucontext");
#if 0 
  SSTMAC_USE_MULTITHREAD
  active_thread thr;
  thr.id = thread_id;
  thr.pthr = pthr;
  lock_.lock();
  active_threads_.push_back(thr);
  virt_thr_type_ = kernel;
  lock_.unlock();
#endif
}

int
thread_info::kernel_space_thread_id()
{
  lock_.lock();
  pthread_t me = pthread_self();
  std::list<active_thread>::iterator it, end = active_threads_.end();
  for (it = active_threads_.begin(); it != end; ++it){
    active_thread& tester = *it;
    if (pthread_equal(me, *tester.pthr)){
      lock_.unlock();
      return tester.id;
    }
  }

  spkt_throw_printf(sprockit::illformed_error,
    "thread_info::current_physical_thread_id: cannot find thread id - %d active threads",
    active_threads_.size());

  return -1;
}

void
thread_info::register_user_space_virtual_thread(int phys_thread_id, void *stack, size_t stacksize)
{
  //if (virt_thr_type_ == kernel){
  //  spkt_throw(sprockit::value_error,
  //      "thread_info: cannot mix kernel and user space threads");
  //}
#if SSTMAC_USE_MULTITHREAD
  size_t stack_mod = ((size_t)stack) % stacksize;
  if (stack_mod != 0){
    spkt_throw_printf(sprockit::value_error,
        "user space thread stack is not aligned on %llu bytes",
        stacksize);
  }

  //essentially treat this as thread-local storage
  int* tls = (int*) stack;
  tls[0] = phys_thread_id;
  tls[1] = tls_sanity_check;
  lock_.lock();
  if (stacksize_ == 0) stacksize_ = stacksize;
  bool invalid = stacksize_ != stacksize ;
  //virt_thr_type_ = user;
  lock_.unlock();
  if (invalid){
    spkt_throw_printf(sprockit::value_error,
     "thread_info::register_thread: non-uniform stack sizes %llu and %llu",
     stacksize_, stacksize);
  }
#endif
}



}
