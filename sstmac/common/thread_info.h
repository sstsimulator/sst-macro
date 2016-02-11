#ifndef THREAD_INFO_H
#define THREAD_INFO_H

#include <pthread.h>
#include <sstmac/common/sstmac_config.h>

namespace sstmac {

class thread_info {
 public:
  static void
  register_kernel_space_virtual_thread(int thread_id, pthread_t* thr, pthread_attr_t* attrs);

  static void
  register_user_space_virtual_thread(int phys_thread_id, void* stack, size_t stacksize);

  static inline int
  current_physical_thread_id(){
#if SSTMAC_HAVE_UCONTEXT
    return user_space_thread_id();
#else
    return kernel_space_thread_id();
#endif
  }

 protected:
  static int kernel_space_thread_id();

  static inline int
  user_space_thread_id(){
    char x;
    size_t stackptr = (size_t) &x;
    size_t stack_mult = stackptr / stacksize_;
    size_t aligned_stack_ptr = stack_mult * stacksize_;
    int* tls = (int*) aligned_stack_ptr;
    return tls[0];
  }

  static size_t stacksize_;



};

}

#endif // THREAD_INFO_H
