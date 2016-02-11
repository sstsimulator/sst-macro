#ifndef LOCALIZE_GLOBAL_H
#define LOCALIZE_GLOBAL_H

#ifdef __cplusplus

#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>

namespace sstmac {
namespace sw {

class global_variables {
 public:
  static int context_size;
  static char* global_initer;

  static int allocate_offset(int my_size){
    //make sure these are at least 16-byte aligned
    int padding = (16 - my_size%16) % 16;
    int next_offset = context_size;
    context_size += my_size + padding;
    return next_offset;
  }

};

template <class T>
class global_variable {
 public:
  global_variable(){
    my_offset_ = global_variables::allocate_offset(sizeof(T));
  }

  global_variable(const T& t){
    my_offset_ = global_variables::allocate_offset(sizeof(T));
    char* offset_ptr = global_variables::global_initer + my_offset_;
    T* init_ptr = reinterpret_cast<T*>(offset_ptr);
    *init_ptr = t;
  }

  operator T&() {
    char* offset_ptr = (char*) operating_system::static_os_thread_context()
                                  .current_thread->global_variables() + my_offset_;
    T* my_ptr = reinterpret_cast<T*>(offset_ptr);
    return *my_ptr;
  }

 private:
  int my_offset_;
};

template <class T, int N>
class global_variable<T[N]> {
 public:
  typedef T arrayType[N];
  global_variable(){
    my_offset_ = global_variables::allocate_offset(sizeof(arrayType));
  }

  global_variable(const T initer[N]){
    my_offset_ = global_variables::allocate_offset(sizeof(arrayType));
    char* offset_ptr = global_variables::global_initer + my_offset_;
    ::memcpy(offset_ptr, initer, sizeof(arrayType));
  }

  operator T*() {
    char* offset_ptr = (char*) operating_system::static_os_thread_context()
                                  .current_thread->global_variables() + my_offset_;
    return reinterpret_cast<T*>(offset_ptr);
  }

 private:
  int my_offset_;
};

  }
}

#endif
#endif // LOCALIZE_GLOBAL_H

