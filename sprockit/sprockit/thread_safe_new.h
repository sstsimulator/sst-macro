#ifndef THREAD_SAFE_NEW_H
#define THREAD_SAFE_NEW_H

#include <vector>

namespace sprockit {

template <class T>
T& thread_stack_size(){
  static T stacksize = 0;
  return stacksize;
}

static int inline current_thread_id() {
  int stacksize = thread_stack_size<int>();
  if (stacksize == 0){
    return 0;
  } else {
    char x;
    intptr_t stackptr = (intptr_t) &x;
    intptr_t stack_mult = stackptr / stacksize;
    char* aligned_stack_ptr = (char*) (stack_mult * stacksize);
    int* thrPtr = (int*) aligned_stack_ptr;
    return *thrPtr;
  }
}

template <class T>
class thread_safe_new {
 public:
#if 1
  static void* operator new(size_t sz){
    if (sz != sizeof(T)){
      spkt_abort_printf("allocating mismatched sizes: %d != %d",
                        sz, sizeof(T));
    }
    int thread = current_thread_id();
    auto& vec = to_allocate_[thread];
    if (vec.empty()){
      grow(vec);
    }
    void* ret = vec.back();
    vec.pop_back();
    return ret;
  }

  static void* operator new(size_t sz, void* ptr){
    return ptr;
  }

  static void operator delete(void* ptr){
    int thread = current_thread_id();
    auto& vec = to_allocate_[thread];
    vec.push_back(ptr);
  }
#endif
  static void grow(std::vector<void*>& vec){
    size_t unitSize = sizeof(T);
    if (unitSize % 32 != 0){
      size_t rem = 32 - unitSize % 32;
      unitSize += rem;
    }

    char* newTs = new char[unitSize*increment];
    char* ptr = newTs;
    for (int i=0; i < increment; ++i, ptr += unitSize){
      vec.push_back(ptr);
    }
  }

 private:
#define MAX_NUM_NEW_SAFE_THREADS 128
  static std::vector<void*> to_allocate_[MAX_NUM_NEW_SAFE_THREADS];
  static int constexpr increment = 512;

};

template <class T> std::vector<void*> thread_safe_new<T>::to_allocate_[MAX_NUM_NEW_SAFE_THREADS];

}

#endif // THREAD_SAFE_NEW_H
