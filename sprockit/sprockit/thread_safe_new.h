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

struct thread_allocator_set {
#define MAX_NUM_NEW_SAFE_THREADS 128
  std::vector<char*> allocations[MAX_NUM_NEW_SAFE_THREADS];
  std::vector<void*> available[MAX_NUM_NEW_SAFE_THREADS];
  ~thread_allocator_set(){
    for (int i=0; i < MAX_NUM_NEW_SAFE_THREADS; ++i){
      auto& vec = allocations[i];
      for (char* ptr : vec){
        delete[] ptr;
      }
    }
  }
};

template <class T>
class thread_safe_new {
 public:
#if 1
  static void free_at_end(T* ptr){
    //do nothing - the allocation is getting cleaned up
  }

  template <class... Args>
  static T* allocate_at_beginning(Args&&... args){
    void* ptr = allocate(0);
    return new (ptr) T(std::forward<Args>(args)...);
  }

  static void* allocate(int thread){
    if (alloc_.available[thread].empty()){
      grow(thread);
    }
    void* ret = alloc_.available[thread].back();
    alloc_.available[thread].pop_back();
    return ret;
  }

  static void* operator new(size_t sz){
    if (sz != sizeof(T)){
      spkt_abort_printf("allocating mismatched sizes: %d != %d",
                        sz, sizeof(T));
    }
    int thread = current_thread_id();
    return allocate(thread);
  }

  static void* operator new(size_t sz, void* ptr){
    return ptr;
  }

  static void operator delete(void* ptr){
    int thread = current_thread_id();
    alloc_.available[thread].push_back(ptr);
  }
#else
  template <class... Args>
  static T* allocate_at_beginning(Args&&... args){
    return new T(std::forward<Args>(args)...);
  }

  static void free_at_end(T* ptr){
    delete ptr;
  }
#endif
#define SSTMAC_CACHE_ALIGNMENT 64
  static void grow(int thread){
    size_t unitSize = sizeof(T);
    if (unitSize % SSTMAC_CACHE_ALIGNMENT != 0){
      size_t rem = SSTMAC_CACHE_ALIGNMENT - unitSize % SSTMAC_CACHE_ALIGNMENT;
      unitSize += rem;
    }

    char* newTs = new char[unitSize*increment];
    char* ptr = newTs;
    int numElems = increment;
    if (uintptr_t(ptr) % SSTMAC_CACHE_ALIGNMENT){
      size_t rem = SSTMAC_CACHE_ALIGNMENT - (uintptr_t(ptr) % SSTMAC_CACHE_ALIGNMENT);
      ptr += rem;
      numElems -= 1;
    }
    for (int i=0; i < numElems; ++i, ptr += unitSize){
      alloc_.available[thread].push_back(ptr);
    }
    alloc_.allocations[thread].push_back(newTs);
  }

 private:
  static thread_allocator_set alloc_;
  static int constexpr increment = 512;

};

template <class T> thread_allocator_set thread_safe_new<T>::alloc_;

}

#endif // THREAD_SAFE_NEW_H
