#ifndef sprockit_refcount_ptr_h
#define sprockit_refcount_ptr_h

#include <cstdio>
#include <sprockit/spkt_config.h>

#if SPKT_ATOMIC_REFCOUNT
#define ref_increment(refcount) __atomic_add_fetch(&refcount, 1, __ATOMIC_SEQ_CST)
#define ref_decrement_return(refcount)  __atomic_add_fetch(&refcount, -1, __ATOMIC_SEQ_CST)
inline int fetch_and_add( int * variable, int value) {
  asm volatile("lock; xaddl %%eax, %2;"
    :"=a" (value)                  //Output
    :"a" (value), "m" (*variable)  //Input
    :"memory");
    return value;
}
#else
#define ref_increment(refcount) ++refcount
#define ref_decrement_return(refcount) --refcount
#endif

namespace sprockit {

template <class T>
class refcount_ptr {
  template <class U> friend class refcount_ptr;
 private:
  T* ptr;

  template <class U>
  void decref(U* ptr){
    if (ptr){   
      if (ref_decrement_return(ptr->references) == 0)
        delete ptr;
    }
  }
  
  template <class U>
  void incref(U* ptr){
    if (ptr) ref_increment(ptr->references);
  }

 public:
  refcount_ptr() : ptr(0) { 
  }

  template <class U>
  refcount_ptr(const refcount_ptr<U>& rhs) : ptr(rhs.ptr) {
    incref(ptr);
  }

  refcount_ptr(const refcount_ptr& rhs) : ptr(rhs.ptr) {
    incref(ptr);
  }
  
  refcount_ptr(T* rhs) : ptr(rhs) {
    incref(ptr);
  }

  ~refcount_ptr(){
    decref(ptr);
  }
  
  T*
  get() const {
    return ptr;
  }

  template <class U>
  refcount_ptr<T>&
  operator=(const refcount_ptr<U>& rhs){
    incref(rhs.ptr);
    decref(ptr);
    ptr = rhs.ptr;
    return *this;
  }
  
  refcount_ptr<T>&
  operator=(const refcount_ptr& rhs){
    incref(rhs.ptr);
    decref(ptr);
    ptr = rhs.ptr;
    return *this;    
  }

  refcount_ptr<T>&
  operator=(T* rhs){
    incref(rhs);
    decref(ptr);
    ptr = rhs;
    return *this;
  }
  
  operator bool() const {
    return bool(ptr);
  }

  T*
  operator->() const {
    return ptr;
  }

  bool
  null() const {
    return ptr == 0;
  }
};

}

#if __cplusplus > 199711L

namespace std {
 template <class T> struct hash<sprockit::refcount_ptr<T> > {
  size_t 
  operator()(const sprockit::refcount_ptr<T>& ptr) const {
    return hash<void*>()(ptr.get());
  }
 };
}

#endif

#endif
