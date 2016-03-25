#ifndef sumi_THREAD_SAFE_REFCOUNT_PTR_H
#define sumi_THREAD_SAFE_REFCOUNT_PTR_H

#include <cstdio>

namespace sumi {

template <class T>
class thread_safe_refcount_ptr {
  template <class U> friend class thread_safe_refcount_ptr;

 private:
  T* ptr;

  template <class U>
  static void decref(U* ptr){
    if (ptr && ptr->decref() == 0)
      delete ptr;
  }
  
  template <class U>
  static void incref(U* ptr){
    if (ptr) ptr->incref();
  }

 public:
  thread_safe_refcount_ptr() : ptr(0) { 
  }

  template <class U>
  thread_safe_refcount_ptr(const thread_safe_refcount_ptr<U>& rhs) : ptr(rhs.ptr) {
    incref(ptr);
  }

  thread_safe_refcount_ptr(const thread_safe_refcount_ptr& rhs) : ptr(rhs.ptr) {
    incref(ptr);
  }
  
  thread_safe_refcount_ptr(T* rhs) : ptr(rhs) {
    incref(ptr);
  }

  ~thread_safe_refcount_ptr(){
    decref(ptr);
  }
  
  T*
  get() const {
    return ptr;
  }

  template <class U>
  thread_safe_refcount_ptr<T>&
  operator=(const thread_safe_refcount_ptr<U>& rhs){
    incref(rhs.ptr);
    decref(ptr);
    ptr = rhs.ptr;
    return *this;
  }
  
  thread_safe_refcount_ptr<T>&
  operator=(const thread_safe_refcount_ptr& rhs){
    incref(rhs.ptr);
    decref(ptr);
    ptr = rhs.ptr;
    return *this;    
  }

  thread_safe_refcount_ptr<T>&
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

#endif
