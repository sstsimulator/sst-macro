/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#ifndef sprockit_refcount_ptr_h
#define sprockit_refcount_ptr_h

#include <cstdio>
#include <functional>
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