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