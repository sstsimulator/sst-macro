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

#ifndef THREAD_SAFE_LIST_H
#define THREAD_SAFE_LIST_H

#include <list>
#include <sumi/lockable.h>
#include <sprockit/errors.h>

namespace sumi
{

/** Thread-safe replacement for std::list, with some differences in API. */
template <class T>
class thread_safe_list :
  public lockable
{
 public:
  typedef typename std::list<T>::iterator iterator;
  typedef typename std::list<T>::const_iterator const_iterator;
  typedef size_t size_type;

  //
  // Constructors.
  //

  thread_safe_list() {}

  thread_safe_list(const thread_safe_list& other){
    lock();
    other.lock();
    list_ = other.list_;
    other.unlock();
    unlock();
  }

  //
  // These functions acquire a lock.
  //

  const_iterator
  start_iteration() const {
    lock();
    return list_.end();
  }

  iterator
  start_iteration(){
    lock();
    return list_.end();
  }

  //
  // These functions release a previously-acquired lock.
  //

  void
  end_iteration() const {
    unlock();
  }

  //
  // These functions require that a lock has already been acquired
  // (since most take or return iterators).  Note that they do not
  // check WHO acquired the lock (although for correct operation,
  // the caller must be the one who acquired it).
  //

  const_iterator
  begin() const {
    verify("begin");
    return list_.begin();
  }

  iterator
  begin(){
    verify("begin");
    return list_.begin();
  }

  void
  erase(iterator it){
    verify("erase");
    list_.erase(it);
  }

  iterator
  insert(iterator it, const T& entry){
    verify("insert");
    return list_.insert(it, entry);
  }

  bool
  empty_locked() const {
    verify("empty_locked");
    return list_.empty();
  }

  size_type size_locked() const {
    verify("size_locked");
    return list_.size();
  }

  void
  clear_locked(){
    verify("clear_locked");
    list_.clear();
  }

  void push_back_locked(const T& t){
    verify("push_back_locked");
    list_.push_back(t);
  }

  bool
  pop_front_and_return_locked(T& ret){
    verify("pop_front_and_return_locked");
    bool empty = list_.empty();
    if (!empty){
      ret = list_.front();
      list_.pop_front();
    }
    return empty;
  }

  bool
  pop_back_and_return_locked(T& ret){
    verify("pop_back_and_return_locked");
    bool empty = list_.empty();
    if (!empty){
      ret = list_.back();
      list_.pop_back();
    }
    return empty;
  }

  //
  // These functions lock on entry and unlock before returning.
  //

  bool
  empty() const {
    lock();
    bool ret = list_.empty();
    unlock();
    return ret;
  }

  size_type size() const {
    lock();
    size_type ret = list_.size();
    unlock();
    return ret;
  }

  thread_safe_list& operator=(const thread_safe_list &rhs){
    if (&rhs != this){
      lock();
      rhs.lock();
      list_ = rhs.list_;
      rhs.unlock();
      unlock();
    }
    return *this;
  }

  void push_back(const T& t){
    lock();
    push_back_locked(t);
    unlock();
  }

  bool
  pop_front_and_return(T& ret){
    lock();
    bool empty = pop_front_and_return_locked(ret);
    unlock();
    return empty;
  }

  bool
  pop_back_and_return(T& ret){
    lock();
    bool empty = pop_back_and_return_locked(ret);
    unlock();
    return empty;
  }

  void
  clear(){
    lock();
    list_.clear();
    unlock();
  }

 protected:
  void
  verify(const char* fxn) const {
    if (!locked()){
      spkt_throw_printf(sprockit::value_error,
        "thread safe list is not locked in %s", fxn);
    }
  }

  std::list<T> list_;
};

}

#endif // THREAD_SAFE_LIST_H