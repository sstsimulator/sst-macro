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

#ifndef sumi_THREAD_SAFE_INT_H
#define sumi_THREAD_SAFE_INT_H

#include <sumi/lockable.h>

namespace sumi {

/** Can be used as a drop-in replacement for integral types where the
 *  provided API is sufficient.  Each member function locks on entry
 *  and unlocks before returning. */
template <class Integer>
class thread_safe_int :
  public lockable
{
 public:
  template <class Y>
  thread_safe_int(const Y& y) : value_(y) {}

  thread_safe_int(const thread_safe_int &y){
    lock();
    y.lock();
    value_ = y.value_;
    y.unlock();
    unlock();
  }

  template <class Y>
  Integer& operator=(const Y& y){
    lock();
    value_ = y;
    unlock();
    return value_;
  }

  thread_safe_int& operator=(const thread_safe_int &rhs){
    if (&rhs != this){  // avoid deadlock
      lock();
      rhs.lock();
      value_ = rhs.value_;
      rhs.unlock();
      unlock();
    }
    return *this;
  }

  Integer
  operator+=(Integer rhs){
    lock();
    value_ += rhs;
    Integer tmp = value_;
    unlock();
    return tmp;
  }

  Integer
  operator-=(Integer rhs){
    lock();
    value_ -= rhs;
    Integer tmp = value_;
    unlock();
    return tmp;
  }

  Integer
  operator|=(Integer rhs){
    lock();
    value_ |= rhs;
    Integer tmp = value_;
    unlock();
    return tmp;
  }

  Integer
  operator++(){
    lock();
    ++value_;
    Integer tmp = value_;
    unlock();
    return tmp;
  }

  Integer
  operator++(int i){
    lock();
    Integer tmp(value_);
    value_++;
    unlock();
    return tmp;
  }

  Integer
  operator--(){
    lock();
    --value_;
    Integer tmp = value_;
    unlock();
    return tmp;
  }

  Integer
  operator--(int i){
    Integer tmp(value_);
    lock();
    value_--;
    unlock();
    return tmp;
  }

  bool
  operator==(const thread_safe_int &rhs) const {
    if (&rhs == this) return true;  // avoid deadlock
    lock();
    Integer r = rhs;
    bool ret = (value_ == r);
    unlock();
    return ret;
  }

  bool
  operator!=(const thread_safe_int &rhs) const {
    if (&rhs == this) return false;  // avoid deadlock
    lock();
    Integer r = rhs;
    bool ret = (value_ != r);
    unlock();
    return ret;
  }

  bool
  operator<(const thread_safe_int &rhs) const {
    if (&rhs == this) return false;  // avoid deadlock
    lock();
    Integer r = rhs;
    bool ret = (value_ < r);
    unlock();
    return ret;
  }

  bool
  operator>(const thread_safe_int &rhs) const {
    if (&rhs == this) return false;  // avoid deadlock
    lock();
    Integer r = rhs;
    bool ret = (value_ > r);
    unlock();
    return ret;
  }

  bool
  operator<=(const thread_safe_int &rhs) const {
    if (&rhs == this) return true;  // avoid deadlock
    lock();
    Integer r = rhs;
    bool ret = (value_ <= r);
    unlock();
    return ret;
  }

  bool
  operator>=(const thread_safe_int &rhs) const {
    if (&rhs == this) return true;  // avoid deadlock
    lock();
    Integer r = rhs;
    bool ret = (value_ >= r);
    unlock();
    return ret;
  }

  bool
  operator==(const Integer &rhs) const {
    lock();
    bool ret = (value_ == rhs);
    unlock();
    return ret;
  }

  bool
  operator!=(const Integer &rhs) const {
    lock();
    bool ret = (value_ != rhs);
    unlock();
    return ret;
  }

  bool
  operator<(const Integer &rhs) const {
    lock();
    bool ret = (value_ < rhs);
    unlock();
    return ret;
  }

  bool
  operator>(const Integer &rhs) const {
    lock();
    bool ret = (value_ > rhs);
    unlock();
    return ret;
  }

  bool
  operator<=(const Integer &rhs) const {
    lock();
    bool ret = (value_ <= rhs);
    unlock();
    return ret;
  }

  bool
  operator>=(const Integer &rhs) const {
    lock();
    bool ret = (value_ >= rhs);
    unlock();
    return ret;
  }

  operator Integer() const {
    lock();
    Integer tmp =  value_;
    unlock();
    return tmp;
  }

 protected:
  Integer value_;
};

}

#endif // THREAD_SAFE_INT_H