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

#ifndef fake_variable_h_
#define fake_variable_h_

#include <type_traits>
#include <cstddef>
#include <sstmac/software/process/thread.h>

#define DECLARE_OPERATOR(op,CONST,REF) \
  template <class T, class U> \
  Variable<T>REF \
  operator op(CONST Variable<T>& t, const U& u); \
  template <class T, class U> \
  U REF \
  operator op(CONST U& u, const Variable<T>& t); \
  template <class T, class U> \
  Variable<T> REF \
  operator op(CONST Variable<T>& t, const Variable<U>& u);

#define FRIEND_OPERATOR(op,CONST,REF) \
  template <class T1, class U> \
  friend Variable<T1> REF \
  operator op(CONST Variable<T1>& t, const U& u); \
  template <class T1, class U> \
  friend U REF \
  operator op(CONST U& u, const Variable<T1>& t); \
  template <class T1, class U> \
  friend Variable<T1> REF \
  operator op(CONST Variable<T1>& t, const Variable<U>& u);

template <class T> class VariablePtr;
template <class T> class Variable;

DECLARE_OPERATOR(+,const,)
DECLARE_OPERATOR(-,const,)
DECLARE_OPERATOR(*,const,)
DECLARE_OPERATOR(/,const,)
DECLARE_OPERATOR(&,const,)
DECLARE_OPERATOR(|,const,)
DECLARE_OPERATOR(+=,,&)
DECLARE_OPERATOR(*=,,&)
DECLARE_OPERATOR(-=,,&)
DECLARE_OPERATOR(/=,,&)
DECLARE_OPERATOR(&=,,&)
DECLARE_OPERATOR(|=,,&)

template <class T>
Variable<T>
sqrt(const Variable<T> &t);

template <class T>
Variable<T>
cbrt(const Variable<T> &t);

template <class T>
Variable<T>
fabs(const Variable<T> &t);

template <class T>
void*
memset(const VariablePtr<T>& t, int value, size_t size);

template <class T>
void*
memcpy(const VariablePtr<T>& dst, const VariablePtr<T>& src, size_t size);

template <class T>
class Variable 
{
  FRIEND_OPERATOR(+,const,)
  FRIEND_OPERATOR(-,const,)
  FRIEND_OPERATOR(*,const,)
  FRIEND_OPERATOR(/,const,)
  FRIEND_OPERATOR(&,const,)
  FRIEND_OPERATOR(|,const,)
  FRIEND_OPERATOR(+=,,&)
  FRIEND_OPERATOR(*=,,&)
  FRIEND_OPERATOR(-=,,&)
  FRIEND_OPERATOR(/=,,&)
  FRIEND_OPERATOR(&=,,&)
  FRIEND_OPERATOR(|=,,&)
  friend Variable<T> sqrt<>(const Variable<T> &t);
  friend Variable<T> cbrt<>(const Variable<T> &t);
  friend Variable<T> fabs<>(const Variable<T> &t);

 public:
  template <typename = std::enable_if<std::is_default_constructible<T>::value>>
  Variable()
    : nops(sstmac::sw::operating_system::current_thread()
            ->register_perf_ctr_variable<uint64_t>(this)),
      owns_nops(true)
  {
  }

  template <typename U,
    typename = std::enable_if<std::is_convertible<T,U>::value>>
  Variable(const U& u)
    : nops(sstmac::sw::operating_system::current_thread()
            ->register_perf_ctr_variable<uint64_t>(this)),
      owns_nops(true)
  {
  }

  Variable(uint64_t &nops_array)
    : nops(nops_array),
      owns_nops(false)
  {
  }

  Variable(const Variable& v)
    : nops(sstmac::sw::operating_system::current_thread()
            ->register_perf_ctr_variable<uint64_t>(this)),
      owns_nops(true)
  {
  }

  ~Variable(){
    if (owns_nops)
      sstmac::sw::operating_system::current_thread()
          ->remove_perf_ctr_variable(this);
  }

  VariablePtr<T> operator&() {
    return VariablePtr<T>(nops);
  }

  void* operator new[](std::size_t count) throw() {
    return 0;
  }

  Variable& operator=(const Variable& v) {
    return *this;
  }

  template <class U>
  Variable& operator=(const U& u){
    return *this;
  }

  constexpr operator T() const {
    return 0;
  }

  Variable operator-() const {
    nops++;
    return *this;
  }

  Variable& operator++(){
    nops++;
    return *this;
  }

  Variable operator++(int u){
    nops++;
    return *this;
  }

  Variable& operator--(){
    nops++;
    return *this;
  }

  Variable operator--(int u){
    nops++;
    return *this;
  }

 private:
  uint64_t &nops;
  bool owns_nops;
};

#define COMPARE(op) \
  template <class T, class U> \
  bool \
  operator op(const Variable<T>& t, const U& u){ \
    return true; \
  } \
  template <class T, class U> \
  bool \
  operator op(const U& u, const Variable<T>& t){ \
    return true;\
  } \
  template <class T, class U> \
  bool \
  operator op(const Variable<T>& t, const Variable<U>& u){ \
    return true; \
  }

#define OPERATOR(op,CONST,REF) \
  template <class T, class U> \
  Variable<T>REF \
  operator op(CONST Variable<T>& t, const U& u){ \
    t.nops++; \
    return t; \
  } \
  template <class T, class U> \
  U REF \
  operator op(CONST U& u, const Variable<T>& t){ \
    t.nops++; \
    return u;\
  } \
  template <class T, class U> \
  Variable<T> REF \
  operator op(CONST Variable<T>& t, const Variable<U>& u){ \
    t.nops++; \
    return t; \
  }

COMPARE(!=)
COMPARE(<)
COMPARE(>)
COMPARE(<=)
COMPARE(>=)
COMPARE(==)
OPERATOR(+,const,)
OPERATOR(-,const,)
OPERATOR(*,const,)
OPERATOR(/,const,)
OPERATOR(&,const,)
OPERATOR(|,const,)
OPERATOR(+=,,&)
OPERATOR(*=,,&)
OPERATOR(-=,,&)
OPERATOR(/=,,&)
OPERATOR(&=,,&)
OPERATOR(|=,,&)

template <class T>
Variable<T>
sqrt(const Variable<T> &t){
  t.nops++;
  return t;
}

template <class T>
Variable<T>
cbrt(const Variable<T> &t){
  t.nops++;
  return t;
}

template <class T>
Variable<T>
fabs(const Variable<T> &t){
  t.nops++;
  return t;
}


template <class T>
class VariablePtr
{
  friend void* memset<>(const VariablePtr<T>& t, int value, size_t size);
  friend void* memcpy<>(const VariablePtr<T>& dst, const VariablePtr<T>& src, size_t size);

 public:
  VariablePtr()
    : nops(sstmac::sw::operating_system::current_thread()
            ->register_perf_ctr_variable<uint64_t>(this)),
      elem(nops)
  {
  }

  template <typename U,
    typename = std::enable_if<std::is_convertible<T*,U>::value>>
  VariablePtr(const U& u)
    : nops(sstmac::sw::operating_system::current_thread()
            ->register_perf_ctr_variable<uint64_t>(this)),
      elem(nops)
  {
  }

  VariablePtr(uint64_t &nops_elem)
    : nops(nops_elem)
  {
  }

  VariablePtr(const VariablePtr &vp)
    : nops(sstmac::sw::operating_system::current_thread()
            ->register_perf_ctr_variable<uint64_t>(this)),
      elem(nops)
  {
  }

  ~VariablePtr(){
    sstmac::sw::operating_system::current_thread()
        ->remove_perf_ctr_variable(this);
  }

  void resize(size_t n){}

  void clear(){}

  Variable<T>& operator[](int idx){
    return elem;
  }

  const Variable<T>& operator[](int idx) const {
    return elem;
  }

  Variable<T>& operator[](Variable<int> idx){
    return elem;
  }

  const Variable<T>& operator[](Variable<int> idx) const {
    return elem;
  }

  Variable<T>& operator*() {
    return elem;
  }

  const Variable<T>& operator*() const {
    return elem;
  }

  VariablePtr<T>&
  operator=(const VariablePtr& ptr){
    return *this;
  }

  template <class U,
    typename = std::enable_if<std::is_convertible<T*,U>::value>>
  VariablePtr<T>&
  operator=(const U& ptr){
    return *this;
  }

  template <class U,
    typename = std::enable_if<std::is_convertible<T*,U>::value>>
  const VariablePtr<T>&
  operator=(const U& ptr) const {
    return *this;
  }

  template <class U,
    typename = std::enable_if<std::is_convertible<T*,U>::value>>
  bool
  operator==(const U& ptr) const {
    return false;
  }

  template <class U,
    typename = std::enable_if<std::is_convertible<T*,U>::value>>
  bool
  operator!=(const U& ptr) const {
    return true;
  }

  constexpr operator void*() const {
    return 0;
  }

  VariablePtr<T>&
  operator+=(const ptrdiff_t &offset){
    return *this;
  }

  VariablePtr<T>&
  operator-=(const ptrdiff_t &offset){
    return *this;
  }

 private:
  uint64_t &nops;
  Variable<T> elem;
};

template <class T>
void*
memset(const VariablePtr<T>& t, int value, size_t size){
  t.nops += size / sizeof(T);
  return 0;
}

template <class T>
void*
memcpy(const VariablePtr<T>& dst, const VariablePtr<T>& src, size_t size){
  dst.nops += size / sizeof(T);
  return 0;
}

typedef Variable<double> Double;
typedef VariablePtr<double> DoublePtr;
typedef Variable<int> Int;
typedef VariablePtr<int> IntPtr;


#endif