/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef SSTMAC_SOFTWARE_PROCESS_GLOBAL_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_GLOBAL_H_INCLUDED

#include <sstream>
#include <sstmac/software/process/tls.h>

extern int sstmac_global_stacksize;

namespace sstmac {

class GlobalVariable {
 public:
  GlobalVariable(int& offset, const int size, const void* initData);

  static int globalsSize() {
    return stackOffset;
  }

  static void* globalInit() {
    return globalInits;
  }

 private:
  static int stackOffset;
  #define SSTMAC_MAX_GLOBALS 16384 // For now assume 16KB is the max
  static char globalInits[16384];

};

static inline void* get_global_at_offset(int offset){
  int stack; int* stackPtr = &stack;
  size_t stackTopInt = ((size_t)stackPtr/sstmac_global_stacksize)*sstmac_global_stacksize + TLS_GLOBAL_MAP;
  char** stackTopPtr = (char**) stackTopInt;
  char* globalMap = *stackTopPtr;
  return globalMap + offset;
}

template <class T>
static inline T& get_global_ref_at_offset(int offset){
  T* t = (T*) get_global_at_offset(offset);
  return *t;
}

namespace sw {

template <class T,class enable=void>
struct global {};

template <class T>
struct global<T*,void> : public GlobalVariable {
  explicit global() : GlobalVariable(offset,sizeof(T*),nullptr){}

  explicit global(T* t) : GlobalVariable(offset, sizeof(T*), &t){}

  T*& get() {
    return get_global_ref_at_offset<T*>(offset);
  }

  operator T*() const {
    return get_global_ref_at_offset<T*>(offset);
  }

  template <class U>
  T*& operator=(U*& u) {
    T*& t = get_global_ref_at_offset<T*>(offset);
    t = u;
    return t;
  }

  template <class U>
  T*& operator=(U& u) {
    T*& t = get_global_ref_at_offset<T*>(offset);
    t = u;
    return t;
  }

  int offset;
};

template <class T>
struct global<T,typename std::enable_if<std::is_arithmetic<T>::value>::type> : public GlobalVariable {

  explicit global() : GlobalVariable(offset, sizeof(T), nullptr){}

  explicit global(const T& t) : GlobalVariable(offset, sizeof(T), &t){}

  template <class U>
  T& operator=(const U& u){
    T& t = get_global_ref_at_offset<T>(offset);
    t = u;
    return t;
  }

  template <class U>
  operator U() const {
    return get_global_ref_at_offset<T>(offset);
  }

  T& get() const {
    return get_global_ref_at_offset<T>(offset);
  }

  template <class U>
  bool operator<(const U& u){
    return get_global_ref_at_offset<T>(offset) < u;
  }

  template <class U>
  void operator+=(const U& u){
    get_global_ref_at_offset<T>(offset) += u;
  }

  T& operator++(){
    T& t = get_global_ref_at_offset<T>(offset);
    t++;
    return t;
  }

  T operator++(int offset){
    T& t = get_global_ref_at_offset<T>(offset);
    T result(t);
    t++;
    return result;
  }

  T* operator&() {
    return (T*) get_global_at_offset(offset);
  }

  int offset;
};

#define OPERATOR(op,ret) \
  template <typename T, typename U> ret \
  operator op(const global<T>& l, const U& r){ \
    return l.get() op r; \
  } \
  template <typename T, typename U> ret \
  operator op(const U& l, const global<T>& r){ \
    return l op r.get(); \
  }

OPERATOR(+,T)
OPERATOR(-,T)
OPERATOR(*,T)
OPERATOR(&,T)
OPERATOR(<,bool)
OPERATOR(<=,bool)
OPERATOR(>,bool)
OPERATOR(>=,bool)
OPERATOR(==,bool)

}
}

typedef sstmac::sw::global<int> global_int;
typedef sstmac::sw::global<const char*> global_cstr;

#endif

