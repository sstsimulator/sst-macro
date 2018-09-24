/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#ifndef SSTMAC_SOFTWARE_PROCESS_GLOBAL_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_GLOBAL_H_INCLUDED

#include <sstream>
#include <iostream>
#include <sstmac/software/process/tls.h>
#include <list>
#include <unordered_set>

extern "C" int sstmac_global_stacksize;

namespace sstmac {

class CppGlobal;
class GlobalVariableContext {
 public:
  void init();

  ~GlobalVariableContext();

  int append(const int size, const char* name, const void* initData);

  int globalsSize() {
    return stackOffset;
  }

  int allocSize() {
    return allocSize_;
  }

  void setAllocSize(int sz){
    allocSize_ = sz;
  }

  void* globalInit() {
    return globalInits;
  }

  void callCtors(void* globals);

  void addActiveSegment(void* globals){
    activeGlobalMaps_.insert(globals);
  }

  void removeActiveSegment(void* globals){
    activeGlobalMaps_.erase(globals);
  }

  void initGlobalSpace(void* ptr, int size, int offset);

  void relocatePointers(void* globals);

  void registerRelocation(void* srcPtr, void* srcBasePtr, int& srcOffset,
                                 void* dstPtr, void* dstBasePtr, int& dstOffset);

  void dlopenRelocate();

  void registerCtor(CppGlobal* g){
    cppCtors.push_back(g);
  }

 private:
  int stackOffset;
  char* globalInits;
  int allocSize_;
  std::list<CppGlobal*> cppCtors;

  struct relocation {
    int srcOffset;
    int dstOffset;
    relocation(int src, int dst) :
      srcOffset(src), dstOffset(dst) {}
  };
  std::list<relocation> relocations;

  static inline void relocate(relocation& r, char* segment)
  {
    void* src = &segment[r.srcOffset];
    void** dst = (void**) &segment[r.dstOffset];
    *dst = src;
  }

  struct relocationCfg {
    void* srcPtr;
    void* srcBasePtr;
    int& srcOffset;
    void* dstPtr;
    void* dstBasePtr;
    int& dstOffset;
    relocationCfg(void* s, void* bs, int& os,
                 void* d, void* bd, int& od) :
      srcPtr(s), srcBasePtr(bs), srcOffset(os),
      dstPtr(d), dstBasePtr(bd), dstOffset(od)
    {
    }
  };
  std::list<relocationCfg> relocationCfgs;

 private:
  std::unordered_set<void*> activeGlobalMaps_;

};

class GlobalVariable {
 public:
  static int init(const int size, const char* name, const void* initData, bool tls = false);

  static GlobalVariableContext glblCtx;
  static GlobalVariableContext tlsCtx;
  static bool inited;
};



class RelocationPointer {
 public:
  RelocationPointer(void* srcPtr, void* srcBasePtr, int& srcOffset,
                    void* dstPtr, void* dstBasePtr, int& dstOffset){

    GlobalVariableContext& ctx = GlobalVariable::glblCtx;
    ctx.registerRelocation(srcPtr, srcBasePtr, srcOffset,
                           dstPtr, dstBasePtr, dstOffset);
  }
};

static inline void* get_special_at_offset(int offset, int map_offset)
{
  int stack; int* stackPtr = &stack;
  intptr_t stackTopInt = ((intptr_t)stackPtr/sstmac_global_stacksize)*sstmac_global_stacksize + map_offset;
  char** stackTopPtr = (char**) stackTopInt;
  char* globalMap = *stackTopPtr;
  return globalMap + offset;
}

static inline void* get_global_at_offset(int offset){
  return get_special_at_offset(offset, TLS_GLOBAL_MAP);
}

static inline void* get_tls_at_offset(int offset){
  return get_special_at_offset(offset, TLS_TLS_MAP);
}

template <class T>
static inline T& get_global_ref_at_offset(int offset){
  T* t = (T*) get_global_at_offset(offset);
  return *t;
}

template <class T>
static inline T& get_tls_ref_at_offset(int offset){
  T* t = (T*) get_tls_at_offset(offset);
  return *t;
}

namespace sw {

template <class T,class enable=void>
struct global {};

template <class T>
struct global<T*,void> : public GlobalVariable {
  explicit global(){ 
    offset = GlobalVariable::init(sizeof(T*),"",nullptr);
  }

  explicit global(T* t){
    offset = GlobalVariable::init(sizeof(T*), "", &t);
  }

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
struct global<T,typename std::enable_if<std::is_arithmetic<T>::value>::type> {

  explicit global()
  {
    offset = GlobalVariable::init(sizeof(T), "", nullptr);
  }

  explicit global(const T& t)
  {
    offset = GlobalVariable::init(sizeof(T), "", &t);
  } 

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
