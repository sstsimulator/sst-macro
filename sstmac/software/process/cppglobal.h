/**
Copyright 2009-2024 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2024, NTESS

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
#ifndef sstmac_software_process_CPPGLOBAL_H
#define sstmac_software_process_CPPGLOBAL_H


#include <sstmac/software/process/global.h>
#include <new>

#if __cplusplus >= 201103L
#include <tuple>
#include <functional>

namespace sstmac {

/**
 * @brief The CppGlobalRegisterGuard struct
 * Constructor causes the global variable to be registered and requires init for each new thread
 * Destructor causes the global variable to be unregistestered
 */
struct CppGlobalRegisterGuard {
  CppGlobalRegisterGuard(int& offset, int size,
                         bool tls, const char* name, std::function<void(void*)>&& fxn);

  ~CppGlobalRegisterGuard();

 private:
  bool tls_;
  int offset_;
};

template <class T, bool tls>
struct CppInplaceGlobalInitializer {
  CppInplaceGlobalInitializer(const char* name)
  {
    offset = GlobalVariable::init(sizeof(T), name, tls);
  }
  static int offset;
};
template <class T, bool tls> int CppInplaceGlobalInitializer<T,tls>::offset;

template <class Tag, class T, bool tls>
int inplaceCppGlobal(const char* name, std::function<void(void*)>&& fxn){
  static CppInplaceGlobalInitializer<T,tls> init{name};
  static CppGlobalRegisterGuard holder(init.offset, sizeof(T), tls, name, std::move(fxn));
  return init.offset;
}

/** a special wrapper for variable template members */
template <class Tag, class T, bool tls>
class CppVarTemplate {
 public:
  template <class... CtorArgs>
  static void invoke(void* ptr, CtorArgs&&... args){
    new (ptr) T(std::forward<CtorArgs>(args)...);
  }

  template <class... Args>
  CppVarTemplate(Args&&... args){
    std::function<void(void*)> f = std::bind(&invoke<Args&...>, std::placeholders::_1, std::forward<Args>(args)...);
    offset_ = inplaceCppGlobal<Tag,T,tls>("no name", std::move(f));
  }

  int getOffset() const {
    return offset_;
  }

  T& operator()(){
    return tls ? get_tls_ref_at_offset<T>(offset_) : get_global_ref_at_offset<T>(offset_);
  }

  int offset_;
};


}

#endif //C++11

#endif // CPPGLOBAL_H
