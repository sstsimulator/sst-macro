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

#include <sstmac/software/process/global.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/cppglobal.h>

extern "C" {

int sstmac_global_stacksize = 0;
char* static_init_glbls_segment = nullptr;
char* static_init_tls_segment = nullptr;
void allocate_static_init_tls_segment(){
  static_init_tls_segment = new char[int(1e6)];
}
void allocate_static_init_glbls_segment(){
  static_init_glbls_segment = new char[int(1e6)];
}

}

namespace sstmac {

GlobalVariableContext GlobalVariable::glblCtx;
GlobalVariableContext GlobalVariable::tlsCtx;
bool GlobalVariable::inited = false;

int
GlobalVariable::init(const int size, const char* name, bool tls)
{
  if (!inited){
    tlsCtx.init();
    glblCtx.init();
    inited = true;
  }
  if (tls) return tlsCtx.append(size, name);
  else return glblCtx.append(size, name);
}

void
GlobalVariableContext::init()
{
  stackOffset = 0;
  allocSize_ = 4096;
  globalInits = nullptr;
}

void
GlobalVariableContext::registerInitFxn(int offset, std::function<void (void *)> &&fxn)
{
  initFxns[offset] = std::move(fxn);
}

int
GlobalVariableContext::append(const int size, const char* name)
{
  int offset = stackOffset;

  int rem = size % 4;
  int offsetIncrement = rem ? (size + (4-rem)) : size; //align on 32-bits

  bool realloc = false;
  while ((offsetIncrement + stackOffset) > allocSize_){
    realloc = true;
    allocSize_ *= 2; //grow until big enough
  }

  if (realloc && !activeGlobalMaps_.empty()){
    spkt_abort_printf("dynamically loaded global variable overran storage space\n"
                      "to fix, explicitly set globals_size = %d in app params",
                      allocSize_);
  }

  if (realloc || globalInits == nullptr){
    char* old = globalInits;
    globalInits = new char[allocSize_];
    if (old){
      //move everything in that was already there
      ::memcpy(globalInits, old, stackOffset);
      delete[] old;
    }
  }

  //printf("Allocated global variable %s of size %d at offset %d - %s\n",
  //       name, size, offset, (realloc ? "reallocated to fit" : "already fits"));
  //fflush(stdout);

  stackOffset += offsetIncrement;

  return offset;
}

CppGlobalRegisterGuard::CppGlobalRegisterGuard(int& offset, int size, bool tls, const char* name,
                                               std::function<void(void*)>&& fxn) :
  tls_(tls), offset_(offset)
{
  offset_ = offset = GlobalVariable::init(size, name, tls);
  if (tls){
    GlobalVariable::tlsCtx.registerInitFxn(offset, std::move(fxn));
  } else {
    GlobalVariable::glblCtx.registerInitFxn(offset, std::move(fxn));
  }
}

CppGlobalRegisterGuard::~CppGlobalRegisterGuard()
{
  if (tls_){
    GlobalVariable::tlsCtx.unregisterInitFxn(offset_);
  } else {
    GlobalVariable::glblCtx.unregisterInitFxn(offset_);
  }
}

void
GlobalVariableContext::callInitFxns(void *globals)
{
  for (auto& pair : initFxns){
    int offset = pair.first;
    char* ptr = ((char*)globals) + offset;
    (pair.second)(ptr);
  }
}

GlobalVariableContext::~GlobalVariableContext()
{
  if (globalInits){
    delete[] globalInits;
    globalInits = nullptr;
  }
}

void
GlobalVariableContext::initGlobalSpace(void* ptr, int size, int offset)
{
  for (void* globals : activeGlobalMaps_){
    char* dst = ((char*)globals) + offset;
    ::memcpy(dst, ptr, size);
  }

  //also do the global init for any new threads spawned
  char* dst = ((char*)globalInits) + offset;
  ::memcpy(dst, ptr, size);
}

}

#include <dlfcn.h>

extern "C" void *sstmac_dlopen(const char* filename, int flag)
{
  void* ret = dlopen(filename, flag);
  return ret;
}

extern "C" void sstmac_init_global_space(void* ptr, int size, int offset, bool tls)
{
  if (tls) sstmac::GlobalVariable::tlsCtx.initGlobalSpace(ptr, size, offset);
  else sstmac::GlobalVariable::glblCtx.initGlobalSpace(ptr, size, offset);
}
