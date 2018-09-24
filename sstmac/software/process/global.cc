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

/**
int GlobalVariable::stackOffset = 0;
int GlobalVariable::allocSize_ = 4096;
char* GlobalVariable::globalInits = nullptr;
std::list<GlobalVariable::relocation> GlobalVariable::relocations;
std::list<GlobalVariable::relocationCfg> GlobalVariable::relocationCfgs;
std::list<CppGlobal*> GlobalVariable::cppCtors;
std::unordered_set<void*> GlobalVariable::activeGlobalMaps_;
*/

GlobalVariableContext GlobalVariable::glblCtx;
GlobalVariableContext GlobalVariable::tlsCtx;
bool GlobalVariable::inited = false;

int
GlobalVariable::init(const int size, const char* name, const void *initData, bool tls)
{
  if (!inited){
    tlsCtx.init();
    glblCtx.init();
    inited = true;
  }
  if (tls) return tlsCtx.append(size, name, initData);
  else return glblCtx.append(size, name, initData);
}

void
GlobalVariableContext::init()
{
  stackOffset = 0;
  allocSize_ = 4096;
  globalInits = nullptr;
}

int
GlobalVariableContext::append(const int size, const char* name, const void *initData)
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

  if (initData){
    void* initStart = (char*)globalInits + stackOffset;
    ::memcpy(initStart, initData, size);
    for (void* seg : activeGlobalMaps_){
      char* dst = ((char*)seg) + stackOffset;
      ::memcpy(dst, initData, size);
    }
  }

  stackOffset += offsetIncrement;

  return offset;
}

void registerCppGlobal(CppGlobal* g, bool tls){
  if (tls){
    GlobalVariable::tlsCtx.registerCtor(g);
  } else {
    GlobalVariable::glblCtx.registerCtor(g);
  }
}

void
GlobalVariableContext::registerRelocation(void* srcPtr, void* srcBasePtr, int& srcOffset,
                                   void* dstPtr, void* dstBasePtr, int& dstOffset)
{
  relocationCfgs.emplace_back(srcPtr, srcBasePtr, srcOffset,
                              dstPtr, dstBasePtr, dstOffset);
}

void
GlobalVariableContext::dlopenRelocate()
{
  //after a dlopen, there might be new relocations to execute
  if (!relocationCfgs.empty()){
    for (auto& cfg : relocationCfgs){
      int dstFieldDelta = (intptr_t)cfg.dstPtr - (intptr_t)cfg.dstBasePtr;
      int srcFieldDelta = (intptr_t)cfg.srcPtr - (intptr_t)cfg.srcBasePtr;
      int src = cfg.srcOffset + srcFieldDelta;
      int dst = cfg.dstOffset + dstFieldDelta;
      relocations.emplace_back(src,dst);
      for (void* seg : activeGlobalMaps_){
        relocate(relocations.back(), (char*)seg);
      }
    }
    relocationCfgs.clear();
  }
}

void
GlobalVariableContext::relocatePointers(void* globals)
{
  if (!relocationCfgs.empty()){
    for (auto& cfg : relocationCfgs){
      int dstFieldDelta = (intptr_t)cfg.dstPtr - (intptr_t)cfg.dstBasePtr;
      int srcFieldDelta = (intptr_t)cfg.srcPtr - (intptr_t)cfg.srcBasePtr;
      int src = cfg.srcOffset + srcFieldDelta;
      int dst = cfg.dstOffset + dstFieldDelta;
      relocations.emplace_back(src,dst);
    }
    relocationCfgs.clear();
  }

  char* segment = (char*) globals;
  for (relocation& r : relocations){
    relocate(r,segment);
  }
}

void
GlobalVariableContext::callCtors(void *globals)
{
  for (CppGlobal* g : cppCtors){
    g->allocate(globals);
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
  sstmac::GlobalVariable::glblCtx.dlopenRelocate();
  sstmac::GlobalVariable::tlsCtx.dlopenRelocate();
  return ret;
}

extern "C" void sstmac_init_global_space(void* ptr, int size, int offset, bool tls)
{
  if (tls) sstmac::GlobalVariable::tlsCtx.initGlobalSpace(ptr, size, offset);
  else sstmac::GlobalVariable::glblCtx.initGlobalSpace(ptr, size, offset);
}
