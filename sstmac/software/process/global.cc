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

#include <sstmac/software/process/global.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>

int sstmac_global_stacksize = 0;

namespace sstmac {

int GlobalVariable::stackOffset = 0;
int GlobalVariable::allocSize = 4096;
char* GlobalVariable::globalInits = nullptr;
std::list<GlobalVariable::relocation> GlobalVariable::relocations;
std::list<GlobalVariable::relocationCfg> GlobalVariable::relocationCfgs;
std::list<CppGlobal*> GlobalVariable::cppCtors;

GlobalVariable::GlobalVariable(int &offset, const int size, const char* name, const void *initData)
{

  offset = stackOffset;

  int rem = size % 4;
  int offsetIncrement = rem ? (size + (4-rem)) : size; //align on 32-bits

  bool realloc = globalInits == nullptr;
  while ((offsetIncrement + stackOffset) > allocSize){
    realloc = true;
    allocSize *= 2; //grow until big enough
  }

  if (realloc){
    char* old = globalInits;
    globalInits = new char[allocSize];
    if (old){
      //move everything in that was already there
      ::memcpy(globalInits, old, stackOffset);
      delete[] old;
    }
  }

  //printf("Allocated global variable %s of size %d at offset %d - %s\n",
  //       name, size, offset, (realloc ? "reallocated to fit" : "already fits"));
  fflush(stdout);

  if (initData){
    void* initStart = (char*)globalInits + stackOffset;
    ::memcpy(initStart, initData, size);
  }

  stackOffset += offsetIncrement;
}

void registerCppGlobal(CppGlobal* g){
  GlobalVariable::registerCtor(g);
}

void
GlobalVariable::registerRelocation(void* srcPtr, void* srcBasePtr, int& srcOffset,
                                   void* dstPtr, void* dstBasePtr, int& dstOffset)
{
  relocationCfgs.emplace_back(srcPtr, srcBasePtr, srcOffset,
                              dstPtr, dstBasePtr, dstOffset);
}

void
GlobalVariable::relocatePointers(void* globals)
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
    void* src = &segment[r.srcOffset];
    void** dst = (void**) &segment[r.dstOffset];
    *dst = src;
  }
}

void
GlobalVariable::callCtors(void *globals)
{
  for (CppGlobal* g : cppCtors){
    g->allocate(globals);
  }
}

GlobalVariable::~GlobalVariable()
{
  if (globalInits){
    delete[] globalInits;
    globalInits = nullptr;
  }
}

}
