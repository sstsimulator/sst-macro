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

#include <sstmac/software/process/thread_info.h>
#include <sstmac/software/process/tls.h>
#include <sstmac/software/process/global.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/common/sstmac_config.h>
#include <sprockit/errors.h>
#include <iostream>
#include <string.h>
#include <stdint.h>
#include <sprockit/thread_safe_new.h>
#include <list>

namespace sstmac {

#if (SSTMAC_TLS_OFFSET != SPKT_TLS_OFFSET)
#error sprockit and sstmac do not agree on stack TLS offset
#endif

static const int tls_sanity_check = 42042042;

static thread_lock globals_lock;

void
ThreadInfo::registerUserSpaceVirtualThread(int phys_thread_id, void *stack,
                                           void* globalsMap, void* tlsMap)
{
  size_t stack_mod = ((size_t)stack) % sstmac_global_stacksize;
  if (stack_mod != 0){
    spkt_throw_printf(sprockit::ValueError,
        "user space thread stack is not aligned on %llu bytes",
        sstmac_global_stacksize);
  }

  //essentially treat this as thread-local storage
  char* tls = (char*) stack;
  int* thr_id_ptr = (int*) &tls[SSTMAC_TLS_THREAD_ID];
  *thr_id_ptr = phys_thread_id;

  int* sanity_ptr = (int*) &tls[SSTMAC_TLS_SANITY_CHECK];
  *sanity_ptr = tls_sanity_check;

  //this is dirty - so dirty, but it works
  void** globalPtr = (void**) &tls[SSTMAC_TLS_GLOBAL_MAP];
  *globalPtr = globalsMap;

  void** tlsPtr = (void**) &tls[SSTMAC_TLS_TLS_MAP];
  *tlsPtr = tlsMap;

  void** statePtr = (void**) &tls[SSTMAC_TLS_IMPLICIT_STATE];
  *statePtr = nullptr;

  globals_lock.lock();
  if (globalsMap){
    GlobalVariable::glblCtx.addActiveSegment(globalsMap);
    GlobalVariable::glblCtx.callCtors(globalsMap);
    GlobalVariable::glblCtx.relocatePointers(globalsMap);
  }

  if (tlsMap){
    GlobalVariable::tlsCtx.addActiveSegment(tlsMap);
    GlobalVariable::tlsCtx.callCtors(tlsMap);
    GlobalVariable::tlsCtx.relocatePointers(tlsMap);
  }
  globals_lock.unlock();
}

void
ThreadInfo::deregisterUserSpaceVirtualThread(void* stack)
{
  globals_lock.lock();
  char* tls = (char*) stack;
  void** globalsPtr = (void**) &tls[SSTMAC_TLS_GLOBAL_MAP];
  void* globalsMap = *globalsPtr;
  void** tlsPtr = (void**) &tls[SSTMAC_TLS_TLS_MAP];
  void* tlsMap = *tlsPtr;
  if (globalsMap){
    GlobalVariable::glblCtx.removeActiveSegment(globalsMap);
  }
  if (tlsMap){
    GlobalVariable::tlsCtx.removeActiveSegment(tlsMap);
  }
  globals_lock.unlock();
}

}

