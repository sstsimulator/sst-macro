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

#ifndef sstmac_software_process_tls_H
#define sstmac_software_process_tls_H

/* start at 64 to avoid conflicts with other libs */
#define SSTMAC_TLS_OFFSET 64
#define SSTMAC_TLS_THREAD_ID SSTMAC_TLS_OFFSET
#define SSTMAC_TLS_GLOBAL_MAP SSTMAC_TLS_THREAD_ID + sizeof(int)
#define SSTMAC_TLS_TLS_MAP SSTMAC_TLS_GLOBAL_MAP + sizeof(void*)
#define SSTMAC_TLS_SANITY_CHECK SSTMAC_TLS_TLS_MAP + sizeof(void*)
#define SSTMAC_TLS_IMPLICIT_STATE SSTMAC_TLS_SANITY_CHECK + sizeof(int)
#define SSTMAC_TLS_END SSTMAC_TLS_IMPLICIT_STATE + sizeof(void*)
#define SSTMAC_TLS_SIZE (SSTMAC_TLS_END - SSTMAC_TLS_OFFSET)

#ifndef SSTMAC_INLINE
#ifdef __STRICT_ANSI__
#define SSTMAC_INLINE
#else
#define SSTMAC_INLINE inline
#endif
#endif

#ifdef __cplusplus
#include <cstdint>
extern "C" int sstmac_global_stacksize;
#else
#include <stdint.h>
extern int sstmac_global_stacksize;
#endif


static SSTMAC_INLINE uintptr_t get_sstmac_tls(){
  int stack; int* stackPtr = &stack;
  uintptr_t localStorage = ((uintptr_t) stackPtr/sstmac_global_stacksize)*sstmac_global_stacksize;
  return localStorage;
}

#endif
