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

#ifndef sstmac_skeleton_tls_h
#define sstmac_skeleton_tls_h

#include <sstmac/common/sstmac_config.h>
#include <sstmac/software/process/tls.h>
#include <unusedvariablemacro.h>

#ifndef SSTMAC_INLINE
#ifdef __STRICT_ANSI__
#define SSTMAC_INLINE
#else
#define SSTMAC_INLINE inline
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

extern char* static_init_glbls_segment;
extern char* static_init_tls_segment;
void allocate_static_init_glbls_segment();
void allocate_static_init_tls_segment();

#ifdef __cplusplus
}
#endif

SSTMAC_MAYBE_UNUSED
static SSTMAC_INLINE char* get_sstmac_global_data(){
  if (sstmac_global_stacksize == 0){
    if (static_init_glbls_segment == 0){
      allocate_static_init_glbls_segment();
    }
    return static_init_glbls_segment;
  } else {
    char** globalMapPtr = (char**)(get_sstmac_tls() + SSTMAC_TLS_GLOBAL_MAP);
    return *globalMapPtr;
  }
}

SSTMAC_MAYBE_UNUSED
static SSTMAC_INLINE char* get_sstmac_tls_data(){
  if (sstmac_global_stacksize == 0){
    if (static_init_tls_segment == 0){
      allocate_static_init_tls_segment();
    }
    return static_init_tls_segment;
  } else {
    char** globalMapPtr = (char**)(get_sstmac_tls() + SSTMAC_TLS_TLS_MAP);
    return *globalMapPtr;
  }
}

SSTMAC_MAYBE_UNUSED
static SSTMAC_INLINE int get_sstmac_tls_thread_id(){
  int* idPtr = (int*)(get_sstmac_tls() + SSTMAC_TLS_THREAD_ID);
  return *idPtr;
}

#undef SSTMAC_INLINE

#endif
