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
#ifndef sstmac_string_h_included
#define sstmac_string_h_included

#ifndef SSTMAC_INSIDE_STL
#define SSTMAC_INSIDE_STL
#include <sstmac/replacements/sstmac_pthread_clear.h>
#include <sstmac/replacements/clear_symbol_macros.h>
#define STRING_H_OWNS_STL
#endif

#include_next <string.h>

#ifdef SSTMAC_INSIDE_STL
#define sstmac_must_return_memcpy
#define sstmac_must_return_memset
#else
#ifndef memset
#define memset sstmac_memset
#endif
#ifndef memcpy
#define memcpy sstmac_memcpy
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif


#if defined(__clang__) // Remove pragma warnings if clang
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunknown-pragmas"
#endif

#pragma sst null_ptr safe
void* sstmac_memset(void* ptr, int value, unsigned long  sz);

#pragma sst null_ptr safe
void* sstmac_memcpy(void* dst, const void* src, unsigned long sz);

#if defined(__clang__) // Add back the warnings
#pragma clang diagnostic pop
#endif

#ifdef __cplusplus
}
#endif

#ifdef STRING_H_OWNS_STL
#undef STRING_H_OWNS_STL
#undef SSTMAC_INSIDE_STL
#include <sstmac/replacements/sstmac_pthread_return.h>
#include <sstmac/replacements/return_symbol_macros.h>
#endif

#endif


