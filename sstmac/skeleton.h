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

#ifndef sstmac_skeleton_h
#define sstmac_skeleton_h

#define SSTPP_QUOTE(name) #name
#define SSTPP_STR(name) SSTPP_QUOTE(name)
#define SST_APP_NAME_QUOTED SSTPP_STR(sstmac_app_name)
#define ELI_NAME(app) app##_eli
typedef int (*main_fxn)(int,char**);
typedef int (*empty_main_fxn)();

#include <sstmac/common/sstmac_config.h>
#ifndef __cplusplus
#include <stdbool.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

double sstmac_sim_time();

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

#include <sstmac/software/process/cppglobal.h>

#include <new>
#include <utility>

namespace sstmac {

class vector {
 public:
  vector() : size_(0) {}

  void resize(unsigned long sz){
    size_ = sz;
  }

  unsigned long size() const {
    return size_;
  }

  template <class... Args>
  void push_back(Args... args){
    ++size_;
  }

  template <class... Args>
  void emplace_back(Args... args){
    ++size_;
  }

  bool empty() const {
    return size_ == 0;
  }

  void clear() {
    size_ = 0;
  }

 private:
  unsigned long  size_;
};
}


#include <sprockit/sim_parameters.h>
#include <sstmac/software/process/global.h>
#include <sstmac/software/api/api_fwd.h>

/** Automatically inherit runtime types */
using sprockit::sim_parameters;


#define define_var_name_pass_through(x) sstmac_dont_ignore_this##x
#define define_var_name(x) define_var_name_pass_through(x)

#ifndef SSTMAC_NO_REFACTOR_MAIN
#undef main
#define main USER_MAIN
#define USER_MAIN(...) \
 fxn_that_nobody_ever_uses_to_make_magic_happen(); \
 typedef int (*this_file_main_fxn)(__VA_ARGS__); \
 int user_skeleton_main_init_fxn(const char* name, this_file_main_fxn fxn); \
 static int user_skeleton_main(__VA_ARGS__); \
 static int dont_ignore_this = \
  user_skeleton_main_init_fxn(SST_APP_NAME_QUOTED, user_skeleton_main); \
 static int user_skeleton_main(__VA_ARGS__)
#endif

extern sprockit::sim_parameters* get_params();

#else
static void* nullptr = 0;

#define main ignore_for_app_name; const char* sstmac_appname_str = SST_APP_NAME_QUOTED; int main
#endif


#ifdef __cplusplus
#include <cstdint>
#ifndef _Bool
using _Bool = bool;
#endif
extern "C" {
#else
#include <stdint.h>
#endif

extern int sstmac_global_stacksize;
extern char* static_init_glbls_segment;
extern char* static_init_tls_segment;
void allocate_static_init_glbls_segment();
void allocate_static_init_tls_segment();
void sstmac_init_global_space(void* ptr, int size, int offset, bool tls);
void sstmac_advance_time(const char* param_name);

#ifdef __cplusplus
}
#endif


#ifdef __STRICT_ANSI__
#define SSTMAC_INLINE
#else
#define SSTMAC_INLINE inline
#endif



static SSTMAC_INLINE char* get_sstmac_global_data(){
  if (sstmac_global_stacksize == 0){
    if (static_init_glbls_segment == 0){
      allocate_static_init_glbls_segment();
    }
    return static_init_glbls_segment;
  } else {
    int stack; int* stackPtr = &stack;
    uintptr_t localStorage = ((uintptr_t) stackPtr/sstmac_global_stacksize)*sstmac_global_stacksize;
    char** globalMapPtr = (char**)(localStorage + sizeof(int));
    return *globalMapPtr;
  }
}

static SSTMAC_INLINE char* get_sstmac_tlsl_data(){
  if (sstmac_global_stacksize == 0){
    if (static_init_tls_segment == 0){
      allocate_static_init_tls_segment();
    }
    return static_init_tls_segment;
  } else {
    int stack; int* stackPtr = &stack;
    uintptr_t localStorage = ((uintptr_t) stackPtr/sstmac_global_stacksize)*sstmac_global_stacksize;
    char** globalMapPtr = (char**)(localStorage + sizeof(int) + sizeof(void*));
    return *globalMapPtr;
  }
}

#undef SSTMAC_INLINE

#endif


