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

#include <sstmac/util.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/app.h>
#include <cstring>
#include <sstmac/null_buffer.h>

typedef int (*main_fxn)(int,char**);
typedef int (*empty_main_fxn)();

extern "C" double
sstmac_now(){
  return sstmac::sw::operating_system::current_os()->now().sec();
}

sprockit::sim_parameters*
get_params(){
  return sstmac::sw::operating_system::current_thread()->parent_app()->params();
}

extern "C"
void* sstmac_memset(void* ptr, int value, unsigned long sz){
#ifdef memset
#error #sstmac memset macro should not be defined in util.cc - refactor needed
#endif
  if (isNonNullBuffer(ptr)) std::memset(ptr,value,sz);
  if (sz > 128){
    //model this as a delay
    sstmac::sw::operating_system::current_thread()->parent_app()
        ->compute_block_write(sz);
  }
  return ptr;
}

extern "C"
void* sstmac_memcpy(void* dst, const void* src, unsigned long sz){
#ifdef memcpy
#error #sstmac memcpy macro should not be defined in util.cc - refactor needed
#endif
  if (isNonNullBuffer(dst) && isNonNullBuffer(src)) memcpy(dst,src,sz);
  if (sz >= 128){
    //model this as a delay
    sstmac::sw::operating_system::current_thread()->parent_app()
        ->compute_block_memcpy(sz);
  }
  return dst;
}

namespace std {
void* sstmac_memset(void* ptr, int value, unsigned long  sz){
  return ::sstmac_memset(ptr, value, sz);
}

void* sstmac_memcpy(void *dst, const void *src, unsigned long sz){
#ifdef memcpy
#error #sstmac memcpy macro should not be defined in util.cc - refactor needed
#endif
  return ::sstmac_memcpy(dst, src, sz);
}

void sstmac_free(void* ptr){
  if (isNonNullBuffer(ptr)){
    ::free(ptr);
  }
}
}

extern "C" void sstmac_exit(int code)
{
  sstmac::sw::operating_system::current_thread()->kill(code);
}

extern "C" unsigned int sstmac_alarm(unsigned int delay)
{
  //for now, do nothing
  //seriously, why are you using the alarm function?
  return 0;
}

extern "C" int sstmac_on_exit(void(*fxn)(int,void*), void* arg)
{
  //for now, just ignore any atexit functions
  return 0;
}

extern "C" int sstmac_atexit(void (*fxn)(void))
{
  //for now, just ignore any atexit functions
  return 0;
}

extern "C"
int sstmac_gethostname(char* name, size_t len)
{
  int addr = sstmac::sw::operating_system::current_os()->addr();
  std::string sst_name = sprockit::printf("nid%d", addr);
  if (sst_name.size() > len){
    return -1;
  } else {
    ::strcpy(name, sst_name.c_str());
    return 0;
  }
}

extern "C"
void sstmac_free(void* ptr){
#ifdef free
#error #sstmac free macro should not be defined in util.cc - refactor needed
#endif
  if (isNonNullBuffer(ptr)) free(ptr);
}

#include <unordered_map>

extern "C"
void sstmac_advance_time(const char* param_name)
{
  sstmac::sw::thread* thr = sstmac::sw::operating_system::current_thread();
  sstmac::sw::app* parent = thr->parent_app();
  using ValueCache = std::unordered_map<void*,sstmac::timestamp>;
  static std::map<sstmac::sw::app_id,ValueCache> cache;
  auto& subMap = cache[parent->aid()];
  auto iter = subMap.find((void*)param_name);
  if (iter == subMap.end()){
    subMap[(void*)param_name] = parent->params()->get_time_param(param_name);
    iter = subMap.find((void*)param_name);
  }
  parent->compute(iter->second);
}

int
user_skeleton_main_init_fxn(const char* name, main_fxn fxn)
{
  sstmac::sw::user_app_cxx_full_main::register_main_fxn(name, fxn);
  return 42;
}

static empty_main_fxn empty_skeleton_main;

int
user_skeleton_main_init_fxn(const char* name, empty_main_fxn fxn)
{
  sstmac::sw::user_app_cxx_empty_main::register_main_fxn(name, fxn);
  return 42;
}
