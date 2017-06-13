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

#include <sstmac/software/threading/threading_ucontext.h>
#include <sstmac/software/process/thread_info.h>

namespace sstmac {
namespace sw {

/// Initializing a context.
void
threading_ucontext::init_context()
{
  if (getcontext(&context_) != 0) {
    spkt_throw_printf(sprockit::os_error,
      "threading_ucontext::init_context: %s",
      ::strerror(errno));
  }
}


/// Start a new context.
void
threading_ucontext::start_context(int physical_thread_id,
   void *stack, size_t stacksize, void
   (*func)(void*), void *args, threading_interface *yield_to,
   void* globals_storage)
{
  thread_info::register_user_space_virtual_thread(physical_thread_id, stack, globals_storage);

  funcptr funcp(func);
  voidptr voidp(args);
  context_.uc_stack.ss_sp = stack;
  context_.uc_stack.ss_size = stacksize;
  init_context();

  threading_ucontext* casted = (threading_ucontext*)yield_to;
  if(yield_to) {
    context_.uc_link = &casted->context_;
  }
  else {
    context_.uc_link = NULL;
  }
  makecontext(&context_, (void
        (*)()) (context_springboard), 4, funcp.fpair.a, funcp.fpair.b,
        voidp.vpair.a, voidp.vpair.b);
}

/// Swap context.
void
threading_ucontext::swap_context(threading_interface *to)
{
  threading_ucontext* casted = (threading_ucontext*)to;
  if (swapcontext(&context_, &casted->context_) == -1) {
    spkt_throw_printf(sprockit::os_error,
       "threading_ucontext::swap_context: %s",
       strerror(errno));
  }
}


} }