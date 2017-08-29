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

#ifndef SSTMAC_SOFTWARE_THREADING_THREADING_PTHREAD_H_INCLUDED
#define SSTMAC_SOFTWARE_THREADING_THREADING_PTHREAD_H_INCLUDED

#include <sstmac/software/threading/threading_interface.h>
#include <vector>
#include <sprockit/factories/factory.h>

#ifdef SSTMAC_HAVE_PTHREAD
#include <pthread.h>
#endif

namespace sstmac {
namespace sw {

#ifdef SSTMAC_HAVE_PTHREAD

class threading_pthread : public threading_interface
{

 public:
  FactoryRegister("pthread", threading_interface, threading_pthread)

  threading_pthread(sprockit::sim_parameters* params);

  void init_context() override;

  void destroy_context() override;

  threading_interface* copy(sprockit::sim_parameters* params) override {
    return new threading_pthread(params);
  }

  void start_context(int physical_thread_id, void *stack, size_t stacksize, void
                 (*func)(void*), void *args, threading_interface *yield_to,
                void* globals_storage) override;

  void complete_context(threading_interface *to) override;

  void swap_context(threading_interface *to) override;

 private:
  struct threadcontext_t
  {
    pthread_t thread;
    pthread_cond_t ready;
    pthread_mutex_t* context_switch_lock;
    bool started;
    bool waiting;
  };

  class threadargs
  {
   public:
    void (*func)(void*);
    void *args;
    threadcontext_t *context;
    threadargs(void
               (*f)(void*), void *a, threadcontext_t *c) {
      func = f;
      args = a;
      context = c;
    }
  };
  /// This part of context initialization is common to
  /// init_context and start_context.
  void init_context_common(threadcontext_t &t);

  static void wait_signal(threadcontext_t* context);
  static void send_signal(threadcontext_t* context);
  static void* pthread_run_func(void*args);

  threadcontext_t context_;
  static pthread_mutex_t context_switch_mutex_;

};

#endif
}
} // end of namespace sstmac
#endif
