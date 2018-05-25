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

#include <sprockit/errors.h>
#include <sprockit/debug.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/software/process/thread_info.h>

#ifdef SSTMAC_HAVE_PTHREAD
MakeDebugSlot(pth);

#include <sstmac/software/threading/threading_interface.h>
#include <vector>
#include <sprockit/factories/factory.h>
#include <pthread.h>

namespace sstmac {
namespace sw {

class threading_pthread : public thread_context
{
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

 public:
  FactoryRegister("pthread", thread_context, threading_pthread)

  threading_pthread(sprockit::sim_parameters* params){
    static bool inited = false;
    if (!inited){
      inited = true;
      pthread_mutex_init(&context_switch_mutex_, NULL);
    }
  }

  void init_context() override {
    init_context_common(context_);
    context_.thread = pthread_self();
  }

  void destroy_context() override {
    pthread_cond_destroy(&context_.ready);
    // This can be called for the main thread's context as well,
    // in which case we cannot join.
    if (!pthread_equal(pthread_self(), context_.thread)) {
      pthread_join(context_.thread, NULL);
    }
  }

  thread_context* copy() const override {
    return new threading_pthread(nullptr);
  }

  void start_context(int physical_thread_id, void *stack, size_t stacksize, void
             (*func)(void*), void *args, void* globals_storage, void* tls_storage,
             thread_context* from) override {
    if (globals_storage || tls_storage){
      spkt_abort_printf("cannot use global variables with pthread");
    }

    pthread_attr_t thread_attr;
    pthread_attr_init(&thread_attr);
    //pthread_attr_setstack(&thread_attr, stack, stacksize);
    init_context_common(context_);

    threadargs *targs = new threadargs(func, args, &context_);
    pthread_create(&context_.thread, &thread_attr, &pthread_run_func, (void*) targs);
    pthread_attr_destroy(&thread_attr);

    resume_context(from);
  }

  void complete_context(thread_context* to) override {
    if (!pthread_equal(pthread_self(), context_.thread)) {
      sprockit::abort("threading_pthread::complete_context: done from thread other than \"from\" thread");
    }
    threading_pthread* casted = (threading_pthread*)to;
    if (casted) {
      send_signal(&casted->context_);
    }
    else {
      spkt_throw_printf(sprockit::illformed_error,
                       "received non-pthread context on complete_context");
    }
    pthread_exit(0);
  }

  void resume_context(thread_context* from) override {
    swap_context(static_cast<threading_pthread*>(from), this);
  }

  void pause_context(thread_context* to) override {
    swap_context(this, static_cast<threading_pthread*>(to));
  }

 private:
  static void swap_context(threading_pthread* from, threading_pthread* to) {
    if (!pthread_equal(pthread_self(), from->context_.thread)) {
      sprockit::abort("threading_pthread::swap_context: done from thread other than \"from\" thread");
    }

    // incorrect ordering for c1->c2->c1 context switches:
    // c1 signals c2 to start
    // c2 signals c1 to start
    // c1 begins waiting on signals (deadlock)
    //
    // correct order:
    // (lock) c1 signals c2 to start
    // c1 begins waiting on signals (unlock)
    // (lock) c2 signals c1 to start
    // c2 begins waiting on signals (unlock)

    // Grab the context lock.
    pthread_mutex_lock(from->context_.context_switch_lock);

    to->context_.started = true;

    // Wake up the to context.
    send_signal(&to->context_);

    // Put the from thread to sleep.
    wait_signal(&from->context_);

    pthread_mutex_unlock(from->context_.context_switch_lock);
  }

  /// This part of context initialization is common to
  /// init_context and start_context.
  void init_context_common(threadcontext_t& context){
    context_.context_switch_lock = &context_switch_mutex_;
    pthread_cond_init(&context.ready, NULL);
    context.started = false;
    context.waiting = true;
  }

  static void wait_signal(threadcontext_t* context){
    //pthread_cond_wait can just randomly drop out on certain OSes
    //you have to actually implement this by looping on a variable
    //so that if cond_wait erroneoulsy returns the bool bounces it back
    do {
      pthread_cond_wait(&context->ready, context->context_switch_lock);
    }
    while (context->waiting);
    //go back to wait mode
    context->waiting = true;
  }

  static void send_signal(threadcontext_t* context){
    context->waiting = false;
    pthread_cond_signal(&context->ready);
  }

  static void* pthread_run_func(void*args){
    threading_pthread::threadargs *thread_info = (threading_pthread::threadargs*) (
          args);
    void* thread_args = thread_info->args;
    void (*func)(void*) = thread_info->func;

    // The following code handles the initial context switch
    // starting this thread. After that, swap_context
    // performs all context switches.

    // Grab the context lock.
    pthread_mutex_lock(thread_info->context->context_switch_lock);

    // Put this thread to sleep, if it has not
    // already been started.
    if (!thread_info->context->started) {
      wait_signal(thread_info->context);
    }

    pthread_mutex_unlock(thread_info->context->context_switch_lock);

    //delete the thread info here
    //Once we enter this function, we shall never return, dear Frodo
    delete thread_info;

    // Run the thread
    (*func)(thread_args);

    spkt_throw_printf(sprockit::illformed_error,
      "pthreadfunc arrived at end of function\n"
      "complete_context should have terminated this with pthread_exit");

    return NULL;
  }

  threadcontext_t context_;
  static pthread_mutex_t context_switch_mutex_;

};

pthread_mutex_t threading_pthread::context_switch_mutex_;

}
} // end of namespace sstmac

#endif


