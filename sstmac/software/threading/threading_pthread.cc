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

#include <sprockit/errors.h>
#include <sprockit/debug.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/software/threading/threading_pthread.h>
#include <sstmac/software/process/thread_info.h>

MakeDebugSlot(pth);
#define pthread_ctx_debug(ctx,...) \
  debug_printf(sprockit::dbg::pth, "context=%p,thread=%p: %s", \
    ctx, pthread_self(), sprockit::printf(__VA_ARGS__).c_str())
#define pthread_debug(...) \
   pthread_ctx_debug(&context_, __VA_ARGS__)

namespace sstmac {
namespace sw {
#ifdef SSTMAC_HAVE_PTHREAD

std::vector<pthread_mutex_t> threading_pthread::context_switch_mutexes;

void
send_signal(threadcontext_t* context)
{
  context->waiting = false;
  pthread_cond_signal(&context->ready);
}

void
wait_signal(threadcontext_t* context)
{
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

threading_pthread::threading_pthread(int thread_id, int nthread) :
  thread_id_(thread_id),
  nthread_(nthread)
{
  static thread_lock lock;
  lock.lock();
  if (context_switch_mutexes.size() == 0){
    //not yet done
    context_switch_mutexes.resize(nthread);
    pthread_debug("initializing pthread app stack for %d real threads", nthread);
    for (int i=0; i < nthread; ++i){
      pthread_debug("init context switch mutex %d", i);
      pthread_mutex_init(&context_switch_mutexes[i], NULL);
    }
  }
  context_.context_switch_lock = 0;
  lock.unlock();
}

static void*
pthreadfunc(void*args)
{
  threading_pthread::threadargs *thread_info = (threading_pthread::threadargs*) (
        args);
  void* thread_args = thread_info->args;
  void (*func)(void*) = thread_info->func;

  pthread_ctx_debug(thread_info->context, "launched new pthread application stack");

  // The following code handles the initial context switch
  // starting this thread. After that, swap_context
  // performs all context switches.

  // Grab the context lock.
  pthread_mutex_lock(thread_info->context->context_switch_lock);

  pthread_ctx_debug(thread_info->context,
    "locked new pthread application stack");

  // Put this thread to sleep, if it has not
  // already been started.
  if (!thread_info->context->started) {
    pthread_ctx_debug(thread_info->context,
      "putting thread to sleep");
    wait_signal(thread_info->context);
  }

  pthread_ctx_debug(thread_info->context, "thread awoken - unlocking mutex");

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

/// This part of context initialization is common to
/// init_context and start_context.
void
threading_pthread::init_context_common(threadcontext_t &context)
{
  context_.context_switch_lock = &context_switch_mutexes[thread_id_];
  pthread_cond_init(&context.ready, NULL);
  context.started = false;
  context.waiting = true;
}

/// Initialize the context to be that for the currently running thread.
void
threading_pthread::init_context()
{
  init_context_common(context_);
  context_.thread = pthread_self();
}

/// This tears down the context. It is only called from the scheduler's thread.
void
threading_pthread::destroy_context()
{
  pthread_cond_destroy(&context_.ready);
  // This can be called for the main thread's context as well,
  // in which case we cannot join.
  if (!pthread_equal(pthread_self(), context_.thread)) {
    pthread_join(context_.thread, NULL);
  }
}

/// Start a new context. It does not start yet -- swap_context will start it. This is only called from the scheduler's thread.
void
threading_pthread::start_context(int physical_thread_id,
   void *stack, size_t stacksize, void
   (*func)(void*), void *args, threading_interface *yield_to,
   void* globals_storage)
{
  if (globals_storage){
    spkt_abort_printf("cannot use global variables with pthread");
  }

  pthread_attr_t thread_attr;
  pthread_attr_init(&thread_attr);
  //pthread_attr_setstack(&thread_attr, stack, stacksize);
  init_context_common(context_);

  pthread_debug("starting pthread %p with stack %p of size %lu on physical thread %d",
       &context_, &context_.thread, stack, stacksize, physical_thread_id);

  threadargs *targs = new threadargs(func, args, &context_);
  pthread_create(&context_.thread, &thread_attr, &pthreadfunc, (void*) targs);
  pthread_attr_destroy(&thread_attr);
}

void
threading_pthread::complete_context(threading_interface *to)
{
  if (!pthread_equal(pthread_self(), context_.thread)) {
    spkt_throw(sprockit::spkt_error, 
        "threading_pthread::complete_context: done from thread other than \"from\" thread");
  }
  threading_pthread* casted = (threading_pthread*)to;
  if(casted) {
    pthread_debug("completing context");
    send_signal(&casted->context_);
  }
  else {
    spkt_throw_printf(sprockit::illformed_error,
                     "received non-pthread context on complete_context");
  }
  pthread_exit(0);
}

/// Swap context. The from context is always the currently running context.
void
threading_pthread::swap_context(threading_interface *to)
{
  if (!pthread_equal(pthread_self(), context_.thread)) {
    spkt_throw(sprockit::spkt_error, 
        "threading_pthread::swap_context: done from thread other than \"from\" thread");
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

#if SSTMAC_SANITY_CHECK
  threading_pthread *casted = dynamic_cast<threading_pthread*>(to);
  if (!casted){
    spkt_throw(sprockit::null_error,
        "threading_pthread::swap_context: thread is not a pthread");
  }
#else
  threading_pthread *casted = static_cast<threading_pthread*>(to);
#endif

  // Grab the context lock.
  pthread_debug("locking context switch to start swap to thread");
  pthread_mutex_lock(context_.context_switch_lock);

  casted->context_.started = true;

  // Wake up the to context.
  pthread_debug("cond signal to cause swap context");
  send_signal(&casted->context_);

  // Put the from thread to sleep.
  pthread_debug("wait on cond in swap context");
  wait_signal(&context_);

  pthread_debug("resuming, unlocking context switch");
  pthread_mutex_unlock(context_.context_switch_lock);
}

#endif
}
} // end of namespace sstmac