/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sprockit/errors.h>
#include <sstmac/common/thread_info.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/software/threading/threading_pthread.h>

namespace sstmac {
namespace sw {
#ifdef SSTMAC_HAVE_PTHREAD

std::vector<pthread_mutex_t> threading_pthread::context_switch_mutexes;

threading_pthread::threading_pthread(int thread_id, int nthread) :
  thread_id_(thread_id),
  nthread_(nthread)
{
  static thread_lock lock;
  lock.lock();
  if (context_switch_mutexes.size() == 0){
    //not yet done
    context_switch_mutexes.resize(nthread);
    for (int i=0; i < nthread; ++i){
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

  // The following code handles the initial context switch
  // starting this thread. After that, swap_context
  // performs all context switches.

  // Grab the context lock.
  pthread_mutex_lock(thread_info->context->context_switch_lock);

  // Put this thread to sleep, if it has not
  // already been started.
  if (!thread_info->context->started) {
    pthread_cond_wait(&thread_info->context->ready,
                      thread_info->context->context_switch_lock);
  }

  pthread_mutex_unlock(thread_info->context->context_switch_lock);

  //delete the thread info here
  //Once we enter this function, we shall never return, dear Frodo
  delete thread_info;

  // Run the thread
  (*func)(thread_args);

  spkt_throw_printf(sprockit::illformed_error,
                   "pthreadfunc arrived at end of function. complete_context should have terminated this with pthread_exit");

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
   (*func)(void*), void *args, threading_interface *yield_to)
{
  pthread_attr_t thread_attr;
  pthread_attr_init(&thread_attr);
  //pthread_attr_setstack(&thread_attr, stack, stacksize);
  init_context_common(context_);

  //thread_info::register_kernel_space_virtual_thread(physical_thread_id, &context_.thread, &thread_attr);

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
    pthread_cond_signal(&casted->context_.ready);
  }
  else {
    spkt_throw_printf(sprockit::illformed_error,
                     "received non-pthread context on complete_context");
    //pthread_cond_signal(NULL);
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

  // Grab the context lock.
  pthread_mutex_lock(context_.context_switch_lock);

#if SSTMAC_SANITY_CHECK
  threading_pthread *casted = dynamic_cast<threading_pthread*>(to);
  if (!casted){
    spkt_throw(sprockit::null_error,
        "threading_pthread::swap_context: thread is not a pthread");
  }
#else
  threading_pthread *casted = static_cast<threading_pthread*>(to);
#endif

  casted->context_.started = true;

  // Wake up the to context.
  pthread_cond_signal(&casted->context_.ready);

  // Put the from thread to sleep.
  pthread_cond_wait(&context_.ready, context_.context_switch_lock);

  pthread_mutex_unlock(context_.context_switch_lock);
}

#endif
}
} // end of namespace sstmac

