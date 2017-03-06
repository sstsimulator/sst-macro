//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program.  If not, see http://www.gnu.org/licenses/.
//

#ifndef SSTMAC_SOFTWARE_THREADING_THREADING_PTHREAD_H_INCLUDED
#define SSTMAC_SOFTWARE_THREADING_THREADING_PTHREAD_H_INCLUDED

#include <sstmac/software/threading/threading_interface.h>
#include <vector>

#ifdef SSTMAC_HAVE_PTHREAD
#include <pthread.h>
#endif

namespace sstmac {
namespace sw {

#ifdef SSTMAC_HAVE_PTHREAD


struct threadcontext_t
{
  pthread_t thread;
  pthread_cond_t ready;
  pthread_mutex_t* context_switch_lock;
  bool started;
  bool waiting;
};

class threading_pthread : public threading_interface
{
 private:
  threadcontext_t context_;
  int thread_id_;
  int nthread_;

 public:
  threading_pthread(int thread_id, int nthread);

  /// This part of context initialization is common to
  /// init_context and start_context.
  void
  init_context_common(threadcontext_t &t);

  /// Initialize the context to be that for the currently running thread.
  void
  init_context();

  /// This tears down the context. It is only called from the scheduler's thread.
  void
  destroy_context();

  class threadargs
  {
   public:
    void
    (*func)(void*);
    void *args;
    threadcontext_t *context;
    threadargs(void
               (*f)(void*), void *a, threadcontext_t *c) {
      func = f;
      args = a;
      context = c;
    }
  };

  static std::vector<pthread_mutex_t> context_switch_mutexes;

  virtual
  threading_interface* copy() {
    return new threading_pthread(thread_id_, nthread_);
  }

  /// Start a new context. It does not start yet -- swap_context will start it. This is only called from the scheduler's thread.
  void
  start_context(int physical_thread_id, void *stack, size_t stacksize, void
                 (*func)(void*), void *args, threading_interface *yield_to,
                void* globals_storage);

  /// This is called when we have completed running the thread. It is
  /// called in the from context.
  void
  complete_context(threading_interface *to);

  /// Swap context. The from context is always the currently running context.
  void
  swap_context(threading_interface *to);
};

#endif
}
} // end of namespace sstmac
#endif

