#include <sstmac/software/threading/threading_ucontext.h>
#include <sstmac/common/thread_info.h>

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
   (*func)(void*), void *args, threading_interface *yield_to)
{
  thread_info::register_user_space_virtual_thread(physical_thread_id, stack, stacksize);

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

