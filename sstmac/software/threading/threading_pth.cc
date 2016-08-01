#include <sstmac/software/threading/threading_pth.h>
#include <sstmac/common/thread_info.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

void
threading_pth::init_context() {
  if (pth_uctx_create(&context_) != TRUE) {
    spkt_throw_printf(sprockit::os_error,
        "threading_pth::init_context: %s",
        ::strerror(errno));
  }
}

void
threading_pth::destroy_context() {
  if (pth_uctx_destroy(context_) != TRUE) {
      spkt_throw_printf(sprockit::os_error,
        "threading_pth::destroy_context: %s",
        ::strerror(errno));
  }
}

  /// Start a new context.
void
threading_pth::start_context(int physical_thread_id, void *stack, size_t stacksize, void
                (*func)(void*), void *args, threading_interface *yield_to) {
  if (stacksize < (16384)) {
    spkt_throw(sprockit::value_error,
        "threading_pth::start_context: PTH does not accept stacks smaller than 16KB");
  }
  thread_info::register_user_space_virtual_thread(physical_thread_id, stack, stacksize);
  init_context();
  threading_pth* yield_pth = (threading_pth*)yield_to;
  int retval = pth_uctx_make(context_, (char*) stack, stacksize, NULL, func,
                             args, (yield_to ? yield_pth->context_ : NULL));
  if (retval != TRUE) {
    spkt_throw_printf(sprockit::os_error,
        "threading_pth::start_context: %s",
        ::strerror(errno));
  }
}

  /// Swap context.
void
threading_pth::swap_context(threading_interface *to) {
  threading_pth* topth = (threading_pth*)to;
#ifdef SSTMAC_HAVE_PTH_UCTX_SWITCH_IGNORE_SIGMASK
  if (pth_uctx_switch_ignore_sigmask(context_, topth->context_) != TRUE) {
#else
  if (pth_uctx_switch(context_, topth->context_) != TRUE) {
#endif
    spkt_throw_printf(sprockit::os_error,
      "threading_pth::swap_context: %s",
      strerror(errno));
  }
}

} }

