#include <sstmac/software/process/thread_info.h>
#include <sstmac/software/process/tls.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/common/sstmac_config.h>
#include <sprockit/errors.h>
#include <iostream>
#include <string.h>
#include <stdint.h>
#include <list>

namespace sstmac {

static const int tls_sanity_check = 42042042;

void
thread_info::register_user_space_virtual_thread(int phys_thread_id, void *stack, void* globalsMap)
{
  size_t stack_mod = ((size_t)stack) % sstmac_global_stacksize;
  if (stack_mod != 0){
    spkt_throw_printf(sprockit::value_error,
        "user space thread stack is not aligned on %llu bytes",
        sstmac_global_stacksize);
  }

  //essentially treat this as thread-local storage
  char* tls = (char*) stack;
  int* thr_id_ptr = (int*) &tls[TLS_THREAD_ID];
  *thr_id_ptr = phys_thread_id;

  int* sanity_ptr = (int*) &tls[TLS_SANITY_CHECK];
  *sanity_ptr = tls_sanity_check;

  //this is dirty - so dirty, but it works
  void** globalPtr = (void**) &tls[TLS_GLOBAL_MAP];
  *globalPtr = globalsMap;
}



}
