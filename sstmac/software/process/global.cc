#include <sstmac/software/process/global.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>

namespace sstmac {
namespace sw {

process_context
sstmac_global::current_context() const {
  thread* t = operating_system::current_thread();
  if (t) {
    return t->get_process_context();
  }
  else {
    return process_context(process_context::none);
  }
}

}
}
