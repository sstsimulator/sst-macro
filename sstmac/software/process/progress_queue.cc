#include <sstmac/software/process/progress_queue.h>
#include <sstmac/software/process/operating_system.h>

namespace sstmac {
namespace sw {

void
progress_queue::block(std::queue<thread*>& q, double timeout){
  q.push(os->active_thread());
  if (timeout > 0){
    os->block_timeout(timeout);
  } else {
    os->block();
  }
}

void
progress_queue::unblock(std::queue<thread*>& q){
#if SSTMAC_SANITY_CHECK
  if (q.empty()){
    spkt_abort_printf("trying to unblock CQ, but there are no pending threads");
  }
#endif
  auto* thr = q.front();
  q.pop();
  os->unblock(thr);
}

}
}
