#include <sstmac/software/process/progress_queue.h>
#include <sstmac/software/process/operating_system.h>

namespace sstmac {
namespace sw {

void
ProgressQueue::block(std::queue<Thread*>& q, double timeout){
  q.push(os->activeThread());
  if (timeout > 0){
    os->blockTimeout(TimeDelta(timeout));
  } else {
    os->block();
  }
}

void
ProgressQueue::unblock(std::queue<Thread*>& q){
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
