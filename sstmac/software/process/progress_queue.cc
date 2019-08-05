#include <sstmac/software/process/progress_queue.h>
#include <sstmac/software/process/operating_system.h>

namespace sstmac {
namespace sw {

void
ProgressQueue::block(std::list<Thread*>& q, double timeout){
  Thread* thr = os->activeThread();
  q.push_back(thr);
  if (timeout > 0){
    os->blockTimeout(TimeDelta(timeout));
  } else {
    os->block();
  }
  q.remove(thr);
}

void
ProgressQueue::unblock(std::list<Thread*>& q){
#if SSTMAC_SANITY_CHECK
  if (q.empty()){
    spkt_abort_printf("trying to unblock CQ, but there are no pending threads");
  }
#endif
  auto* thr = q.front();
  //don't erase here - this gets erased in the block call
  os->unblock(thr);
}

}
}
