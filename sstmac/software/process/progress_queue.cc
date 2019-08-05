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

void
PollingQueue::block()
{
  int max_empty_polls = 2;
  Timestamp now = os->now();
  if (now == last_check_){
    ++num_empty_calls_;
  } else {
    num_empty_calls_ = 0;
    last_check_ = now;
  }
  if (num_empty_calls_ >= max_empty_polls){
    ProgressQueue::block(pending_threads_, -1);
  }
}

void
PollingQueue::unblock()
{
  num_empty_calls_ = 0;
  ProgressQueue::unblock(pending_threads_);
}

}
}
