#include <sstmac/software/process/simple_compute_scheduler.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/common/sstmac_config.h>

namespace sstmac {
namespace sw {

void
simple_compute_scheduler::reserve_core(thread* thr)
{
#if SSTMAC_SANITY_CHECK
  if (ncore_active_ > ncores_){
    spkt_throw_printf(
      sprockit::value_error,
      "simple_compute_scheduler::reserve_core: %d cores active, only %d cores total",
      ncore_active_, ncores_);
  }
#endif
  if (ncore_active_ >= ncores_){
    pending_threads_.push_back(thr);
    os_->block(thr->schedule_key());
  }
  //If I got here, there are either open cores or I unblocked
  ++ncore_active_;
}

void
simple_compute_scheduler::release_core(thread* thr)
{
  --ncore_active_;
  if (!pending_threads_.empty()){
    thread* next = pending_threads_.front();
    pending_threads_.pop_front();
    os_->unblock(next->schedule_key());
  }
}

}
}
