#include <sstmac/software/process/cpuset_compute_scheduler.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/operating_system.h>

namespace sstmac {
namespace sw {

void
cpuset_compute_scheduler::configure(int ncore, int nsocket)
{
  compute_scheduler::configure(ncore, nsocket);
  //all cores greater than ncore should be removed from bitmask
  for (int i=0; i < ncore; ++i){
    available_cores_ = available_cores_ | (1<<i);
  }
}

int 
cpuset_compute_scheduler::available_core(int ncore, uint64_t cpumask)
{
  for (int i=0; i < ncore; ++i){
    uint64_t mask = cpumask & (1<<i);
    if (mask != 0){
      return i;
    }
  }
  return -1;
}

void
cpuset_compute_scheduler::reserve_core(thread *thr)
{
  uint64_t valid_cores = available_cores_ & thr->cpumask();
  if (valid_cores == 0){
    debug_printf(sprockit::dbg::compute_scheduler,
        "No available cores from set %lu intersect cpumask %lu for thread %ld",
        available_cores_, thr->cpumask(), thr->thread_id());
    //no available cores, hold up
    pending_threads_.push_back(thr);
    os_->block(thr->schedule_key());
    //if I got here, I unblocked because of available cores
    //figure out which core that is exactly
    valid_cores = available_cores_ & thr->cpumask();
  }
  int core_to_use = available_core(ncores_, valid_cores);
  debug_printf(sprockit::dbg::compute_scheduler,
      "Core %d matches from set %lu intersecting cpumask %lu for thread %ld",
      core_to_use, available_cores_, thr->cpumask(), thr->thread_id());
  allocate_core(core_to_use);
  thr->set_active_core(core_to_use);
}

void
cpuset_compute_scheduler::release_core(thread *thr)
{
  debug_printf(sprockit::dbg::compute_scheduler,
      "Releasing core %d for thread %ld",
      thr->active_core(), thr->thread_id()); 
  
  deallocate_core(thr->active_core());
  if (pending_threads_.empty())
    return;
  
  std::list<thread*>::iterator it, tmp, end = pending_threads_.end();
  it = pending_threads_.begin();
  while (it != end){
    tmp = it++;
    thread* thr = *tmp;
    uint64_t valid_cores = available_cores_ & thr->cpumask();    
    if (valid_cores != 0){
      //the newly freed core allows another thread to continue
      pending_threads_.erase(tmp);
      os_->unblock(thr->schedule_key());
      break;
    }
  }
  
}

}
}
