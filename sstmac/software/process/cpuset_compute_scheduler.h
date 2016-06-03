#ifndef CPUSET_COMPUTE_scheduleR_H
#define CPUSET_COMPUTE_scheduleR_H

#include <sstmac/software/process/compute_scheduler.h>

namespace sstmac {
namespace sw {

class cpuset_compute_scheduler : public compute_scheduler
{
 public:  
  cpuset_compute_scheduler() : available_cores_(0)
  {
  }

  void configure(int ncore, int nsocket);
  
  void reserve_core(thread *thr);
  
  void release_core(thread *thr);
  
  compute_scheduler*
  clone(operating_system *os) const {
    cpuset_compute_scheduler* cln = new cpuset_compute_scheduler;
    cln->os_ = os;
    clone_into(cln);
    return cln;
  }
 
 protected:
  void
  clone_into(cpuset_compute_scheduler *cln) const {
    cln->available_cores_ = available_cores_;
    compute_scheduler::clone_into(cln);
  }
  
 private:
  static int available_core(int ncore, uint64_t cpumask);
  
  void allocate_core(int core){
    available_cores_ = available_cores_ & ~(1<<core);
  }
  
  void deallocate_core(int core){
    available_cores_ = available_cores_ | (1<<core);
  }
  
 private:
  uint64_t available_cores_;
  std::list<thread*> pending_threads_;


};

}
}

#endif // CPUSET_COMPUTE_scheduleR_H
