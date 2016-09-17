#ifndef CPUSET_COMPUTE_scheduleR_H
#define CPUSET_COMPUTE_scheduleR_H

#include <sstmac/software/process/compute_scheduler.h>

namespace sstmac {
namespace sw {

class cpuset_compute_scheduler : public compute_scheduler
{
 public:  
  cpuset_compute_scheduler(sprockit::sim_parameters* params,
                           operating_system* os) :
    available_cores_(0),
    compute_scheduler(params, os)
  {
  }

  void configure(int ncore, int nsocket);
  
  void reserve_core(thread *thr);
  
  void release_core(thread *thr);
  
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
