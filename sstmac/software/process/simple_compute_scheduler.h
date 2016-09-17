#ifndef sstmac_software_process_simple_COMPUTE_scheduleR_H
#define sstmac_software_process_simple_COMPUTE_scheduleR_H

#include <sstmac/software/process/compute_scheduler.h>

namespace sstmac {
namespace sw {

class simple_compute_scheduler : public compute_scheduler
{
 public:
  simple_compute_scheduler(sprockit::sim_parameters*params,
                           operating_system* os)
    : ncore_active_(0), compute_scheduler(params, os)
  {}
  
  void reserve_core(thread* thr);
  
  void release_core(thread* thr);

 private:
  std::list<thread*> pending_threads_;
  int ncore_active_;
};

}
}

#endif // BASIC_COMPUTE_scheduleR_H
