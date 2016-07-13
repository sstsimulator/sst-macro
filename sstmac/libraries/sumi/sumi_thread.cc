#include <sstmac/libraries/sumi/sumi_thread.h>
#include <sstmac/software/launch/app_launch.h>
#include <sstmac/software/process/operating_system.h>

namespace sstmac {

uint64_t sumi_thread::num_threads_ = 0;

sumi_thread::sumi_thread()
{
  std::string libcomp_name = sprockit::printf("sumi_thread_compute_%lu",
                                       num_threads_);
  compute_ = sstmac::sw::lib_compute_time::construct(libcomp_name);
  ++num_threads_;
}

void
sumi_thread::start()
{
  register_lib(compute_);
  sstmac::sw::operating_system::current_thread()->spawn(this);
}

void
sumi_thread::compute(double sec)
{
  compute_->compute(timestamp(sec));
}

}
