#include <sstmac/libraries/sumi/sumi_thread.h>
#include <sstmac/software/launch/app_launch.h>
#include <sstmac/software/process/operating_system.h>

namespace sstmac {

uint64_t sumi_thread::num_threads_ = 0;

sumi_thread::sumi_thread(sw::software_id sid)
{
  std::string libname = sprockit::printf("sumi_compute_thread%lu", num_threads_);
  compute_ = new sstmac::sw::lib_compute_time(libname, sid);
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
