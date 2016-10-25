#include <sstmac/libraries/sumi/sumi_thread.h>
#include <sstmac/software/launch/app_launch.h>
#include <sstmac/software/process/operating_system.h>

namespace sstmac {

uint64_t sumi_thread::num_threads_ = 0;

sumi_thread::sumi_thread(sprockit::sim_parameters* params, sw::software_id sid,
                         sw::operating_system* os) :
  thread(params, sid, os)
{
}

void
sumi_thread::start()
{
  sstmac::sw::operating_system::current_thread()->spawn(this);
}

void
sumi_thread::compute(double sec)
{
  parent_app_->compute(timestamp(sec));
}

}
