#ifndef JOB_LAUNCH_EVENT_H
#define JOB_LAUNCH_EVENT_H

#include <sstmac/common/sst_event.h>
#include <sstmac/software/launch/launch_info_fwd.h>
#include <sstmac/software/process/app_manager_fwd.h>

namespace sstmac {
namespace sw {

class job_launch_event :
  public event
{
  NotSerializable(job_launch_event)

 public:
  job_launch_event(int appnum, app_manager* appman) :
    appnum_(appnum), appman_(appman)
  {
  }

  int appnum() const {
    return appnum_;
  }

  app_manager*
  appman() const {
    return appman_;
  }

  std::string
  to_string() const {
    return "job launch event";
  }

 private:
  int appnum_;
  app_manager* appman_;
};

}
}

#endif // JOB_LAUNCH_EVENT_H

