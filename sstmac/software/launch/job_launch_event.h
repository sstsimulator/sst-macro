#ifndef JOB_LAUNCH_EVENT_H
#define JOB_LAUNCH_EVENT_H

#include <sstmac/common/sst_event.h>
#include <sstmac/software/launch/app_launch_fwd.h>

namespace sstmac {
namespace sw {

class job_launch_event :
  public event
{
  NotSerializable(job_launch_event)

 public:
  job_launch_event(app_launch* appman) :
    appman_(appman)
  {
  }

  app_launch*
  appman() const {
    return appman_;
  }

  std::string
  to_string() const {
    return "job launch event";
  }

 private:
  int appnum_;
  app_launch* appman_;
};

}
}

#endif // JOB_LAUNCH_EVENT_H

