#ifndef JOB_LAUNCH_EVENT_H
#define JOB_LAUNCH_EVENT_H

#include <sstmac/common/sst_event.h>
#include <sstmac/software/launch/launch_request_fwd.h>

namespace sstmac {
namespace sw {

class job_launch_event :
  public event
{
  NotSerializable(job_launch_event)

 public:
  job_launch_event(app_launch_request* appman) :
    appman_(appman)
  {
  }

  app_launch_request*
  appman() const {
    return appman_;
  }

 private:
  int appnum_;
  app_launch_request* appman_;
};

}
}

#endif // JOB_LAUNCH_EVENT_H

