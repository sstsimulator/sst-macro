#ifndef JOB_LAUNCHER_H
#define JOB_LAUNCHER_H

#include <sprockit/factories/factory.h>
#include <sstmac/common/event_handler.h>
#include <sstmac/software/launch/launch_info.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>


namespace sstmac {
namespace sw {

class job_launcher :
  public sprockit::factory_type,
  public event_handler
{
 public:
  void handle(event *ev);

  void
  set_interconnect(hw::interconnect* ic){
    interconnect_ = ic;
  }

 private:
  virtual void
  handle_new_launch(int appnum, app_manager* appman) = 0;

 protected:
  hw::interconnect* interconnect_;

};

DeclareFactory(job_launcher)

class default_job_launcher :
  public job_launcher
{
 public:
  std::string
  to_string() const {
    return "default job launcher";
  }

 private:
  void handle_new_launch(int appnum, app_manager* appman);

};

}
}



#endif // JOB_LAUNCHER_H

