#ifndef JOB_LAUNCHER_H
#define JOB_LAUNCHER_H

#include <sprockit/factories/factory.h>
#include <sprockit/unordered.h>
#include <sstmac/common/event_handler.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/software/launch/node_set.h>
#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/software/launch/app_launch_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>

namespace sstmac {
namespace sw {

/**
 * @brief The job_launcher class performs the combined operations a queue scheduler like PBS or MOAB
 * and a job launcher like SLURM (srun) or ALPS (aprun).
 * The job launcher allocates nodes to each requested MPI job (or other application).
 * Once nodes are allocated, the job_launcher has to assign MPI ranks to each node (mapping or indexing).
 * Each application can request its a specific allocation or indexing.
 * However, it is ultimately the responsibility of the job launcher to decide on the final
 * allocation/indexing. In most cases, the job_launcher will honor exactly each applications's request
 * unless there is a conflict - in which case the job_launcher must arbitrate conflicting requests.
 */
class job_launcher
{
 public:
  node_id
  node_for_task(app_id aid, task_id tid) const;

  app_launch*
  task_mapper(app_id aid) const;

  /**
   * @brief handle_new_launch_request As if a new job had been submitted with qsub or salloc.
   * The job_launcher receives a new request to launch an application, at which point
   * it can choose to launch the application immediately if node allocation succeeds.
   * @param appman  An object specifying all the details (indexing, allocation, application type)
   *                of the application being launched
   */
  virtual void
  handle_new_launch_request(app_launch* appman, hw::node* target) = 0;

  static job_launcher*
  static_job_launcher(sprockit::sim_parameters* params, event_manager* mgr);

  static int
  launch_root() {
    return launch_root_;
  }

  static void
  clear_static_job_launcher(){
    if (static_launcher_) delete static_launcher_;
    static_launcher_ = nullptr;
  }

  device_id
  event_location() const {
    return device_id();
  }

  static app_launch*
  app_launcher(int aid) {
    return static_launcher_->apps_launched_[aid];
  }

  bool
  app_done(app_id aid){
    apps_finished_.insert(aid);
    if (apps_launched_.find(aid) == apps_launched_.end()){
      spkt_abort_printf("trying to finish invalid app id %d", aid);
    }
    return apps_launched_.size() == apps_finished_.size();
  }

  static bool
  static_app_done(app_id aid){
    return static_launcher_->app_done(aid);
  }


  virtual ~job_launcher(){}

 protected:
  job_launcher(sprockit::sim_parameters* params, event_manager* mgr);

  void
  satisfy_launch_request(app_launch* appman, hw::node* nd);

 protected:
  hw::interconnect* interconnect_;
  ordered_node_set allocated_;
  ordered_node_set available_;

  std::map<app_id, app_launch*> apps_launched_;
  std::set<app_id> apps_finished_;

  static job_launcher* static_launcher_;
  static int launch_root_;

};

DeclareFactory(job_launcher, event_manager*);

class default_job_launcher :
  public job_launcher
{
 public:
  default_job_launcher(sprockit::sim_parameters* params, event_manager* mgr) :
    job_launcher(params, mgr)
  {
  }

 protected:
  void handle_new_launch_request(app_launch* appman, hw::node* nd);

};

}
}



#endif // JOB_LAUNCHER_H

