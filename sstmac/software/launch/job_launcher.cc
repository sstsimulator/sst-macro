#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/launch/launch_event.h>
#include <sstmac/software/launch/job_launch_event.h>
#include <sstmac/software/launch/app_launch.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/thread_lock.h>
#include <sprockit/util.h>

ImplementFactory(sstmac::sw::job_launcher)

namespace sstmac {
namespace sw {

SpktRegister("default", job_launcher, default_job_launcher);

job_launcher* job_launcher::static_launcher_ = nullptr;

job_launcher*
job_launcher::static_job_launcher(sprockit::sim_parameters* params, event_manager* mgr)
{
  if (!static_launcher_){
    static_launcher_ = job_launcher_factory::get_optional_param("job_launcher", "default",
                                                                params, mgr);
    runtime::set_job_launcher(static_launcher_);
  }
  return static_launcher_;
}

job_launcher::job_launcher(sprockit::sim_parameters* params, event_manager* mgr)
{
  interconnect_ = sstmac::hw::interconnect::static_interconnect(params, mgr);
  int num_nodes = interconnect_->num_nodes();
  for (int i=0; i < num_nodes; ++i){
    available_.insert(i);
  }
}

app_launch*
job_launcher::task_mapper(app_id aid) const
{
  auto iter = apps_launched_.find(aid);
  if (iter == apps_launched_.end()){
    spkt_throw_printf(sprockit::value_error,
                      "cannot find application launched with id %d",
                      int(aid));
  }
  return iter->second;
}

node_id
job_launcher::node_for_task(app_id aid, task_id tid) const
{
  auto iter = apps_launched_.find(aid);
  if (iter == apps_launched_.end()){
    spkt_throw(sprockit::value_error,
               "cannot find launched application %d",
               int(aid));
  }
  app_launch* appman = iter->second;
  return appman->node_assignment(int(tid));
}

void
job_launcher::satisfy_launch_request(app_launch* appman, hw::node* nd)
{
  apps_launched_[appman->aid()] = appman;


  const std::list<int>& my_ranks = appman->rank_assignment(nd->addr());
  std::list<int>::const_iterator it, end = my_ranks.end();
  for (it=my_ranks.begin(); it != end; ++it){
    int rank = *it;
    sw::launch_event* lev = new launch_event(appman->app_template(), appman->aid(),
                                    rank, appman->core_affinities());
    nd->handle(lev);
  }
}

void
default_job_launcher::handle_new_launch_request(app_launch* appman, hw::node* nd)
{
  static thread_lock lock;
  lock.lock();
  if (!appman->is_indexed()){
    ordered_node_set allocation;
    appman->request_allocation(available_, allocation);
    for (const node_id& nid : allocation){
      if (available_.find(nid) == available_.end()){
        spkt_throw_printf(sprockit::value_error,
                          "allocation requested node %d, but it's not available",
                          int(nid));
      }
      available_.erase(nid);
    }
    appman->index_allocation(allocation);
  }
  lock.unlock();
  satisfy_launch_request(appman, nd);
}

}
}
