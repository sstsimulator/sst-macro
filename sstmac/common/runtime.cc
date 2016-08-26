#include <sstmac/common/runtime.h>
#include <sprockit/statics.h>
#include <sprockit/delete.h>
#include <sstmac/software/launch/app_launch.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/launch/job_launcher.h>

namespace sstmac {

bool runtime::do_deadlock_check_ = false;
std::list<deadlock_check*> runtime::deadlock_checks_;
sw::job_launcher* runtime::launcher_ = nullptr;
hw::topology* runtime::topology_ = nullptr;

void
runtime::check_deadlock()
{
  if (!do_deadlock_check_) return;

  std::list<deadlock_check*>::iterator it, end = deadlock_checks_.end();
  for (it=deadlock_checks_.begin(); it != end; ++it){
    deadlock_check* check = *it;
    check->run();
  }
}

node_id
runtime::current_node()
{
  return sw::operating_system::current_node_id();
}

void
runtime::delete_statics()
{
  //not owned by runtime - do not delete
  //if (topology_) delete topology_;
  //if (launcher_) delete launcher_;
  for (deadlock_check* chk: deadlock_checks_){
    delete chk;
  }
  deadlock_checks_.clear();
}

node_id
runtime::node_for_task(sw::app_id aid, sw::task_id tid)
{
  return launcher_->node_for_task(aid, tid);
}

void
runtime::finish()
{
}

}

