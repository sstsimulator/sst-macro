#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/launch/launch_event.h>
#include <sstmac/software/launch/job_launch_event.h>
#include <sstmac/software/process/app_manager.h>
#include <sstmac/common/runtime.h>
#include <sprockit/util.h>

ImplementFactory(sstmac::sw::job_launcher)

namespace sstmac {
namespace sw {

SpktRegister("default", job_launcher, default_job_launcher);

void
job_launcher::handle(event *ev)
{
  job_launch_event* lev = safe_cast(job_launch_event, ev);
  handle_new_launch(lev->appnum(), lev->appman());
}

void
default_job_launcher::handle_new_launch(int appnum, app_manager* appman)
{
  appman->allocate_and_index_jobs();
  launch_info* linfo = appman->launch_info();
  sstmac::sw::app_id aid(appnum);
  for (int i=0; i < appman->nproc(); ++i) {
    node_id dst_nid = appman->node_assignment(i);
    runtime::register_node(aid, task_id(i), dst_nid);

    hw::node* dst_node = interconnect_->node_at(dst_nid);
    if (!dst_node) {
      // mpiparallel, this node belongs to someone else
      continue;
    }

    sw::launch_event* lmsg = new launch_event(linfo, sw::launch_event::ARRIVE, task_id(i));
    dst_node->handle(lmsg);
  }
}

}
}
