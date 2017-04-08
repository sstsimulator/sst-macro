/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/software/launch/app_launcher.h>
#include <sstmac/software/launch/launch_event.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/app.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <unistd.h>
#include <getopt.h>

namespace sstmac {
namespace sw {

app_launcher::app_launcher(operating_system* os) :
  is_completed_(false),
  service(std::string("launcher"), software_id(0,0), os)
{
}

app_launcher::~app_launcher() throw()
{
}

void
app_launcher::incoming_event(event* ev)
{
  start_app_event* lev = safe_cast(start_app_event, ev);
  if (lev->type() == launch_event::Start){
    task_mapping::add_global_mapping(lev->aid(), lev->unique_name(), lev->mapping());
    software_id sid(lev->aid(), lev->tid());
    app* theapp = app_factory::get_param("name", lev->app_params(), sid, os_);
    theapp->set_unique_name(lev->unique_name());
    int intranode_rank = num_apps_launched_[lev->aid()]++;
    int core_affinity = lev->core_affinity(intranode_rank);
    theapp->set_affinity(core_affinity);
    os_->increment_app_refcount();
    os_->start_app(theapp, lev->unique_name());
  }
  delete lev;
}

void
app_launcher::start()
{
  service::start();
  if (!os_) {
    spkt_throw_printf(sprockit::value_error,
                     "instantlaunch::start: OS hasn't been registered yet");
  }
}

int
start_app_event::core_affinity(int intranode_rank) const
{
  return thread::no_core_affinity;
}


}
}

