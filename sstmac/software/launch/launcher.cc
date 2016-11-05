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

#include <sstmac/software/launch/launcher.h>
#include <sstmac/software/launch/launch_event.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/software/launch/app_launch.h>
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
  launch_event* lev = safe_cast(launch_event, ev);
  app_launch* launch = job_launcher::app_launcher(lev->aid());
  if (lev->type() == launch_event::Start){
    software_id sid(lev->aid(), lev->tid());
    app* theapp = app_factory::get_param("name", launch->app_params(),
                                         sid, os_);
    int intranode_rank = num_apps_launched_[lev->aid()]++;
    int core_affinity = lev->core_affinity(intranode_rank);
    theapp->set_affinity(core_affinity);
    os_->increment_app_refcount();
    os_->start_app(theapp);
  } else {
    bool app_done = launch->proc_done();
    if (app_done){
      bool sim_done = job_launcher::static_app_done(lev->aid());
      if (sim_done){
        os_->decrement_app_refcount();
      }
    }
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
launch_event::core_affinity(int intranode_rank) const
{
  return thread::no_core_affinity;
}


}
}

