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
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/app.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <unistd.h>
#include <getopt.h>

namespace sstmac {
namespace sw {

launcher::launcher() :
  is_completed_(false),
  service(std::string("launcher"), software_id(0,0))
{
}

launcher::~launcher() throw()
{
}

void
launcher::incoming_event(event* ev)
{
  launch_event* lev = safe_cast(launch_event, ev);

  software_id sid(lev->aid(), lev->tid());
  app* theapp = lev->app_template()->clone(sid);
  sprockit::sim_parameters* app_params = lev->app_template()->params();
  theapp->init_perf_model_params(app_params);
  theapp->consume_params(app_params);
  int intranode_rank = num_apps_launched_[lev->aid()]++;
  int core_affinity = lev->core_affinity(intranode_rank);
  theapp->set_affinity(core_affinity);
  os_->start_app(theapp);

  delete lev;
}

void
launcher::start()
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
  if (core_affinities_.size() > 0) { //we are assigned
    return core_affinities_[intranode_rank];
  }
  else {
    return thread::no_core_affinity;
  }
}


}
}

